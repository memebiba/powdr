use num_bigint::{BigInt, BigUint};
use polyexen::expr::{ColumnKind, Expr, PlonkVar};
use polyexen::plaf::backends::halo2::PlafH2Circuit;
use polyexen::plaf::{ColumnFixed, ColumnWitness, Columns, Info, Lookup, Plaf, Poly, Witness};

use crate::analyzer::{BinaryOperator, Expression, IdentityKind};
use crate::number::AbstractNumberType;
use crate::{analyzer, constant_evaluator, witness_generator};
use num_traits::{One, Zero};

use super::circuit_data::CircuitData;

pub(crate) fn analyzed_to_circuit(
    analyzed: &analyzer::Analyzed,
    query_callback: Option<impl FnMut(&str) -> Option<AbstractNumberType>>,
    field_mod: BigUint,
    verbose: bool,
    int_to_field: &dyn Fn(&BigInt) -> BigUint,
) -> PlafH2Circuit {
    // The structure of the table is as following
    //
    // | constant columns | __enable_cur | __enable_next |  witness columns | \
    // |  bla_bla_bla     |    1         |       1       |   bla_bla_bla    |  |
    // |  bla_bla_bla     |    1         |       1       |   bla_bla_bla    |  |>  witness + fixed  2^(k-1)
    // |  ...             |   ...        |      ...      |   ...            |  |
    // |  bla_bla_bla     |    1         |       0       |   bla_bla_bla    | /     <- __enable_next == 0 since there's no state transition
    // |  0               |    0         |       0       |   0              | \
    // |  0               |    0         |       0       |   0              |  |
    // |  ...             |   ...        |      ...      |   ...            |  |> 2^2-1
    // |  0               |    0         |       0       |   <unusable>     |  |
    // |  0               |    0         |       0       |   <unusable>     | /

    // generate fixed and witness (witness).

    let query = |column, rotation| Expr::Var(PlonkVar::ColumnQuery { column, rotation });

    let (fixed, degree) = constant_evaluator::generate(analyzed);
    let witness = witness_generator::generate(analyzed, degree, &fixed, query_callback, verbose);

    let mut cd = CircuitData::from(fixed, witness);

    // append to fixed columns:
    // - one that enables constraints that do not have rotations
    // - and another that enables constraints that have a rotation
    // (note this is not activated) in last row.

    let num_rows = cd.len();

    let q_enable_cur = query(
        cd.insert_constant("__enable_cur", itertools::repeat_n(BigInt::one(), num_rows)),
        0,
    );

    let q_enable_next = query(
        cd.insert_constant(
            "__enable_next",
            itertools::repeat_n(BigInt::one(), num_rows - 1).chain(std::iter::once(BigInt::zero())),
        ),
        0,
    );

    let mut lookups = vec![];
    let mut polys = vec![];

    // build Plaf columns -------------------------------------------------

    let columns = Columns {
        fixed: cd
            .fixed
            .iter()
            .map(|(name, _)| ColumnFixed::new(name.to_string()))
            .collect(),
        witness: cd
            .witness
            .iter()
            .map(|(name, _)| ColumnWitness::new(name.to_string(), 0))
            .collect(),
        public: vec![],
    };

    // build Plaf info. -------------------------------------------------------------------------

    let info = Info {
        p: field_mod,
        num_rows: cd.len(),
        challenges: vec![],
    };

    // build Plaf polys. -------------------------------------------------------------------------

    for id in &analyzed.identities {
        if id.kind == IdentityKind::Polynomial {
            // polynomial identities.

            assert_eq!(id.right.expressions.len(), 0);
            assert_eq!(id.right.selector, None);
            assert_eq!(id.left.expressions.len(), 0);

            let (exp, rotation) =
                expression_2_expr(&cd, id.left.selector.as_ref().unwrap(), int_to_field);

            // depending whether this polynomial contains a rotation,
            // enable for all rows or all except the last one.

            let exp = if rotation == 0 {
                Expr::Mul(vec![exp, q_enable_cur.clone()])
            } else {
                Expr::Mul(vec![exp, q_enable_next.clone()])
            };
            polys.push(Poly {
                name: "".to_string(),
                exp,
            });
        } else if id.kind == IdentityKind::Plookup {
            // lookups.

            assert_eq!(id.right.selector, None);

            let left_selector = id
                .left
                .selector
                .clone()
                .map_or(Expr::Const(BigUint::one()), |expr| {
                    expression_2_expr(&cd, &expr, int_to_field).0
                });

            let left = id
                .left
                .expressions
                .iter()
                .map(|expr| left_selector.clone() * expression_2_expr(&cd, expr, int_to_field).0)
                .collect();

            let right = id
                .right
                .expressions
                .iter()
                .map(|expr| expression_2_expr(&cd, expr, int_to_field).0)
                .collect();

            lookups.push(Lookup {
                name: "".to_string(),
                exps: (left, right),
            });
        } else {
            unimplemented!()
        }
    }

    // build Plaf fixed. -------------------------------------------------------------------------

    let fixed: Vec<Vec<_>> = cd
        .fixed
        .iter()
        .map(|(_, row)| row.iter().map(|value| Some(int_to_field(value))).collect())
        .collect();

    // build witness. -------------------------------------------------------------------------

    let witness: Vec<Vec<_>> = cd
        .witness
        .iter()
        .map(|(_, row)| {
            row.iter()
                .map(|value| Some(value.to_biguint().unwrap()))
                .collect()
        })
        .collect();

    let witness_cols = cd
        .witness
        .iter()
        .enumerate()
        .map(|(n, (name, _))| (name.to_string(), (ColumnKind::Fixed, n)));

    let wit = Witness {
        num_rows: cd.witness.len(),
        columns: witness_cols
            .map(|(name, _)| ColumnWitness::new(name, 0))
            .collect(),
        witness,
    };

    let copys = vec![];

    // build plaf. -------------------------------------------------------------------------

    let plaf = Plaf {
        info,
        columns,
        polys,
        lookups,
        copys,
        fixed,
    };

    // return circuit description + witness. -------------

    PlafH2Circuit { plaf, wit }
}

fn expression_2_expr(
    cd: &CircuitData,
    expr: &Expression,
    int_to_field: &dyn Fn(&BigInt) -> BigUint,
) -> (Expr<PlonkVar>, i32) {
    match expr {
        Expression::Number(n) => (Expr::Const(int_to_field(n)), 0),
        Expression::PolynomialReference(polyref) => {
            assert_eq!(polyref.index, None);

            let rotation = if polyref.next { 1 } else { 0 };

            let plonkvar = PlonkVar::ColumnQuery {
                column: cd.col(&polyref.name),
                rotation,
            };

            (Expr::Var(plonkvar), rotation)
        }
        Expression::BinaryOperation(lhe, op, rhe) => {
            let (lhe, lhe_rot) = expression_2_expr(cd, lhe, int_to_field);
            let (rhe, rhe_rot) = expression_2_expr(cd, rhe, int_to_field);
            let res = match op {
                BinaryOperator::Add => Expr::Sum(vec![lhe, rhe]),
                BinaryOperator::Sub => Expr::Sum(vec![lhe, Expr::Neg(Box::new(rhe))]),
                BinaryOperator::Mul => Expr::Mul(vec![lhe, rhe]),
                _ => unimplemented!("{:?}", expr),
            };
            (res, std::cmp::max(lhe_rot, rhe_rot))
        }

        _ => unimplemented!("{:?}", expr),
    }
}
