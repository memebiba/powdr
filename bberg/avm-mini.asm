machine MiniAvm {
    degree 256;

    reg pc[@pc];
     reg Ta[<=];
    reg Tb[<=];
    reg Tc[<=];
    reg Ia;
    reg Ib;
    reg Ic;

    reg Imem[<=];

    reg tmp;

// ========== Table SUBOP-TR =================
    pol constant clk(i) { i };
    pol constant positive(i) { i + 1 };
    pol constant first = [1] + [0]*;
    pol constant last(i) { FIRST(i + 1) }; // Can we write it as [0]* + [1] ?

    // Enum over sub operations
    // 0: LOAD
    // 1: STORE
    pol commit subop;

    // Enum expressing intermediate register
    // 0: Ia
    // 1: Ib
    // 2: Ic
    //pol commit reg_idx;
    reg reg_idx[<=];


    // We should range constrain it to 32 bits ultimately. For mini-AVM,
    // we will assume that this column will be of the right type.
    reg mem_idx;

    // Relations on type constraints
    subop * (1 - subop) = 0;
    reg_idx * (1 - reg_idx) * (2 - reg_idx) = 0;

    // Add
    // Take in two input registers, send the result into the output register
    instr add Ta, Tb -> Tc {
        Tc = Ta + Tb
    }

// ========== Table MEM-TR =================
    pol commit m_clk;
    pol commit m_addr;
    pol commit m_val;
    pol commit m_lastAccess; // Boolean (1 when this row is the last of a given address)
    pol commit m_rw; // Enum: 0 (read), 1 (write)

    // The table must be sorted by m_addr and then by m_clk.

    // Type constraints
    m_lastAccess * (1 - m_lastAccess) = 0;
    m_rw * (1 - m_rw) = 0;

    // this is a check if m_addr' - m_addr is 0 or 1 (requires placeholders):
    // (m_addr' - m_addr) * (m_addr' - m_addr) - (m_addr' - m_addr) = 0;
    // if m_addr' - m_addr is 0 or 1, the following is equiv. to m_lastAccess
    // (m_addr' - m_addr)

    // m_lastAccess == 0 ==> m_addr' == m_addr
    (1 - m_lastAccess) * (m_addr' - m_addr) = 0;

    // m_addr' == m_addr ==> m_lastAccess == 0
    // This condition does not apply on the last row.
    { m_lastAccess * (m_addr' - m_addr) } in positive;

    // m_lastAccess == 0 && m_rw' == 0 ==> m_val == m_val'
    // This condition does not apply on the last row.
    // Note: in barretenberg, a shifted polynomial will be 0 on the last row (shift is not cyclic)
    // Note2: in barretenberg, if a poynomial is shifted, its non-shifted equivalent must be 0 on the first row
    (1 - m_lastAccess) * (1 - m_rw') * (m_val' - m_val) = 0;

    // Read

    // Load

    // instr store X { { addr, STEP, X } is m_is_write { m_addr, m_step, m_value } }

    // TODO quality of life feature lol
    // let m_is_read := 1 - m_rw;

    instr load -> Imem {
        { mem_idx, clk, Imem } is (1 - m_rw) { m_addr, m_clk, m_val }
    }

    // Store
    instr store Imem {
        { mem_idx, clk, Imem } is m_rw { m_addr, m_clk, m_val }
    }

    instr pushA Ta {
        Ia = Ta
    }
    instr pushB Tb {
        Ib = Tb
    }
    instr pushC Tc {
        Ic = Tc
    }
    // if reg_idx = 0, Ia = Imem
   (1 - reg_idx) * (2 - reg_idx) * (Imem - Ia) = 0;

    // if reg_idx = 1, Ib = Imem
    reg_idx * (2 - reg_idx) * (Imem - Ib) = 0;

    // if reg_idx = 2, Ic = Imem
    reg_idx * (1 - reg_idx) * (Imem - Ic) = 0;

    function main {
        pushA(1);
        pushB(2);
        Ic <== add(Ia, Ib);
        mem_idx <=Ta= 1;
        store Ic;
       // tmp <== add(3, 6);

        return;
    }
}