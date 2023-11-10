machine MiniAvm2 {
    degree 256;

    reg pc[@pc];
    reg Ta[<=];
    reg Tb[<=];
    reg Tc[<=];
    reg X[<=];
    reg Ia;
    reg Ib;
    reg Ic;
    reg addr;

    pol constant STEP(i) { i };

    pol commit m_addr;
    pol commit m_step;
    pol commit m_value;
//    pol commit m_rw; // Enum: 0 (read), 1 (write)
    col witness m_is_write;

// ========== Table SUBOP-TR =================

    // Add
    // Take in two input registers, send the result into the output register
  //  instr add Ta, Tb -> Tc {
  //      Tc = Ta + Tb
  //  }

  //  instr push Ta -> Tb {
  //      Tb = Ta
  //  }
    // instr store X { { addr, STEP, X } is m_is_write { m_addr, m_step, m_value } }

    instr store X {
        { addr, STEP, X } is m_is_write { m_addr, m_step, m_value }
    }

    function main {
        addr <=X= 0;
        store 1;
    }
}