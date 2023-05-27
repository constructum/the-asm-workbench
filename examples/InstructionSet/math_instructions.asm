//////////////////////////////////////////////////
//
// G. Del Castillo, W. Hardt
// Fast dynamic analysis of instruction sets
// (CODES/CASHE'98, GI/ITG/GMM)
//
//   math_instructions.asm
//

//------------------------------------------------
// semantics of arithmetical instructions
//


// logical operations

transition LogicalInstr (dest_reg, value) ==
  block
    reg (dest_reg) := value
    Neg      := (value < 0)
    Zero     := (value = 0)
  end

transition DO_OR (dest_reg, src_reg_1, src_reg_2) ==
  LogicalInstr (dest_reg, or_fun (reg (src_reg_1), reg (src_reg_2)))

transition DO_AND (dest_reg, src_reg_1, src_reg_2) ==
  LogicalInstr (dest_reg, and_fun (reg (src_reg_1), reg (src_reg_2)))

transition DO_NOR (dest_reg, src_reg_1, src_reg_2) ==
  LogicalInstr (dest_reg, nor_fun (reg (src_reg_1), reg (src_reg_2)))

transition DO_NAND (dest_reg, src_reg_1, src_reg_2) ==
  LogicalInstr (dest_reg, nand_fun (reg (src_reg_1), reg (src_reg_2)))

transition DO_XOR (dest_reg, src_reg_1, src_reg_2) ==
  LogicalInstr (dest_reg, xor_fun (reg (src_reg_1), reg (src_reg_2)))

transition DO_ZERO (dest_reg) ==
  LogicalInstr (dest_reg, zero_fun)

transition DO_XNOR (dest_reg, src_reg_1, src_reg_2) ==
  LogicalInstr (dest_reg, xnor_fun (reg (src_reg_1), reg (src_reg_2)))

transition DO_FF (dest_reg) ==
  LogicalInstr (dest_reg, ff_fun)


// additive operations

transition AdditiveInstr (dest_reg, value, write_dest_reg) ==
  block
    if write_dest_reg then reg (dest_reg) := value endif   // (note: CMP does not write dest_reg)
    Neg  := (value < 0)
    Zero := (value = 0)
  end

transition DO_ADD (dest_reg, src_reg_1, src_reg_2) ==
  AdditiveInstr ( dest_reg, add_fun (reg (src_reg_1), reg (src_reg_2)), true )

transition DO_SUB (dest_reg, src_reg_1, src_reg_2) ==
  AdditiveInstr ( dest_reg, sub_fun (reg (src_reg_1), reg (src_reg_2)), true )

transition DO_CMP (src_reg_1, src_reg_2) ==
  AdditiveInstr ( undef, sub_fun (reg (src_reg_1), reg (src_reg_2)), false )


// shift operation

transition DO_LSH (dest_reg, src_reg_1, src_reg_2) ==
  let value == lsh_fun (reg (src_reg_1), reg (src_reg_2))
  in reg (dest_reg) := value
     Neg := (value < 0)
  end


// multiplication and division

transition MultiplicativeInstr (dest_reg, value) ==
  block
    reg (dest_reg) := value
    Neg      := (value < 0)
    Zero     := (value = 0)
  end

transition DO_MUL (dest_reg, src_reg_1, src_reg_2) ==
  MultiplicativeInstr (dest_reg, mul_fun (reg (src_reg_1), reg (src_reg_2)))

transition DO_DIVQ (dest_reg, src_reg_1, src_reg_2) ==
block
  MultiplicativeInstr (dest_reg, divq_fun (reg (src_reg_1), reg (src_reg_2)))
  Divz := (reg (src_reg_2) = 0)
end

transition DO_DIVR (dest_reg, src_reg_1, src_reg_2) ==
block
  MultiplicativeInstr (dest_reg, divr_fun (reg (src_reg_1), reg (src_reg_2)))
  Divz := (reg (src_reg_2) = 0)
end


// all the arithmetical & logical instructions

transition MATH_RULE ==
  case instr (PMA) of
    AND (rr, r1, r2)   : DO_AND (rr, r1, r2) ;
    OR (rr, r1, r2)    : DO_OR (rr, r1, r2) ;
    NAND (rr, r1, r2)  : DO_NAND (rr, r1, r2) ;
    NOR (rr, r1, r2)   : DO_NOR (rr, r1, r2) ;
    XOR (rr, r1, r2)   : DO_XOR (rr, r1, r2) ;
    ZERO (rr)	       : DO_ZERO (rr) ;
    XNOR (rr, r1, r2)  : DO_XNOR (rr, r1, r2) ;
    FF (rr)	       : DO_FF (rr) ;

    ADD (rr, r1, r2)   : DO_ADD (rr, r1, r2) ;
    SUB (rr, r1, r2)   : DO_SUB (rr, r1, r2) ;
    CMP (r1, r2)       : DO_CMP (r1, r2) ;

    LSH (rr, r1, r2)   : DO_LSH (rr, r1, r2);

    MUL (rr, r1, r2)   : DO_MUL (rr, r1, r2) ;
    DIVQ (rr, r1, r2)  : DO_DIVQ (rr, r1, r2) ;
    DIVR (rr, r1, r2)  : DO_DIVR (rr, r1, r2)
  end
