//////////////////////////////////////////////////
//
// G. Del Castillo, W. Hardt
// Fast dynamic analysis of instruction sets
// (CODES/CASHE'98, GI/ITG/GMM)
//
//   simulator.asm
//

transition ZCPU ==
  block
    MATH_RULE
    IOC_RULE
    INCR_PMA

    INSTR_COUNT
  end