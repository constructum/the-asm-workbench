//////////////////////////////////////////////////
//
// G. Del Castillo, W. Hardt
// Fast dynamic analysis of instruction sets
// (CODES/CASHE'98, GI/ITG/GMM)
//
//   ioc_instructions.asm
//

//------------------------------------------------
// semantics of I/O instructions and branches
//

transition DO_LD (rd, ra, disp) ==
  reg (rd) := mem (reg (ra) + disp_addr (disp))

transition DO_LDA (rd, ra, disp) ==
  reg (rd) := reg (ra) + disp_addr (disp)

transition DO_LDPA (rd, ra, disp) ==
  reg (rd) := PMA + reg (ra) + disp_addr (disp)

transition DO_ST (rd, ra, disp) ==
  mem (reg(ra) + disp_addr (disp)) := reg (rd)



static function eval_cond (cond, N, Z) ==
  case cond of
    TRUE  : true ;
    FALSE : false ;
    EQ	  : Z = true ;
    NE	  : Z = false ;
    LE	  : N = true or Z = true ; 
    LT	  : N = true and Z = false ;
    GE	  : N = false or Z = true ;
    GT	  : N = false and Z = false
  end

transition DO_JUMP (ra, disp, cond) ==
  if eval_cond (cond, Neg, Zero)
  then PMA := reg (ra) + disp_addr (disp)
  else PMA := PMA + 1
  end

transition DO_SKIP (ra, disp, cond) ==
  if eval_cond (cond, Neg, Zero)
  then PMA := PMA + reg (ra) + disp_addr (disp)
  else PMA := PMA + 1
  end



transition IOC_RULE ==
  case instr (PMA) of
    LD (rd, ra, disp)   : DO_LD (rd, ra, disp) ;
    LDA (rd, ra, disp)	: DO_LDA (rd, ra, disp) ;
    LDPA (rd, ra, disp) : DO_LDPA (rd, ra, disp) ;
    ST (rd, ra, disp)	: DO_ST (rd, ra, disp) ;

    JUMP (ra, disp, cond) : DO_JUMP (ra, disp, cond) ;
    SKIP (ra, disp, cond) : DO_SKIP (ra, disp, cond)
  end


transition INCR_PMA ==
  if not (is_jump_instruction (instr (PMA))) and (instr (PMA) != HALT)
  then PMA := PMA + 1
  end
