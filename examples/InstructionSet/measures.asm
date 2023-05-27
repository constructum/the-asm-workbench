//////////////////////////////////////////////////
//
// G. Del Castillo, W. Hardt
// Fast dynamic analysis of instruction sets
// (CODES/CASHE'98, GI/ITG/GMM)
//
//   measures.asm
//

dynamic function instrs initially 0

dynamic function arithm initially 0

dynamic function logic  initially 0
dynamic function adds	initially 0
dynamic function shifts initially 0
dynamic function mults	initially 0
dynamic function divs	initially 0

dynamic function loads initially 0
dynamic function stores initially 0

dynamic function branches initially 0
dynamic function taken_branches initially 0
dynamic function not_taken_branches initially 0

dynamic function steps_from_last_taken_branch initially 0
dynamic function branch_distance_list :LIST(INT) initially []


transition NOT_TAKEN_BRANCH ==
  steps_from_last_taken_branch := steps_from_last_taken_branch + 1

transition MULT_COUNT ==
  block
    NOT_TAKEN_BRANCH
    arithm := arithm + 1
    mults := mults + 1
  end

transition DIV_COUNT ==
  block
    NOT_TAKEN_BRANCH
    arithm := arithm + 1
    divs := divs + 1
  end

transition ADD_COUNT ==
  block
    NOT_TAKEN_BRANCH
    arithm := arithm + 1
    adds := adds + 1
  end

transition SHIFT_COUNT ==
  block
    NOT_TAKEN_BRANCH
    arithm := arithm + 1
    shifts := shifts + 1
  end

transition LOGIC_COUNT ==
  block
    NOT_TAKEN_BRANCH
    arithm := arithm + 1
    logic := logic + 1
  end


transition LOAD_COUNT ==
  block 
    NOT_TAKEN_BRANCH
    loads := loads + 1
  end

transition STORE_COUNT ==
  block
    NOT_TAKEN_BRANCH
    stores := stores + 1
  end


transition JUMP_COUNT (cond) ==
  block
    branches := branches + 1
    if eval_cond (cond, Neg, Zero)
    then taken_branches := taken_branches + 1
         branch_distance_list := append (branch_distance_list, [ steps_from_last_taken_branch ])
         steps_from_last_taken_branch := 0
    else not_taken_branches := not_taken_branches + 1
         steps_from_last_taken_branch := steps_from_last_taken_branch + 1
    end
  end


transition INSTR_COUNT ==
  block
    instrs := instrs + 1
    case instr (PMA) of
      // MAC instructions

      AND (rr, r1, r2)   : LOGIC_COUNT ;
      OR (rr, r1, r2)    : LOGIC_COUNT ;
      NAND (rr, r1, r2)  : LOGIC_COUNT ;
      NOR (rr, r1, r2)   : LOGIC_COUNT ;
      XOR (rr, r1, r2)   : LOGIC_COUNT ;
      ZERO (rr)		 : LOGIC_COUNT ;
      XNOR (rr, r1, r2)  : LOGIC_COUNT ;
      FF (rr)		 : LOGIC_COUNT ;

      ADD (rr, r1, r2)   : ADD_COUNT ;
      SUB (rr, r1, r2)   : ADD_COUNT ;
      CMP (r1, r2)       : ADD_COUNT ;

      LSH (rr, r1, r2)	 : SHIFT_COUNT ;

      MUL (rr, r1, r2)   : MULT_COUNT ;
      DIVQ (rr, r1, r2)  : DIV_COUNT ;
      DIVR (rr, r1, r2)  : DIV_COUNT ;

      // IOC instructions

      LD (rd, ra, disp)   : LOAD_COUNT ;
      LDA (rd, ra, disp)  : LOAD_COUNT ;
      LDPA (rd, ra, disp) : LOAD_COUNT ;
      ST (rd, ra, disp)	  : STORE_COUNT ;

      JUMP (ra, disp, cond) : JUMP_COUNT (cond) ;
      SKIP (ra, disp, cond) : JUMP_COUNT (cond)
    end
  end
