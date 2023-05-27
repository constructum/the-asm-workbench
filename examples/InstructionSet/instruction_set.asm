//////////////////////////////////////////////////
//
// G. Del Castillo, W. Hardt
// Fast dynamic analysis of instruction sets
// (CODES/CASHE'98, GI/ITG/GMM)
//
//   instruction_set.asm
//

//------------------------------------------------
// the instruction set (type "INSTR")
//

datatype JUMP_COND ==
{ TRUE, FALSE, EQ, NE, LT, LE, GT, GE }


datatype REG ==
{ R : INT }      // R (x) denotes addresses of registers

datatype DISP ==
{ Disp : INT }   // displacement Disp(x), x in [0,2^24-1] (24 bits)

static function disp_addr (Disp (addr)) == addr


datatype INSTR ==
{ // arithmetic-logic instructions
  OR   : REG * REG * REG,
  AND  : REG * REG * REG,
  NOR  : REG * REG * REG,
  NAND : REG * REG * REG,
  XOR  : REG * REG * REG,
  ZERO : REG,
  XNOR : REG * REG * REG,
  FF   : REG,

  ADD  : REG * REG * REG,
  SUB  : REG * REG * REG,
  CMP  : REG * REG,

  LSH  : REG * REG * REG,

  MUL  : REG * REG * REG,

  DIVQ : REG * REG * REG,
  DIVR : REG * REG * REG,

  // I/O instructions and branches
  LD   : REG * REG * DISP,
  LDA  : REG * REG * DISP,
  LDPA : REG * REG * DISP,
  ST   : REG * REG * DISP,

  JUMP : REG * DISP * JUMP_COND,
  SKIP : REG * DISP * JUMP_COND,

  HALT }


static function is_jump_instruction (I) ==
  case I of
    JUMP (_, _, _) : true ;
    SKIP (_, _, _) : true ;
    otherwise false
  end
