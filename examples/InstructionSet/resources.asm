//////////////////////////////////////////////////
//
// G. Del Castillo, W. Hardt
// Fast dynamic analysis of instruction sets
// (CODES/CASHE'98, GI/ITG/GMM)
//
//   resources.asm
//

//------------------------------------------------
// the program memory "instr"
// the program counter "PMA" (Program Memory Address)
//

static function instr : INT -> INSTR ==
  MAP_TO_FUN prog

dynamic function PMA  : INT initially 0


//------------------------------------------------
// the data memory "mem"
//

dynamic function mem : INT -> INT
initially MAP_TO_FUN
  map_union (
    { A_addr + i -> apply (A_Matrix, i) | i in domain (A_Matrix) },
    { B_addr + i -> apply (B_Matrix, i) | i in domain (B_Matrix) }
  )


//------------------------------------------------
// the 64 general purpose registers "reg"
//

dynamic function reg : REG -> INT
  initially MAP_TO_FUN emptymap


//------------------------------------------------
// the condition code register (status)
//

dynamic function Neg	  initially false
dynamic function Zero	  initially false
dynamic function Divz	  initially false