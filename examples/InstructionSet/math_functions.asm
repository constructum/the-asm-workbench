//////////////////////////////////////////////////
//
// G. Del Castillo, W. Hardt
// Fast dynamic analysis of instruction sets
// (CODES/CASHE'98, GI/ITG/GMM)
//
//   math_functions.asm
//

//------------------------------------------------
// arithmetical operations (function definitions)
//


static function bool_to_int (b) ==
  if b = false then 0 else 1 end

static function rbinary (x, i) ==
  if i >= 0
  then if andb (x, lsh (1, i)) != 0 then "1" else "0" end
       ## rbinary (x, i - 1)
  else ""
  end

static function binary (x) ==
  rbinary (x, 30)
  

// logical functions

static function or_fun (op1, op2)   == orb (op1, op2)
static function and_fun (op1, op2)  == andb (op1, op2)
static function nor_fun (op1, op2)  == notb (orb (op1, op2))
static function nand_fun (op1, op2) == notb (andb (op1, op2))
static function xor_fun (op1, op2)  == xorb (op1, op2)
static function zero_fun	    == 0
static function xnor_fun (op1, op2) == notb (xorb (op1, op2))
static function ff_fun		    == ~(1)


// additive functions

static function add_fun (op1, op2) == op1 + op2
static function sub_fun (op1, op2) == op1 - op2
static function cmp_fun (op1, op2) == op1 - op2


// logical shift function

static function lsh_fun (op1, op2) ==
  if op2 > 0 then lsh (op1, op2)
  elseif op2 < 0 then rsh (op1, ~(op2))    // ~(op2): ASM-SL notation for "-op2"
  else op1
  end


// multiplication and division

static function mul_fun (op1, op2) ==
  op1 * op2

static function divq_fun (op1, op2) ==
  op1 div op2

static function divr_fun (op1, op2) ==
  op1 mod op2
