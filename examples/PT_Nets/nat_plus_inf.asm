//////////////////////////////////////////////////
//
// ASM-SL model of Petri net semantics
// Place/transition nets
//
//   nat_plus_inf.asm:
//     NAT_PLUS_INF data type
//     (needed to represent infinite capacities)
//

datatype NAT_PLUS_INF == { Fin : INT, Inf }

static function op_l 4 <# (x, y) ==
  case (x, y) of
    (Inf, _)         : false ;
    (Fin(_), Inf)    : true ;
    (Fin(x), Fin(y)) : x < y ;
    _                : false
  endcase

static function op_l 4 ># (x, y) ==
  case (x, y) of
    (_, Inf)         : false ;
    (Inf, Fin(_))    : true ;
    (Fin(x), Fin(y)) : x > y ;
    _                : false
  endcase    

static function op_l 4 <=# (x, y) == (x = y) or (x <# y)
static function op_l 4 >=# (x, y) == (x = y) or (x ># y)

static function op_l 6 +# (x, y) ==
  case (x, y) of
    (Inf, _)         : Inf ;
    (_, Inf)         : Inf ;
    (Fin(x), Fin(y)) : Fin (x + y) ;
    _                : undef
  endcase

static function op_l 6 -# (x, y) ==
  case (x, y) of
    (Inf, Fin(_))    : Inf ;
    (Fin(x), Fin(y)) : Fin (x - y) ;
    _		     : undef
  endcase
