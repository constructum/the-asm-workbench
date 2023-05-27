//////////////////////////////////////////////////
//
// ASM-SL model of Petri net semantics
// Place/transition nets
//
//   v_pt_net_2.asm:
//     visualization of P/T nets: markings
//

// graphical representation of net, including current marking (dynamic)

static function VPTN_OneToken (x,y) ==
  V_Oval ((x-1,y-1),(x+1,y+1),V_Black,V_Black,1)

static function VPTN_Tokens ((x,y), how_many) ==
  if how_many = 0
  then []
  elseif how_many = 1
  then [ VPTN_OneToken (x,y) ]
  elseif how_many >= 2 and how_many <= 6
  then let alpha == fdiv (6.28318530717959, int_to_float(how_many))
       in let r == int_to_float (VN_place_radius div 2)
       in [ VPTN_OneToken
            ( round (fadd (int_to_float (x), fmul (r, cos (fmul (alpha, int_to_float(i)))))),
  	      round (fadd (int_to_float (y), fmul (r, sin (fmul (alpha, int_to_float(i)))))) ) |
            i in [ 0 .. how_many-1 ] ]
       endlet endlet
  else [ V_Text (int_to_string (how_many), (x,y), V_Black, V_Grey, V_Center) ]
  endif


derived function VPTN_MarkingOfPlace (s_, pos) ==
  let (x,y) == apply (pos, s_)
  in case M (s_) of
       Fin (i) : VPTN_Tokens ((x,y), i) ;
       Inf     : [ V_Text ("inf", (x,y), V_Black, V_Grey, V_Center) ]
     endcase    
  endlet

static function flatten (L) ==
  case L of
    []      : [] ;
    x :: xs : x @ flatten (xs)
  endcase
    
derived function VPTN_Marking (pos) ==
  flatten ([ VPTN_MarkingOfPlace (s_, pos) | s_ in set_to_list (domain (FUN_TO_MAP M)) ])

derived function VPTN_Net ==
  VPTN_NetStructure (N, N_pos)
  @ VPTN_Marking (N_pos)

