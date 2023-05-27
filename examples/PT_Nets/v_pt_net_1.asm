//////////////////////////////////////////////////
//
// ASM-SL model of Petri net semantics
// Place/transition nets
//
//   v_pt_net_1.asm:
//     visualization of P/T nets: structure
//

// graphical representation of net structure (static)

static function VPTN_NetStructure ((S_,T_,F_,K_,W_,M0_), pos) ==
  (VN_Places (S_, pos))
  @ (VN_Transitions (T_, pos))
  @ (VN_Arrows (F_, pos))


