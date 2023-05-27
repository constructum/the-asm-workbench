//////////////////////////////////////////////////
//
// ASM-SL model of Petri net semantics
// Place/transition nets
//
//   v_ex1.asm:
//     visualization data for example P/T net
//

static function N_pos :VN_NODE_POS ==
  { n -> (x,y+20) | (n,(x,y)) in
      map_to_set ({ t(1) -> (10,50), s(1) -> (50,10), t(2) -> (90,50), s(2) -> (50,90),
	s(3) -> (130,10), s(4) -> (140,50), t(3) -> (190,50),
	s(5) -> (220,80), t(4) -> (260,80), s(6) -> (290,50),
	t(5) -> (260,20), s(7) -> (220,20) }) }
