//////////////////////////////////////////////////
//
// ASM-SL model of Petri net semantics
// Place/transition nets
//
//   ex1.asm:
//     P/T net example
//     (s. Reisig, "Petrinetze", p. 70)
//

static function N :PT_NET ==
  let S == { s(i) | i in { 1..7 } }
  in let T == { t(i) | i in { 1..5 } }
  in let F == { (s(1),t(2)), (t(2),s(2)), (s(2),t(1)), (t(1),s(1)),
                 (t(2),s(3)), (t(2),s(4)), (s(4),t(3)),
                 (t(3),s(5)), (s(5),t(4)), (t(4),s(6)), (s(6),t(5)),
                 (t(5),s(7)), (s(7),t(3)) }
  in let K == { s(1) -> Fin(1), s(2) -> Fin(1), s(3) -> Inf, s(4) -> Fin(5),
		s(5) -> Fin(2), s(6) -> Fin(2), s(7) -> Fin(1) }
  in let W == override ({ e -> 1 | e in F }, { (t(2),s(4)) -> 3, (s(4),t(3)) -> 2 })
  in let M0 == { i -> Fin(j) | (i,j) in { (s(1),1), (s(2),0), (s(3),5), (s(4),3),
                                          (s(5),0), (s(6),2), (s(7),0) } }
  in (S, T, F, K, W, M0)   // check_pt_net (S, T, F, K, W, M0)
  endlet endlet endlet endlet endlet endlet

