//////////////////////////////////////////////////
//
// ASM-SL model of Petri net semantics
// Place/transition nets
//
//   pt_net_1.asm:
//     P/T net structure
//

freetype NODE == 
{ s : INT,     (* places      *)
  t : INT      (* transitions *) }

typealias EDGE == NODE * NODE

typealias PT_NET ==
( SET(NODE),                  // places
  SET(NODE),                  // transitions
  SET(EDGE),                  // edges
  MAP(NODE, NAT_PLUS_INF),    // place capacities (possibly infinite)
  MAP(EDGE, INT),             // edge weights (always finite)
  MAP(NODE, NAT_PLUS_INF) )   // initial marking of places

static function op_l 7 ** (set1, set2) ==
  Union ({ { (x1,x2) | x1 in set1 } | x2 in set2 })

static relation well_formed_S (S) == (S = { s(i) | s(i) in S })

static relation well_formed_T (T) == (T = { t(i) | t(i) in T })

static relation well_formed_F (S, T, F) ==
  (F <= ((S ** T) union (T ** S)))

static relation well_formed_K (S, K) ==
  (domain (K) = S) and (forall s_ in S : apply (K, s_) >=# Fin(0))

static relation well_formed_W (F, W) ==
  (domain (W) = F) and (forall e in F : apply (W, e) > 0)

static relation well_formed_M0 (S, K, M0) ==
  (domain (M0) = S)
  and (forall s_ in S : let m == apply (M0, s_)
                        in m >=# Fin(0) and m <=# apply (K, s_) endlet)

static relation well_formed_pt_net (S, T, F, K, W, M0) ==
  well_formed_S (S)
  and well_formed_T (T)
  and well_formed_F (S, T, F)
  and well_formed_K (S, K)
  and well_formed_W (F, W)
  and well_formed_M0 (S, K, M0)

static function check_pt_net (N) ==
  if well_formed_pt_net (N) then N else undef endif
