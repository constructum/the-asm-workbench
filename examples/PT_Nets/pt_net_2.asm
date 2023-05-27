//////////////////////////////////////////////////
//
// ASM-SL model of Petri net semantics
// Place/transition nets
//
//   pt_net_2.asm:
//     P/T net behaviour
//

static function S == let (S_,_,_,_,_,_) == N in S_ endlet
static function T == let (_,T_,_,_,_,_) == N in T_ endlet
static function F == let (_,_,F_,_,_,_) == N in F_ endlet
static function K (s_) == let (_,_,_,K_,_,_) == N in apply (K_,s_) endlet
static function W (e_) == let (_,_,_,_,W_,_) == N in apply (W_,e_) endlet
static function M0 == let (_,_,_,_,_,M0_) == N in M0_ endlet

dynamic function M initially MAP_TO_FUN M0

static function pre (t0)  == { s_ | (s_,t_) in F with t_ = t0 }
static function post (t0) == { s_ | (t_,s_) in F with t_ = t0 }

derived function active (t0) ==
  (forall s_ in pre(t0) : (M(s_) -# Fin(W(s_,t0)) >=# Fin(0)))
  and (forall s_ in post(t0) : (M(s_) +# Fin(W(t0,s_)) <=# K(s_)))

dynamic function last_transition :NODE initially undef

transition PTN_STEP ==
  choose t_ in T with active(t_)
    last_transition := t_
    do forall s_ in pre(t_) \ post(t_)
      M(s_) := M(s_) -# Fin(W(s_,t_))
    enddo
    do forall s_ in post(t_) \ pre(t_)
      M(s_) := M(s_) +# Fin(W(t_,s_))
    enddo
    do forall s_ in pre(t_) intersect post(t_)
      M(s_) := M(s_) -# Fin(W(s_,t_)) +# Fin(W(t_,s_))
    enddo
  endchoose
