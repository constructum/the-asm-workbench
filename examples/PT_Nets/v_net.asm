//////////////////////////////////////////////////
//
// ASM-SL model of Petri net semantics
//
//   v_net.asm:
//     visualization of Petri net structure
//     (places, transitions, arrows)
//

// visualization parameters

static function VN_place_radius == 10
static function VN_transition_radius == 8


// net-specific information: position of nodes

typealias VN_NODE_POS == MAP (NODE, V_COORD)


// nice arrows

static function sign (x) ==
  if x < 0.0 then fneg(1.0)
  elseif x > 0.0 then 1.0
  else 0.0
  endif

static function cshorten1_ ((x1,y1),(x2,y2),r) ==
  // shorten arrow from circle (x1, y1)
  let (Delta_x, Delta_y) == (fsub (x2, x1), fsub (y2, y1))
  in if Delta_x = 0.0
     then (x1, fadd (y1, fmul (sign(Delta_y), r)))
     else let delta == fdiv (Delta_y, Delta_x)
	  in let eps_x == fdiv (r, sqrt (fadd (1.0, fmul (delta, delta))))
	  in let eps_y == fmul (delta, eps_x)
	  in (fadd (x1, fmul (sign(Delta_x), eps_x)), fadd (y1, fmul(sign(Delta_x), eps_y)))
	  endlet endlet endlet
     endif
  endlet

static function cshorten1 ((x1,y1),(x2,y2),r) ==
  let (x1,y1) == cshorten1_ ( (int_to_float (x1), int_to_float (y1)),
			      (int_to_float (x2), int_to_float (y2)), int_to_float (r) )
  in (round (x1), round (y1))
  end

static function cshorten2_ ((x1,y1),(x2,y2),r) ==
  // shorten arrow to circle (x2,y2)
  let (Delta_x, Delta_y) == (fsub (x2, x1), fsub (y2, y1))
  in if Delta_x = 0.0
     then (x2, fsub (y2, fmul (sign(Delta_y), r)))
     else let delta == fdiv (Delta_y, Delta_x)
	  in let eps_x == fdiv (r, sqrt (fadd (1.0, fmul (delta, delta))))
	  in let eps_y == fmul (delta, eps_x)
	  in (fsub (x2, fmul (sign(Delta_x), eps_x)), fsub (y2, fmul(sign(Delta_x), eps_y)))
	  endlet endlet endlet
     endif
  endlet

static function cshorten2 ((x1,y1),(x2,y2),r) ==
  let (x2,y2) == cshorten2_ ( (int_to_float (x1), int_to_float (y1)),
			      (int_to_float (x2), int_to_float (y2)), int_to_float (r) )
  in (round (x2), round (y2))
  endlet

static function sshorten1_ ((x1,y1),(x2,y2),rq) ==
  let (Delta_x, Delta_y) == (fsub (x2, x1), fsub (y2, y1))
  in if Delta_y <= Delta_x
     then if Delta_y <= fneg (Delta_x)
	  then (fsub (x1, fmul (fdiv (Delta_x, Delta_y), rq)), fsub (y1, rq))
	  else (fadd (x1, rq), fadd (y1, fmul (fdiv (Delta_y, Delta_x), rq)))
	  endif
     else if Delta_y <= fneg (Delta_x)
	  then (fsub (x1, rq), fsub (y1, fmul (fdiv (Delta_y, Delta_x), rq)))
	  else (fadd (x1, fmul (fdiv (Delta_x, Delta_y), rq)), fadd (y1, rq))
	  endif
     endif
  endlet

static function sshorten1 ((x1,y1),(x2,y2),r) ==
  let (x1,y1) == sshorten1_ ( (int_to_float (x1), int_to_float (y1)),
			      (int_to_float (x2), int_to_float (y2)), int_to_float (r) )
  in (round (x1), round (y1))
  endlet

static function sshorten2_ ((x1,y1),(x2,y2),rq) ==
  let (Delta_x, Delta_y) == (fsub (x2, x1), fsub (y2, y1))
  in if Delta_y <= Delta_x
     then if Delta_y <= fneg (Delta_x)
	  then (fadd (x2, fmul (fdiv (Delta_x, Delta_y), rq)), fadd (y2, rq))
	  else (fsub (x2, rq), fsub (y2, fmul (fdiv (Delta_y, Delta_x), rq)))
	  endif
     else if Delta_y <= fneg (Delta_x)
	  then (fadd (x2, rq), fadd (y2, fmul (fdiv (Delta_y, Delta_x), rq)))
	  else (fsub (x2, fmul (fdiv (Delta_x, Delta_y), rq)), fsub (y2, rq))
	  endif
     endif
  endlet

static function sshorten2 ((x1,y1),(x2,y2),r) ==
  let (x2,y2) == sshorten2_ ( (int_to_float (x1), int_to_float (y1)),
			      (int_to_float (x2), int_to_float (y2)), int_to_float (r) )
  in (round (x2), round (y2))
  endlet


// graphical representation of net elements

static function VN_Places (S_, pos) ==
  let r == VN_place_radius
  in [ let (x, y) == apply (pos, s_)
       in V_Oval ((x-r,y-r),(x+r,y+r),V_Black,V_Transparent,1)
       endlet | s_ in set_to_list(S_) ]
  endlet

static function VN_Transitions (T_, pos) ==
  let r == VN_transition_radius
  in [ let (x, y) == apply (pos, t_)
       in V_Rectangle ((x-r,y-r),(x+r,y+r),V_Black,V_Transparent,1)
       endlet | t_ in set_to_list(T_) ]
  endlet


static function VN_Arrows (F_, pos) ==
  let F_list == set_to_list(F_)
  in [ let ((x1,y1),(x2,y2)) == (apply (pos, s(i)), apply (pos, t(j)))
       in let (x1,y1) == cshorten1 ((x1,y1),(x2,y2),VN_place_radius)
       in let (x2,y2) == sshorten2 ((x1,y1),(x2,y2),VN_transition_radius)
       in V_Arrow ((x1,y1),(x2,y2),V_Black,1)
       endlet endlet endlet | (s(i),t(j)) in F_list ] @
     [ let ((x1,y1),(x2,y2)) == (apply (pos, t(i)), apply (pos, s(j)))
       in let (x1,y1) == sshorten1 ((x1,y1),(x2,y2),VN_transition_radius)
       in let (x2,y2) == cshorten2 ((x1,y1),(x2,y2),VN_place_radius)
       in V_Arrow ((x1,y1),(x2,y2),V_Black,1)
       endlet endlet endlet | (t(i),s(j)) in F_list ]
  endlet

static function VN_Net ((S_,T_,F_,K_,W_,M0_), pos) ==
  (VN_Places (S_, pos))
  @ (VN_Transitions (T_, pos))
  @ (VN_Arrows (F_, pos))
