static function node_radius == 10

static function node_pos ==
{ 1 -> (100, 40),
  2 -> (40, 100),
  3 -> (160, 100),
  4 -> (220, 40),
  5 -> (100, 160),
  6 -> (100, 220),
  7 -> (220, 220) }




static function v_nodes (N, pos, curr) ==
  let r == node_radius
  in [ let (x, y) == apply (pos, n) in
       let colour == if n = curr then V_Red else V_White endif
       in V_Oval ((x-r,y-r),(x+r,y+r),V_Black,colour,2)
       endlet endlet | n in set_to_list(N) ]
  endlet


static function op_l 5 ++ (x, y) == fadd (x, y)
static function op_l 5 -- (x, y) == fsub (x, y)
static function op_l 7 ** (x, y) == fmul (x, y)

static function sign (x) ==
  if x < 0.0 then fneg(1.0) elseif x > 0.0 then 1.0 else 0.0 endif

static function cshorten_ ((x1,y1),(x2,y2),r) ==
  // shorten arrow from circle (x1, y1)
  let (D_x, D_y) == (x2 -- x1, y2 -- y1)
  in if D_x = 0.0
     then (x1, y1 ++ (sign(D_y) ** r))
     else let d == fdiv (D_y, D_x)
	  in let eps_x == fdiv (r, sqrt (1.0 ++ (d ** d)))
	  in (x1 ++ (sign(D_x) ** eps_x), y1 ++ sign(D_x) ** d ** eps_x)
	  endlet endlet
     endif
  endlet

static function cshorten ((x1,y1),(x2,y2),r) ==
  let (x1,y1) == cshorten_ ( (int_to_float (x1), int_to_float (y1)),
			      (int_to_float (x2), int_to_float (y2)), int_to_float (r) )
  in (round (x1), round (y1))
  end


static function v_edges (E, pos) ==
  let E_list == set_to_list(E)
  in [ let ((x1,y1),(x2,y2)) == (apply (pos, n1), apply (pos, n2))
       in let (x1,y1) == cshorten ((x1,y1),(x2,y2),node_radius)
       in let (x2,y2) == cshorten ((x2,y2),(x1,y1),node_radius)
       in V_Arrow ((x1,y1),(x2,y2),V_Black,2)
       endlet endlet endlet | (n1,n2) in E_list ]
  end



derived function v_graph ==
  v_nodes (nodes (G), node_pos, currentNode) @
  v_edges (edges (G), node_pos)
