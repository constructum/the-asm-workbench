structure ASM_Value =
struct
  structure Type = ASM_Type

  val replace = String_.replace

  type TYPE = Type.TYPE


  (* --- implementation of FSET/FMAP is an adaption of binary set/map from the SML/NJ library --- *)

  datatype VALUE =
    UNDEF
    | BOOL of bool
    | INT of IntInf.int
    | FLOAT of real
    | STRING of string
    | TUPLE of VALUE list
    | CELL of string * VALUE
    | FSET_E
    | FSET_T of { elt : VALUE, cnt : int, left : VALUE, right : VALUE }
    | FMAP_E
    | FMAP_T of { key : VALUE, value : VALUE, cnt : int, left : VALUE, right : VALUE }


  fun isList (CELL ("nil", _)) = true
    | isList (CELL ("::", _)) = true
    | isList _ = false

  fun isFSet (FSET_E) = true
    | isFSet (FSET_T _) = true
    | isFSet _ = false

  fun isFMap (FMAP_E) = true
    | isFMap (FMAP_T _) = true
    | isFMap _ = false

  exception NotSet of VALUE
  exception NotMap of VALUE

  fun errorNotSetLR (FSET_E  , r) = raise NotSet r
    | errorNotSetLR (FSET_T _, r) = raise NotSet r
    | errorNotSetLR (l, _)        = raise NotSet l

  fun errorNotMapLR (FMAP_E  , r) = raise NotMap r
    | errorNotMapLR (FMAP_T _, r) = raise NotMap r
    | errorNotMapLR (l, _)        = raise NotMap l

  local
    val enumCons = fn
	  UNDEF      => 0
	| BOOL _     => 1
	| INT _      => 2
	| FLOAT _    => 3
	| STRING _   => 4
	| TUPLE _    => 5
	| CELL _     => 6
	| FSET_E     => 7
	| FSET_T _   => 8
        | FMAP_E     => 9
        | FMAP_T _   => 10

    fun snext ((t as FSET_T{right, ...})::rest) = (t, sleft(right, rest))
      | snext _ = (FSET_E, [])
    and sleft (FSET_E, rest) = rest
      | sleft (t as FSET_T{left=l, ...}, rest) = sleft(l, t::rest)
      | sleft (x, rest) = raise NotSet x

    fun mnext ((t as FMAP_T{right, ...})::rest) = (t, mleft(right, rest))
      | mnext _ = (FMAP_E, [])
    and mleft (FMAP_E, rest) = rest
      | mleft (t as FMAP_T{left=l, ...}, rest) = mleft(l, t::rest)
      | mleft (x, rest) = raise NotMap x
  in
    fun mapCompare (s1, s2) = let
	  fun cmp (t1, t2) = (case (mnext t1, mnext t2)
		 of ((FMAP_E, _), (FMAP_E, _)) => EQUAL
		  | ((FMAP_E, _), _) => LESS
		  | (_, (FMAP_E, _)) => GREATER
		  | ((FMAP_T{key=x1, value=y1, ...}, r1), (FMAP_T{key=x2, value=y2, ...}, r2)) => (
		      case compare(x1, x2)
		       of EQUAL => (case compare(y1, y2)
			     of EQUAL => cmp (r1, r2)
			      | order => order
			    (* end case *))
			| order => order
		      (* end case *))
		  | ((x,_), (y,_)) => errorNotMapLR (x,y))
	  in
	    cmp (mleft(s1, []), mleft(s2, []))
	  end

    and setCompare (s1, s2) = let
	fun cmp (t1, t2) = (case (snext t1, snext t2)
	       of ((FSET_E, _), (FSET_E, _)) => EQUAL
		| ((FSET_E, _), _) => LESS
		| (_, (FSET_E, _)) => GREATER
		| ((FSET_T{elt=e1, ...}, r1), (FSET_T{elt=e2, ...}, r2)) => (
		    case compare(e1, e2)
		     of EQUAL => cmp (r1, r2)
		      | order => order
		    (* end case *))
		| ((l,_),(r,_)) => errorNotSetLR (l,r)
	      (* end case *))
	in
	  cmp (sleft(s1, []), sleft(s2, []))
	end

    and compare (ord_key_1, ord_key_2) =
      case (ord_key_1, ord_key_2) of
	(UNDEF, UNDEF)       => EQUAL
      | (BOOL false, BOOL y) => if y = false then EQUAL else LESS
      | (BOOL true,  BOOL y) => if y = true  then EQUAL else GREATER
      | (INT x, INT y)       => IntInf.compare (x, y)
      | (FLOAT x, FLOAT y)   => Real.compare (x, y)
      | (STRING x, STRING y) => String.compare (x, y)
      | (TUPLE xs, TUPLE ys) => List_.compare compare (xs, ys)
      | (CELL (xtag, x), CELL (ytag, y)) =>
	  if xtag <> ytag
	  then String.compare (xtag, ytag)
	  else compare (x, y)
      | (FSET_E, FSET_E) => EQUAL
      | (S1 as FSET_E,   S2 as FSET_T _) => LESS
      | (S1 as FSET_T _, S2 as FSET_E  ) => GREATER
      | (S1 as FSET_T _, S2 as FSET_T _) => setCompare (S1, S2)
      | (FMAP_E, FMAP_E) => EQUAL
      | (S1 as FMAP_E,   S2 as FMAP_T _) => LESS
      | (S1 as FMAP_T _, S2 as FMAP_E  ) => GREATER
      | (S1 as FMAP_T _, S2 as FMAP_T _) => mapCompare (S1, S2)
      | (x, y) => Int.compare (enumCons x, enumCons y)
  end


  structure Set =
  struct
    val keyCompare = compare

    fun numItems FSET_E = 0
      | numItems (FSET_T{cnt,...}) = cnt
      | numItems x = raise NotSet x
        
    fun isEmpty FSET_E = true
      | isEmpty (FSET_T _) = false
      | isEmpty x = raise NotSet x

    fun mkT(v,n,l,r) = FSET_T{elt=v,cnt=n,left=l,right=r}

    fun valueIsSet FSET_E = true
      | valueIsSet (FSET_T _) = true
      | valueIsSet _ = false

      (* N(v,l,r) = T(v,1+numItems(l)+numItems(r),l,r) *)
    fun N(v,FSET_E,FSET_E) = mkT(v,1,FSET_E,FSET_E)
      | N(v,FSET_E,r as FSET_T{cnt=n,...}) = mkT(v,n+1,FSET_E,r)
      | N(v,l as FSET_T{cnt=n,...}, FSET_E) = mkT(v,n+1,l,FSET_E)
      | N(v,l as FSET_T{cnt=n,...}, r as FSET_T{cnt=m,...}) = mkT(v,n+m+1,l,r)
      | N(_,l,r) = errorNotSetLR (l,r)

    fun single_L (a,x,FSET_T{elt=b,left=y,right=z,...}) = N(b,N(a,x,y),z)
      | single_L _ = raise Match
    fun single_R (b,FSET_T{elt=a,left=x,right=y,...},z) = N(a,x,N(b,y,z))
      | single_R _ = raise Match
    fun double_L (a,w,FSET_T{elt=c,left=FSET_T{elt=b,left=x,right=y,...},right=z,...}) =
          N(b,N(a,w,x),N(c,y,z))
      | double_L _ = raise Match
    fun double_R (c,FSET_T{elt=a,left=w,right=FSET_T{elt=b,left=x,right=y,...},...},z) =
          N(b,N(a,w,x),N(c,y,z))
      | double_R _ = raise Match

    (*
    **  val weight = 3
    **  fun wt i = weight * i
    *)
    fun wt (i : int) = i + i + i

    fun T' (v,FSET_E,FSET_E) = mkT(v,1,FSET_E,FSET_E)
      | T' (v,FSET_E,r as FSET_T{left=FSET_E,right=FSET_E,...}) = mkT(v,2,FSET_E,r)
      | T' (v,l as FSET_T{left=FSET_E,right=FSET_E,...},FSET_E) = mkT(v,2,l,FSET_E)

      | T' (p as (_,FSET_E,FSET_T{left=FSET_T _,right=FSET_E,...})) = double_L p
      | T' (p as (_,FSET_T{left=FSET_E,right=FSET_T _,...},FSET_E)) = double_R p

        (* these cases almost never happen with small weight*)
      | T' (p as (_,FSET_E,FSET_T{left=FSET_T{cnt=ln,...},right=FSET_T{cnt=rn,...},...})) =
            if ln<rn then single_L p else double_L p
      | T' (p as (_,FSET_T{left=FSET_T{cnt=ln,...},right=FSET_T{cnt=rn,...},...},FSET_E)) =
            if ln>rn then single_R p else double_R p

      | T' (p as (_,FSET_E,FSET_T{left=FSET_E,...})) = single_L p
      | T' (p as (_,FSET_T{right=FSET_E,...},FSET_E)) = single_R p

      | T' (p as (v,l as FSET_T{elt=lv,cnt=ln,left=ll,right=lr},
              r as FSET_T{elt=rv,cnt=rn,left=rl,right=rr})) =
          if rn >= wt ln (*right is too big*)
            then
              let val rln = numItems rl
                  val rrn = numItems rr
              in
                if rln < rrn then single_L p else double_L p
              end
          else if ln >= wt rn (*left is too big*)
            then
              let val lln = numItems ll
                  val lrn = numItems lr
              in
                if lrn < lln then single_R p else double_R p
              end
          else mkT(v,ln+rn+1,l,r)
      | T' (_,l,r) = if valueIsSet l then raise NotSet r else raise NotSet l

    fun add (FSET_E,x) = mkT(x,1,FSET_E,FSET_E)
      | add (set as FSET_T{elt=v,left=l,right=r,cnt},x) =
        ( case keyCompare(x,v) of
            LESS => T'(v,add(l,x),r)
          | GREATER => T'(v,l,add(r,x))
          | EQUAL => mkT(x,cnt,l,r) )
      | add (s,_) = raise NotSet s
    fun add' (s, x) = add(x, s)

    fun concat3 (FSET_E,v,r) = add(r,v)
      | concat3 (l,v,FSET_E) = add(l,v)
      | concat3 (l as FSET_T{elt=v1,cnt=n1,left=l1,right=r1}, v, 
                  r as FSET_T{elt=v2,cnt=n2,left=l2,right=r2}) =
        if wt n1 < n2 then T'(v2,concat3(l,v,l2),r2)
        else if wt n2 < n1 then T'(v1,l1,concat3(r1,v,r))
        else N(v,l,r)
      | concat3 (l,v,r) = errorNotSetLR (l,r)

    fun split_lt (FSET_E,x) = FSET_E
      | split_lt (FSET_T{elt=v,left=l,right=r,...},x) =
        ( case keyCompare(v,x) of
            GREATER => split_lt(l,x)
          | LESS => concat3(l,v,split_lt(r,x))
          | _ => l )
      | split_lt (a,x) = raise NotSet a

    fun split_gt (FSET_E,x) = FSET_E
      | split_gt (FSET_T{elt=v,left=l,right=r,...},x) =
        ( case keyCompare(v,x) of
            LESS => split_gt(r,x)
          | GREATER => concat3(split_gt(l,x),v,r)
          | _ => r )
      | split_gt (a,x) = raise NotSet a

    fun min (FSET_T{elt=v,left=FSET_E,...}) = v
      | min (FSET_T{left=l,...}) = min l
      | min _ = raise Match
        
    fun delmin (FSET_T{left=FSET_E,right=r,...}) = r
      | delmin (FSET_T{elt=v,left=l,right=r,...}) = T'(v,delmin l,r)
      | delmin _ = raise Match

    fun delete' (FSET_E,r) = r
      | delete' (l,FSET_E) = l
      | delete' (l,r) = T'(min r,l,delmin r)

    fun concat (FSET_E, s) = s
      | concat (s, FSET_E) = s
      | concat (t1 as FSET_T{elt=v1,cnt=n1,left=l1,right=r1}, 
                  t2 as FSET_T{elt=v2,cnt=n2,left=l2,right=r2}) =
          if wt n1 < n2 then T'(v2,concat(t1,l2),r2)
          else if wt n2 < n1 then T'(v1,l1,concat(r1,t2))
          else T'(min t2,t1, delmin t2)
      | concat (l,r) = errorNotSetLR (l,r)

    local
      fun trim (lo,hi,FSET_E) = FSET_E
        | trim (lo,hi,s as FSET_T{elt=v,left=l,right=r,...}) =
            if keyCompare(v,lo) = GREATER
              then if keyCompare(v,hi) = LESS then s else trim(lo,hi,l)
              else trim(lo,hi,r)
        | trim (lo,hi,x) = raise NotSet x
                
      fun uni_bd (s,FSET_E,_,_) = s
        | uni_bd (FSET_E,FSET_T{elt=v,left=l,right=r,...},lo,hi) = 
             concat3(split_gt(l,lo),v,split_lt(r,hi))
        | uni_bd (FSET_T{elt=v,left=l1,right=r1,...}, 
                   s2 as FSET_T{elt=v2,left=l2,right=r2,...},lo,hi) =
            concat3(uni_bd(l1,trim(lo,v,s2),lo,v),
                v, 
                uni_bd(r1,trim(v,hi,s2),v,hi))
              (* inv:  lo < v < hi *)
        | uni_bd (l,r,_,_) = errorNotSetLR (l,r)

        (* all the other versions of uni and trim are
         * specializations of the above two functions with
         *     lo=-infinity and/or hi=+infinity 
         *)

      fun trim_lo (_, FSET_E) = FSET_E
        | trim_lo (lo,s as FSET_T{elt=v,right=r,...}) =
          ( case keyCompare(v,lo) of
              GREATER => s
            | _ => trim_lo(lo,r) )
        | trim_lo (_,x) = raise NotSet x

      fun trim_hi (_, FSET_E) = FSET_E
        | trim_hi (hi,s as FSET_T{elt=v,left=l,...}) =
          ( case keyCompare(v,hi) of
              LESS => s
            | _ => trim_hi(hi,l) )
        | trim_hi (_,x) = raise NotSet x
                
      fun uni_hi (s,FSET_E,_) = s
        | uni_hi (FSET_E,FSET_T{elt=v,left=l,right=r,...},hi) = 
             concat3(l,v,split_lt(r,hi))
        | uni_hi (FSET_T{elt=v,left=l1,right=r1,...}, 
                   s2 as FSET_T{elt=v2,left=l2,right=r2,...},hi) =
            concat3(uni_hi(l1,trim_hi(v,s2),v),v,uni_bd(r1,trim(v,hi,s2),v,hi))
        | uni_hi (l,r,hi) = errorNotSetLR (l,r)

      fun uni_lo (s,FSET_E,_) = s
        | uni_lo (FSET_E,FSET_T{elt=v,left=l,right=r,...},lo) = 
             concat3(split_gt(l,lo),v,r)
        | uni_lo (FSET_T{elt=v,left=l1,right=r1,...}, 
                   s2 as FSET_T{elt=v2,left=l2,right=r2,...},lo) =
            concat3(uni_bd(l1,trim(lo,v,s2),lo,v),v,uni_lo(r1,trim_lo(v,s2),v))
        | uni_lo (l,r,hi) = errorNotSetLR (l,r)

      fun uni (s,FSET_E) = s
        | uni (FSET_E,s) = s
        | uni (FSET_T{elt=v,left=l1,right=r1,...}, 
                s2 as FSET_T{elt=v2,left=l2,right=r2,...}) =
            concat3(uni_hi(l1,trim_hi(v,s2),v), v, uni_lo(r1,trim_lo(v,s2),v))
        | uni (l,r) = errorNotSetLR (l,r)

    in
      val hedge_union = uni
    end

    fun old_union (FSET_E,s2)  = s2
      | old_union (s1,FSET_E)  = s1
      | old_union (FSET_T{elt=v,left=l,right=r,...},s2) = 
          let val l2 = split_lt(s2,v)
              val r2 = split_gt(s2,v)
          in
            concat3(old_union(l,l2),v,old_union(r,r2))
          end
      | old_union (l,r) = errorNotSetLR (l,r)

    val empty = FSET_E
    fun singleton x = FSET_T{elt=x,cnt=1,left=FSET_E,right=FSET_E}

    fun addList (s,l) = List.foldl (fn (i,s) => add(s,i)) s l
    fun fromList l = List.foldl add' empty l
 
    val add = add

    fun member (set, x) = let
	  fun pk FSET_E = false
	    | pk (FSET_T{elt=v, left=l, right=r, ...}) = (
	      ( case keyCompare(x,v)
		 of LESS => pk l
		  | EQUAL => true
		  | GREATER => pk r
		(* end case *)) )
            | pk x = raise NotSet x
	  in
	    pk set
	  end

    local
        (* true if every item in t is in t' *)
      fun treeIn (t,t') = let
            fun isIn FSET_E = true
              | isIn (FSET_T{elt,left=FSET_E,right=FSET_E,...}) = member(t',elt)
              | isIn (FSET_T{elt,left,right=FSET_E,...}) = 
                  member(t',elt) andalso isIn left
              | isIn (FSET_T{elt,left=FSET_E,right,...}) = 
                  member(t',elt) andalso isIn right
              | isIn (FSET_T{elt,left,right,...}) = 
                  member(t',elt) andalso isIn left andalso isIn right
              | isIn x = raise NotSet x
            in
              isIn t
            end
    in
    fun isSubset (FSET_E,_) = true
      | isSubset (_,FSET_E) = false
      | isSubset (t as FSET_T{cnt=n,...},t' as FSET_T{cnt=n',...}) =
          (n<=n') andalso treeIn (t,t')
      | isSubset (l,r) = errorNotSetLR (l,r)

    fun equal (FSET_E,FSET_E) = true
      | equal (t as FSET_T{cnt=n,...},t' as FSET_T{cnt=n',...}) =
          (n=n') andalso treeIn (t,t')
      | equal _ = false
    end

    fun delete (FSET_E,x) = raise LibBase.NotFound
      | delete (set as FSET_T{elt=v,left=l,right=r,...},x) =
        ( case keyCompare(x,v) of
            LESS => T'(v,delete(l,x),r)
          | GREATER => T'(v,l,delete(r,x))
          | _ => delete'(l,r) )
      | delete (a,x) = raise NotSet a

    val union = hedge_union

    fun intersection (FSET_E, _) = FSET_E
      | intersection (_, FSET_E) = FSET_E
      | intersection (s, FSET_T{elt=v,left=l,right=r,...}) = let
	  val l2 = split_lt(s,v)
	  val r2 = split_gt(s,v)
          in
            if member(s,v)
	      then concat3(intersection(l2,l),v,intersection(r2,r))
	      else concat(intersection(l2,l),intersection(r2,r))
          end
      | intersection (l,r) = errorNotSetLR (l,r)

    fun difference (FSET_E,s) = FSET_E
      | difference (s,FSET_E)  = s
      | difference (s, FSET_T{elt=v,left=l,right=r,...}) =
          let val l2 = split_lt(s,v)
              val r2 = split_gt(s,v)
          in
            concat(difference(l2,l),difference(r2,r))
          end
      | difference (l,r) = errorNotSetLR (l,r)

    val compare = setCompare

    fun map f set = let
	  fun map'(acc, FSET_E) = acc
	    | map'(acc, FSET_T{elt,left,right,...}) =
		map' (add (map' (acc, left), f elt), right)
            | map'(acc, x) = raise NotSet x
	  in 
	    map' (FSET_E, set)
	  end

    fun app apf =
         let fun apply FSET_E = ()
               | apply (FSET_T{elt,left,right,...}) = 
                   (apply left;apf elt; apply right)
               | apply x = raise NotSet x
         in
           apply
         end

    fun foldl f b set = let
	  fun foldf (FSET_E, b) = b
	    | foldf (FSET_T{elt,left,right,...}, b) = 
		foldf (right, f(elt, foldf (left, b)))
            | foldf (a, b) = raise NotSet a
          in
            foldf (set, b)
          end

    fun foldr f b set = let
	  fun foldf (FSET_E, b) = b
	    | foldf (FSET_T{elt,left,right,...}, b) = 
		foldf (left, f(elt, foldf (right, b)))
            | foldf (a, b) = raise NotSet a
          in
            foldf (set, b)
          end

    fun listItems set = foldr (op::) [] set

    fun filter pred set =
	  foldl (fn (item, s) => if (pred item) then add(s, item) else s)
	    empty set

    fun find p FSET_E = NONE
      | find p (FSET_T{elt,left,right,...}) = (case find p left
	   of NONE => if (p elt)
		then SOME elt
		else find p right
	    | a => a
	  (* end case *))
      | find p x = raise NotSet x

    fun exists p FSET_E = false
      | exists p (FSET_T{elt, left, right,...}) =
	  (exists p left) orelse (p elt) orelse (exists p right)
      | exists p x = raise NotSet x

  end (* structure Set *)


  structure Map =
  struct
    val keyCompare = compare

    fun wt (i : int) = i + i + i

    val empty = FMAP_E

    fun isEmpty FMAP_E = true
      | isEmpty (FMAP_T _) = false
      | isEmpty x = raise NotMap x

    fun numItems FMAP_E = 0
      | numItems (FMAP_T{cnt,...}) = cnt
      | numItems x = raise NotMap x

    (* return the first item in the map (or NONE if it is empty) *)
    fun first FMAP_E = NONE
      | first (FMAP_T{value, left=FMAP_E, ...}) = SOME value
      | first (FMAP_T{left, ...}) = first left
      | first x = raise NotMap x

    (* return the first item in the map and its key (or NONE if it is empty) *)
    fun firsti FMAP_E = NONE
      | firsti (FMAP_T{key, value, left=FMAP_E, ...}) = SOME(key, value)
      | firsti (FMAP_T{left, ...}) = firsti left
      | firsti x = raise NotMap x

    local
      fun N(k,v,FMAP_E,FMAP_E) = FMAP_T{key=k,value=v,cnt=1,left=FMAP_E,right=FMAP_E}
	| N(k,v,FMAP_E,r as FMAP_T n) = FMAP_T{key=k,value=v,cnt=1+(#cnt n),left=FMAP_E,right=r}
	| N(k,v,l as FMAP_T n,FMAP_E) = FMAP_T{key=k,value=v,cnt=1+(#cnt n),left=l,right=FMAP_E}
	| N(k,v,l as FMAP_T n,r as FMAP_T n') = 
	    FMAP_T{key=k,value=v,cnt=1+(#cnt n)+(#cnt n'),left=l,right=r}
	| N(k,v,l,r) = errorNotMapLR (l,r)

      fun single_L (a,av,x,FMAP_T{key=b,value=bv,left=y,right=z,...}) = 
	    N(b,bv,N(a,av,x,y),z)
	| single_L _ = raise Match
      fun single_R (b,bv,FMAP_T{key=a,value=av,left=x,right=y,...},z) = 
	    N(a,av,x,N(b,bv,y,z))
	| single_R _ = raise Match
      fun double_L (a,av,w,FMAP_T{key=c,value=cv,left=FMAP_T{key=b,value=bv,left=x,right=y,...},right=z,...}) =
	    N(b,bv,N(a,av,w,x),N(c,cv,y,z))
	| double_L _ = raise Match
      fun double_R (c,cv,FMAP_T{key=a,value=av,left=w,right=FMAP_T{key=b,value=bv,left=x,right=y,...},...},z) = 
	    N(b,bv,N(a,av,w,x),N(c,cv,y,z))
	| double_R _ = raise Match

      fun T' (k,v,FMAP_E,FMAP_E) = FMAP_T{key=k,value=v,cnt=1,left=FMAP_E,right=FMAP_E}
	| T' (k,v,FMAP_E,r as FMAP_T{right=FMAP_E,left=FMAP_E,...}) =
	    FMAP_T{key=k,value=v,cnt=2,left=FMAP_E,right=r}
	| T' (k,v,l as FMAP_T{right=FMAP_E,left=FMAP_E,...},FMAP_E) =
	    FMAP_T{key=k,value=v,cnt=2,left=l,right=FMAP_E}

	| T' (p as (_,_,FMAP_E,FMAP_T{left=FMAP_T _,right=FMAP_E,...})) = double_L p
	| T' (p as (_,_,FMAP_T{left=FMAP_E,right=FMAP_T _,...},FMAP_E)) = double_R p

	  (* these cases almost never happen with small weight*)
	| T' (p as (_,_,FMAP_E,FMAP_T{left=FMAP_T{cnt=ln,...},right=FMAP_T{cnt=rn,...},...})) =
	    if ln < rn then single_L p else double_L p
	| T' (p as (_,_,FMAP_T{left=FMAP_T{cnt=ln,...},right=FMAP_T{cnt=rn,...},...},FMAP_E)) =
	    if ln > rn then single_R p else double_R p

	| T' (p as (_,_,FMAP_E,FMAP_T{left=FMAP_E,...})) = single_L p
	| T' (p as (_,_,FMAP_T{right=FMAP_E,...},FMAP_E)) = single_R p

	| T' (p as (k,v,l as FMAP_T{cnt=ln,left=ll,right=lr,...},
			r as FMAP_T{cnt=rn,left=rl,right=rr,...})) =
	    if rn >= wt ln then (*right is too big*)
	      let val rln = numItems rl
		  val rrn = numItems rr
	      in
		if rln < rrn then  single_L p  else  double_L p
	      end

	    else if ln >= wt rn then  (*left is too big*)
	      let val lln = numItems ll
		  val lrn = numItems lr
	      in
		if lrn < lln then  single_R p  else  double_R p
	      end

	    else FMAP_T{key=k,value=v,cnt=ln+rn+1,left=l,right=r}

	| T' (k,v,l,r) = errorNotMapLR (l,r)

      local
	fun min (FMAP_T{left=FMAP_E,key,value,...}) = (key,value)
	  | min (FMAP_T{left,...}) = min left
	  | min _ = raise Match

	fun delmin (FMAP_T{left=FMAP_E,right,...}) = right
	  | delmin (FMAP_T{key,value,left,right,...}) = T'(key,value,delmin left,right)
	  | delmin _ = raise Match
      in
	fun delete' (FMAP_E,r) = r
	  | delete' (l,FMAP_E) = l
	  | delete' (l as FMAP_T _,r as FMAP_T _) = let val (mink,minv) = min r in
	      T'(mink,minv,l,delmin r)
	    end
	  | delete' (l,r) = errorNotMapLR (l,r)
      end
    in
      fun mkDict () = FMAP_E

      fun singleton (x,v) = FMAP_T{key=x,value=v,cnt=1,left=FMAP_E,right=FMAP_E}

      fun insert (FMAP_E,x,v) = FMAP_T{key=x,value=v,cnt=1,left=FMAP_E,right=FMAP_E}
	| insert (FMAP_T(set as {key,left,right,value,...}),x,v) =
	  ( case keyCompare (key,x) of
	      GREATER => T'(key,value,insert(left,x,v),right)
	    | LESS => T'(key,value,left,insert(right,x,v))
	    | _ => FMAP_T{key=x,value=v,left=left,right=right,cnt= #cnt set} )
	| insert (a,x,v) = raise NotMap a
      fun insert' ((k, x), m) = insert(m, k, x)

      fun inDomain (set, x) = let 
	    fun mem FMAP_E = false
	      | mem (FMAP_T(n as {key,left,right,...})) = (case keyCompare (x,key)
		   of GREATER => mem right
		    | EQUAL => true
		    | LESS => mem left)
	      | mem x = raise NotMap x
	    in
	      mem set
	    end

      fun find (set, x) = let 
	    fun mem FMAP_E = NONE
	      | mem (FMAP_T(n as {key,left,right,...})) = ( case keyCompare (x,key)
		   of GREATER => mem right
		    | EQUAL => SOME(#value n)
		    | LESS => mem left )
	      | mem x = raise NotMap x
	    in
	      mem set
	    end

      fun remove (FMAP_E,x) = raise LibBase.NotFound
	| remove (set as FMAP_T{key,left,right,value,...},x) = (
	    case keyCompare (key,x)
	     of GREATER => let
		  val (left', v) = remove(left, x)
		  in
		    (T'(key, value, left', right), v)
		  end
	      | LESS => let
		  val (right', v) = remove (right, x)
		  in
		    (T'(key, value, left, right'), v)
		  end
	      | _ => (delete'(left,right),value))
	| remove (a,x) = raise NotMap a

      exception FromList
      fun fromList L =
        let fun ins ((x, y), m) =
	      case find (m, x) of
		SOME y' => if compare (y, y') = EQUAL then m else raise FromList
	      | NONE    => insert (m, x, y)
        in List.foldl ins empty L
        end

      fun listItems d = let
	    fun d2l (FMAP_E, l) = l
	      | d2l (FMAP_T{key,value,left,right,...}, l) =
		  d2l(left, value::(d2l(right,l)))
	      | d2l (x,_) = raise NotMap x
	    in
	      d2l (d,[])
	    end

      fun listItemsi d = let
	    fun d2l (FMAP_E, l) = l
	      | d2l (FMAP_T{key,value,left,right,...}, l) =
		  d2l(left, (key,value)::(d2l(right,l)))
	      | d2l (x,_) = raise NotMap x
	    in
	      d2l (d,[])
	    end

      fun listKeys d = let
	    fun d2l (FMAP_E, l) = l
	      | d2l (FMAP_T{key,left,right,...}, l) = d2l(left, key::(d2l(right,l)))
	      | d2l (x,_) = raise NotMap x
	    in
	      d2l (d,[])
	    end

      fun appi f d = let
	    fun app' FMAP_E = ()
	      | app' (FMAP_T{key,value,left,right,...}) = (
		  app' left; f(key, value); app' right)
	      | app' x = raise NotMap x
	    in
	      app' d
	    end
      fun app f d = let
	    fun app' FMAP_E = ()
	      | app' (FMAP_T{value,left,right,...}) = (
		  app' left; f value; app' right)
	      | app' x = raise NotMap x
	    in
	      app' d
	    end

      fun mapi f d = let
	    fun map' FMAP_E = FMAP_E
	      | map' (FMAP_T{key,value,left,right,cnt}) = let
		  val left' = map' left
		  val value' = f(key, value)
		  val right' = map' right
		  in
		    FMAP_T{cnt=cnt, key=key, value=value', left = left', right = right'}
		  end
	      | map' x = raise NotMap x
	    in
	      map' d
	    end
      fun map f d = mapi (fn (_, x) => f x) d

      fun foldli f init d = let
	    fun fold (FMAP_E, v) = v
	      | fold (FMAP_T{key,value,left,right,...}, v) =
		  fold (right, f(key, value, fold(left, v)))
	      | fold (x,_) = raise NotMap x
	    in
	      fold (d, init)
	    end
      fun foldl f init d = foldli (fn (_, v, accum) => f (v, accum)) init d

      fun foldri f init d = let
	    fun fold (FMAP_E,v) = v
	      | fold (FMAP_T{key,value,left,right,...},v) =
		  fold (left, f(key, value, fold(right, v)))
	      | fold (x,_) = raise NotMap x
	    in
	      fold (d, init)
	    end
      fun foldr f init d = foldri (fn (_, v, accum) => f (v, accum)) init d
    end (* local *)

  (* the following are generic implementations of the unionWith and intersectWith
   * operetions.  These should be specialized for the internal representations
   * at some point.
   *)
    fun unionWith f (m1, m2) = let
	  fun ins  f (key, x, m) = (case find(m, key)
		 of NONE => insert(m, key, x)
		  | (SOME x') => insert(m, key, f(x, x'))
		(* end case *))
	  in
	    if (numItems m1 > numItems m2)
	      then foldli (ins (fn (a, b) => f (b, a))) m1 m2
	      else foldli (ins f) m2 m1
	  end
    fun unionWithi f (m1, m2) = let
	  fun ins f (key, x, m) = (case find(m, key)
		 of NONE => insert(m, key, x)
		  | (SOME x') => insert(m, key, f(key, x, x'))
		(* end case *))
	  in
	    if (numItems m1 > numItems m2)
	      then foldli (ins (fn (k, a, b) => f (k, b, a))) m1 m2
	      else foldli (ins f) m2 m1
	  end

    fun intersectWith f (m1, m2) = let
	(* iterate over the elements of m1, checking for membership in m2 *)
	  fun intersect f (m1, m2) = let
		fun ins (key, x, m) = (case find(m2, key)
		       of NONE => m
			| (SOME x') => insert(m, key, f(x, x'))
		      (* end case *))
		in
		  foldli ins empty m1
		end
	  in
	    if (numItems m1 > numItems m2)
	      then intersect f (m1, m2)
	      else intersect (fn (a, b) => f(b, a)) (m2, m1)
	  end
    fun intersectWithi f (m1, m2) = let
	(* iterate over the elements of m1, checking for membership in m2 *)
	  fun intersect f (m1, m2) = let
		fun ins (key, x, m) = (case find(m2, key)
		       of NONE => m
			| (SOME x') => insert(m, key, f(key, x, x'))
		      (* end case *))
		in
		  foldli ins empty m1
		end
	  in
	    if (numItems m1 > numItems m2)
	      then intersect f (m1, m2)
	      else intersect (fn (k, a, b) => f(k, b, a)) (m2, m1)
	  end

  (* this is a generic implementation of filter.  It should
   * be specialized to the data-structure at some point.
   *)
    fun filter predFn m = let
	  fun f (key, item, m) = if predFn item
		then insert(m, key, item)
		else m
	  in
	    foldli f empty m
	  end
    fun filteri predFn m = let
	  fun f (key, item, m) = if predFn(key, item)
		then insert(m, key, item)
		else m
	  in
	    foldli f empty m
	  end

  (* this is a generic implementation of mapPartial.  It should
   * be specialized to the data-structure at some point.
   *)
    fun mapPartial f m = let
	  fun g (key, item, m) = (case f item
		 of NONE => m
		  | (SOME item') => insert(m, key, item')
		(* end case *))
	  in
	    foldli g empty m
	  end
    fun mapPartiali f m = let
	  fun g (key, item, m) = (case f(key, item)
		 of NONE => m
		  | (SOME item') => insert(m, key, item')
		(* end case *))
	  in
	    foldli g empty m
	  end
  end (* structure Map *)



  (* --- default value of a given type --------------------------------- *)

  val defaultValueOfType :ASM_Type.TYPE -> VALUE =
    ASM_Type.inductive {
      RuleType  = UNDEF,          (* this case should actually never occur *)
      FuncType  = Misc.K UNDEF,   (* idem *)
      TypeParam = Misc.K UNDEF,   (* idem: it only makes sense for monomorphic type *)
      TypeVar   = Misc.K UNDEF,   (* same as above *)
      BaseType  =
        fn ("BOOL", _)      => BOOL false
         | ("_TUPLE", args) => TUPLE args
         | _                => UNDEF
    }


  (* --- auxiliary function for bit-wise operation on large integers --- *)

  structure Bitwise =
  struct
    open IntInf
    exception Bitwise of string

    fun toString radix msg x =
      if x < IntInf.fromInt 0
      then raise Bitwise "toHexString"
      else String.map Char.toUpper (fmt radix x)

    fun toHexString x = toString StringCvt.HEX "toHexString" x
    fun toBinString x = toString StringCvt.BIN "toBinString" x

    fun fromString radix msg s =
      case IntInf.scan StringCvt.HEX Substring.getc (Substring.full s) of
        SOME (x, y) => if Substring.string y = "" then x else raise Bitwise "fromHexString"
      | NONE => raise Bitwise "fromHexString"

    fun fromHexString x = fromString StringCvt.HEX "fromHexString" x
    fun fromBinString x = fromString StringCvt.BIN "fromBinString" x

    fun toBitVector x =
      (BitArray.fromString (toHexString x)) handle _ => raise Bitwise "toBitVector"

    fun fromBitVector x =
      (fromHexString (BitArray.toString x)) handle _ => raise Bitwise "fromBitVector"

    fun logic2 F (x, y) =
      let val (bvx, bvy) = (toBitVector x, toBitVector y)
      in fromBitVector (F (bvx, bvy, Int.max (BitArray.length bvx, BitArray.length bvy)))
      end

    val (andb, orb, xorb) = Triple.map logic2 (BitArray.andb, BitArray.orb, BitArray.xorb)
    fun notb x = fromBitVector (BitArray.notb (toBitVector x))

    fun lsh (x, i) = fromBitVector (BitArray.lshift (toBitVector x, IntInf.toInt i))
    fun rsh (x, i) = fromBitVector (BitArray.lshift (toBitVector x, IntInf.toInt i))

  end

  (* --- ML2ASM: convert ML data structures into ASM-WB representation --- *)

  structure ML2ASM =
  struct
    fun integer i =
      INT (IntInf.fromInt i)

    fun list []      = CELL ("nil", TUPLE [])
      | list (x::xs) = CELL ("::",  TUPLE [ x, list xs ])

    fun pair (x, y)  = TUPLE [x, y]
  end


  (* --- ASM2ML: convert ASM-WB data structures into ML representation --- *)

  structure ASM2ML =
  struct
    exception Error of VALUE * string

    fun integer (INT i) = ( (IntInf.toInt i)
			    handle _ => raise Error (INT i, "integer too large") )
      | integer x       = raise Error (x, "not an integer")

    fun list (CELL ("nil", TUPLE [])) = []
      | list (CELL ("::", TUPLE [ x, xs ])) = x :: (list xs)
      | list x = raise Error (x, "not a list")

    fun pair (TUPLE [x, y]) = (x, y)
      | pair x = raise Error (x, "not a pair")
  end

  (* --- Prim: primitive functions of ASM-WB --- *)

  structure Prim =
  struct
    exception Prim of string * VALUE list

    infixr 1 ::
    infix 1 ##
    infixr 5 @
    infix 4 == != < <= > >=
    infix 6 + -
    infix 7 * div mod

    val Undef = UNDEF

    val True  = BOOL true
    val False = BOOL false

    val Nil     = CELL ("nil", TUPLE [])

    fun (x::xs) =
      case xs of
        CELL ("nil", TUPLE [])   => CELL ("::", TUPLE [ x, xs ])
      | CELL ("::", TUPLE [_,_]) => CELL ("::", TUPLE [ x, xs ])
      | _                        => raise Prim ("::", [x,xs])

    fun Hd (CELL ("::", TUPLE [x,_])) = x
      | Hd (CELL ("nil", _)) = UNDEF
      | Hd x = raise Prim ("hd", [x])

    fun Tl (CELL ("::", TUPLE [_,xs])) = xs
      | Tl (CELL ("nil", _)) = UNDEF
      | Tl x = raise Prim ("tl", [x])

    fun Length x =
      let fun F (CELL ("::", TUPLE [_,xs])) = 1 + F xs
            | F (CELL ("nil", TUPLE [])) = 0
            | F _ = raise Prim ("length", [x])
      in INT (IntInf.fromInt (F x))
      end

    fun L1 @ L2 =
      let fun F (CELL ("nil", TUPLE [])) = L2
	    | F (CELL ("::", TUPLE [x,xs])) = CELL ("::", TUPLE [ x, F xs ])
	    | F _ = raise Prim ("append", [L1,L2])
      in case L2 of
           (CELL ("nil", TUPLE []))   => L1
         | (CELL ("::", TUPLE [_,_])) => F L1
         | _ => raise Prim ("append", [L1,L2])
      end

    fun Concat L =
      let fun F (L1 as CELL ("nil", TUPLE [])) = L1
	    | F (CELL ("::", TUPLE [x,xs])) = x @ (Concat xs)
	    | F _ = raise Prim ("concat", [L])
      in F L
      end

    fun interval (a, b, step) =
      List.tabulate ((b - a) div step +1, fn x => a + step * x)

    fun ListInterval (x1 as INT a, x2 as INT b, x3 as INT step) =
          ( ML2ASM.list (map ML2ASM.integer (interval (IntInf.toInt a, IntInf.toInt b, IntInf.toInt step)))
	    handle _ => raise Prim ("list_interval", [x1,x2,x3]) )
      | ListInterval (x1,x2,x3) = raise Prim ("list_interval", [x1,x2,x3])


    fun And (BOOL b1, BOOL b2) = BOOL (b1 andalso b2)
      | And (x1, x2) = raise Prim ("and", [x1,x2])

    fun Or (BOOL b1, BOOL b2) = BOOL (b1 orelse b2)
      | Or (x1, x2) = raise Prim ("or", [x1,x2])

    fun Implies (BOOL b1, BOOL b2) = BOOL ((not b1) orelse b2)
      | Implies (x1, x2) = raise Prim ("=>", [x1,x2])

    fun Not (BOOL b) = BOOL (not b)
      | Not (x) = raise Prim ("not", [x])


    (* !!!!!!!!!! sets, maps -> subset etc. !!!!!!!!! *)
    fun x == y = BOOL (compare (x, y) =  EQUAL)     (* equality of ASM values *)
    fun x != y = BOOL (compare (x, y) <> EQUAL)
    fun x <  y = BOOL (compare (x, y) =  LESS)
    fun x <= y = BOOL (compare (x, y) <> GREATER)
    fun x >  y = BOOL (compare (x, y) =  GREATER)
    fun x >= y = BOOL (compare (x, y) <> LESS)


    fun Andb (INT i1, INT i2) = INT (Bitwise.andb (i1, i2))
      | Andb (x1, x2) = raise Prim ("andb", [x1,x2])

    fun Orb (INT i1, INT i2) = INT (Bitwise.orb (i1, i2))
      | Orb (x1, x2) = raise Prim ("orb", [x1,x2])

    fun Xorb (INT i1, INT i2) = INT (Bitwise.xorb (i1, i2))
      | Xorb (x1, x2) = raise Prim ("xorb", [x1,x2])

    fun Notb (INT i) = INT (Bitwise.notb i)
      | Notb (x) = raise Prim ("notb", [x])

    fun Lsh (INT i1, INT i2) = INT (Bitwise.lsh (i1, i2))
      | Lsh (x1, x2) = raise Prim ("lsh", [x1,x2])

    fun Rsh (INT i1, INT i2) = INT (Bitwise.rsh (i1, i2))
      | Rsh (x1, x2) = raise Prim ("rsh", [x1,x2])

    fun HexToInt (STRING s) = INT (Bitwise.fromHexString s)
      | HexToInt x = raise Prim ("hex_to_int", [x])

    fun BinToInt (STRING s) = INT (Bitwise.fromBinString s)
      | BinToInt x = raise Prim ("bin_to_int", [x])

    fun IntToHex (INT i) = STRING (Bitwise.toHexString i)
      | IntToHex x = raise Prim ("int_to_hex", [x])

    fun IntToBin (INT i) = STRING (Bitwise.toBinString i)
      | IntToBin x = raise Prim ("int_to_bin", [x])

    fun Ord (STRING s) = ( ( INT (IntInf.fromInt (Char.ord (String.sub (s, 0)))) )
	                   handle _ => raise Prim ("ord", [ STRING s ]) )
      | Ord x = raise Prim ("ord", [x])

    fun Chr (INT i) = STRING (String.str (chr (IntInf.toInt i)))
      | Chr x = raise Prim ("chr", [x])

    fun (INT i1) + (INT i2) = INT (IntInf.+ (i1, i2))
      | x1 + x2 = raise Prim ("+", [x1,x2])

    fun (INT i1) - (INT i2) = INT (IntInf.- (i1, i2))
      | x1 - x2 = raise Prim ("-", [x1,x2])

    fun (INT i1) * (INT i2) = INT (IntInf.* (i1, i2))
      | x1 * x2 = raise Prim ("*", [x1,x2])

    val Mul = op *

    fun (INT i1) div (INT i2) = INT (IntInf.div (i1, i2))
      | x1 div x2 = raise Prim ("div", [x1,x2])

    fun (INT i1) mod (INT i2) = INT (IntInf.mod (i1, i2))
      | x1 mod x2 = raise Prim ("mod", [x1,x2])

    fun ~ (INT i) = INT (IntInf.~ i)
      | ~ (x) = raise Prim ("~", [x])


    fun Fadd (FLOAT r1, FLOAT r2) = FLOAT (Real.+ (r1, r2))
      | Fadd (x1,x2) = raise Prim ("fadd", [x1,x2])

    fun Fsub (FLOAT r1, FLOAT r2) = FLOAT (Real.- (r1, r2))
      | Fsub (x1,x2) = raise Prim ("fsub", [x1,x2])

    fun Fmul (FLOAT r1, FLOAT r2) = FLOAT (Real.* (r1, r2))
      | Fmul (x1,x2) = raise Prim ("fmul", [x1,x2])

    fun Fdiv (FLOAT r1, FLOAT r2) = FLOAT (Real./ (r1, r2))
      | Fdiv (x1,x2) = raise Prim ("fdiv", [x1,x2])

    fun Fneg (FLOAT r) = FLOAT (Real.~ (r))
      | Fneg x = raise Prim ("fneg", [x])


    fun (STRING s1) ## (STRING s2) = STRING (s1 ^ s2)
      | x1 ## x2 = raise Prim ("##", [x1,x2])

    fun Abs (INT i) = INT (IntInf.abs i)
      | Abs x = raise Prim ("Abs", [x])

    fun IntToString (INT i) = STRING (IntInf.fmt StringCvt.DEC i)
      | IntToString x = raise Prim ("int_to_string", [x])


    fun Floor (FLOAT r) = INT (IntInf.fromLarge (Real.toLargeInt IEEEReal.TO_NEGINF r))
      | Floor x = raise Prim ("floor", [x])

    fun Round (FLOAT r) = INT (IntInf.fromLarge (Real.toLargeInt IEEEReal.TO_NEGINF (Real.+ (r, 0.5))))
      | Round x = raise Prim ("round", [x])

    fun IntToFloat (INT i) = FLOAT (Real.fromLargeInt (IntInf.toLarge i))
      | IntToFloat x = raise Prim ("int_to_float", [x])


    fun Sqrt (FLOAT r) = FLOAT (Math.sqrt r)
      | Sqrt x = raise Prim ("sqrt", [x])

    fun Exp (FLOAT r) = FLOAT (Math.exp r)
      | Exp x = raise Prim ("exp", [x])

    fun Ln (FLOAT r) = FLOAT (Math.ln r)
      | Ln x = raise Prim ("ln", [x])

    fun Sin (FLOAT r) = FLOAT (Math.sin r)
      | Sin x = raise Prim ("sin", [x])

    fun Cos (FLOAT r) = FLOAT (Math.cos r)
      | Cos x = raise Prim ("cos", [x])

    fun Arctan (FLOAT r) = FLOAT (Math.atan r)
      | Arctan x = raise Prim ("arctan", [x])


    val Emptyset = Set.empty

    fun ListToSet L = ( (Set.fromList (ASM2ML.list L))
		        handle _ => raise Prim ("list_to_set", [L]) )


    fun SetInterval (x1 as INT a, x2 as INT b, x3 as INT step) =
          ( Set.fromList (map ML2ASM.integer (interval (IntInf.toInt a, IntInf.toInt b, IntInf.toInt step)))
            handle _ => raise Prim ("set_interval", [x1,x2,x3]) )
      | SetInterval (x1,x2,x3) = raise Prim ("set_interval", [x1,x2,x3])

    fun Member (x1 as UNDEF, x2) = raise Prim ("member", [x1,x2])
      | Member (x, s as FSET_E) = BOOL (Set.member (s, x))
      | Member (x, s as FSET_T _) = BOOL (Set.member (s, x))
      | Member (x1, x2) = raise Prim ("member", [x1,x2])

    fun Intersect (x1, x2) =
      if isFSet x1 andalso isFSet x2 then Set.intersection (x1, x2) else raise Prim ("intersect", [x1,x2])

    fun Union (x1, x2) =
      if isFSet x1 andalso isFSet x2 then Set.union (x1, x2) else raise Prim ("intersect", [x1,x2])

    fun \ (x1, x2) =
      if isFSet x1 andalso isFSet x2 then Set.difference (x1, x2) else raise Prim ("\\", [x1,x2])

    fun Card x =
      if isFSet x then INT (IntInf.fromInt (Set.numItems x)) else raise Prim ("card", [x])

    fun SetToList x =
      if isFSet x then ML2ASM.list (Set.listItems x) else raise Prim ("set_to_list", [x])

    fun ElementOf x =
      if isFSet x
      then case (Set.listItems x) of [x] => x | _ => raise Prim ("element_of", [x])
      else raise Prim ("element_of", [x])

    fun Prod (x1, x2) =
      let fun F  (x, S)  = Set.foldl (fn (y, S) => Set.add (S, TUPLE [x,y])) Set.empty S
          fun FF (S1, S2) = Set.foldl (fn (x, S) => Set.union (S, F (x, S2))) Set.empty S1
      in ( ( if isFSet x1 andalso isFSet x2 then FF (x1, x2) else raise Prim ("prod", [x1,x2]) )
           handle _ => raise Prim ("prod", [x1,x2]) )
      end

    fun UNION x =
      if isFSet x
      then ( Set.foldr Set.union Set.empty x ) handle _ => raise Prim ("Union", [x])
      else raise Prim ("Union", [x])

    fun INTERSECT x =
      if isFSet x
      then ( Set.foldr Set.intersection (Set.foldr Set.union Set.empty x) x )
	   handle _ => raise Prim ("Intersect", [x])
      else raise Prim ("Intersect", [x])


    val Emptymap = Map.empty

    fun ListToMap L =
      let fun convert (TUPLE [x,y]) = (x,y)
            | convert x = raise Prim ("list_to_map", [L])
      in (Map.fromList (map convert (ASM2ML.list L)))
	 handle Map.FromList => UNDEF
              | _ => raise Prim ("list_to_map", [L])
      end

    fun Apply (m, x) =
      if isFMap m
      then case Map.find (m, x) of SOME y => y | NONE => UNDEF
      else raise Prim ("apply", [m,x])

    fun MapCard m =
      if isFMap m
      then INT (IntInf.fromInt (Map.numItems m))
      else raise Prim ("map_card", [m])

    fun Domain m =
      if isFMap m
      then Set.fromList (map #1 (Map.listItemsi m))
      else raise Prim ("domain", [m])

    fun Range m =
      if isFMap m
      then Set.fromList (map #2 (Map.listItemsi m))
      else raise Prim ("range", [m])

    fun MapUnion (x1, x2) =
    ( let exception LocalExn
      in if isFMap x1 andalso isFMap x2
         then Map.unionWithi (fn (x, y1, y2) => raise LocalExn) (x1, x2)
         else raise Prim ("map_union", [x1,x2])
      end handle LocalExn => UNDEF )

    fun Override (x1, x2) =
      if isFMap x1 andalso isFMap x2
      then Map.unionWithi (fn (x, y1, y2) => y2) (x1, x2)
      else raise Prim ("override", [x1,x2])


    fun MapToSet x =
      if isFMap x
      then ( Set.fromList (map ML2ASM.pair (Map.listItemsi x)) )
	   handle _ => raise Prim ("map_to_set", [x])
      else raise Prim ("map_to_set", [x])

    fun MapToList x =
      if isFMap x
      then ( ML2ASM.list (map ML2ASM.pair (Map.listItemsi x)) )
	   handle _ => raise Prim ("map_to_list", [x])
      else raise Prim ("map_to_list", [x])

    fun SetToMap x =
      if isFSet x
      then ( Map.fromList (map ASM2ML.pair (Set.listItems x)) )
	   handle Map.FromList => UNDEF
                | _ => raise Prim ("set_to_map", [x])
      else raise Prim ("set_to_map", [x])
  end


  (* --- normalized values: eliminate 1-tuples, etc. --- *)

  fun normalized (TUPLE [x]) = normalized x
    | normalized (CELL (f, TUPLE [x])) = CELL (f, normalized x) 
    | normalized x = x


  (* --- toString: printable representation of VALUE --- *)

  (* !!! to do: proper handling of infix constructors !!! *)
  fun toString x =
    case x of
      UNDEF      => "undef"
    | BOOL true  => "true"
    | BOOL false => "false"
    | INT i      => IntInf.toString i
    | FLOAT r    => Real.toString r
    | STRING s   => "\"" ^ (String.toString s) ^ "\""
    | TUPLE xs   => tupleToString xs
    | CELL ("nil",t) => listToString (x, ("nil",t))
    | CELL ("::",t)  => listToString (x, ("::",t))
    | CELL (f, x)    => cellToString (f, x)
    | FSET_E     => setToString x
    | FSET_T _   => setToString x
    | FMAP_E     => mapToString x
    | FMAP_T _   => mapToString x

  and tupleToString xs =
    ListFormat.fmt { init = "(", final=")", sep=", ", fmt = toString } xs

  and listToString (x, (h,t)) =
    ( (ListFormat.fmt { init = "[ ", final=" ]", sep=", ", fmt = toString } (ASM2ML.list x))
      handle _ => cellToString (h,t) )

  and cellToString (f, TUPLE []) = f
    | cellToString (f, TUPLE xs) = f ^ (tupleToString xs)
    | cellToString (f, x) = f ^ " (" ^ (toString x) ^ ")"

  and setToString x =
  ( case Set.listItems x of
      [] => "emptyset"
    | xs => ListFormat.fmt { init = "{ ", final=" }", sep=", ", fmt = toString } xs )
  handle _ => "[ !!! ERROR: NOT A SET !!! ]"

  and mapToString x =
  ( let fun mapPairToString (x, y) = (toString x) ^ " -> " ^ (toString y)
    in case Map.listItemsi x of
	 [] => "emptymap"
       | xs => ListFormat.fmt { init = "{ ", final=" }", sep=", ", fmt = mapPairToString } xs
    end )
  handle _ => "[ !!! ERROR: NOT A MAP !!! ]"
end



signature ASM_ENV =
sig
  type VALUE = ASM_Value.VALUE
  datatype ENV = Success of VALUE StringMap.map | Failure

  val empty     :ENV
  val FAIL      :ENV
  val isFAIL    :ENV -> bool
  val singleton :string * VALUE -> ENV
  val union     :ENV * ENV -> ENV
  val Union     :ENV list -> ENV
  val override  :ENV * ENV -> ENV
  val find      :ENV * string -> VALUE option

  val filter    :(string * VALUE -> bool) -> ENV -> ENV
end


structure ASM_Env :ASM_ENV =
struct
  exception Clash of string

  type VALUE = ASM_Value.VALUE
  structure Map = StringMap

  datatype ENV = Success of VALUE Map.map | Failure

  val empty       = Success Map.empty
  fun singleton x = Success (Map.singleton x)
  val FAIL        = Failure
  val isFAIL      = fn Failure => true | _ => false

  fun union (Success m1, Success m2) =
	let exception Fail
	in (Success (Map.unionWithi (fn (x, y1, y2) => raise Fail) (m1, m2)))
	   handle Fail => Failure
	end
    | union (_, _) = Failure

  fun override (Success m1, Success m2) = Success (Map.unionWithi (fn (x, y1, y2) => y2) (m1, m2))
    | override _ = Failure

  fun Union envs = List.foldl union empty envs

  fun find (Success m, x) = Map.find (m, x)
    | find _ = NONE

  fun filter P (Success M) =
        Success (Map.foldli (fn (v,x,env') => if P (v,x) then Map.insert (env',v,x) else env') Map.empty M)
    | filter P (Failure) =
        Failure

  fun toString Failure = "<< FAIL >>"
    | toString (Success m) =
	let fun envPairToString (x, y) = x ^ " -> " ^ (ASM_Value.toString y)
	in ListFormat.fmt { init = "{ ", final=" }", sep=", ", fmt = envPairToString } (Map.listItemsi m)
	end
end
