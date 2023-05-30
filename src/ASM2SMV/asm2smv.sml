structure ASM2SMV =
struct
  open IL

  exception Impossible of string
  exception PossibleInconsistency of LOCATION
  fun impossible s = raise (Impossible s)

  val concat	= List.concat
  val eq	= ASM0.eq
  val (TRUE, FALSE, And) = (IL.TRUE, IL.FALSE, IL.And)
  infix 3 And
  infix 2 Or

  structure LocSet = struct
    structure Set = BinarySetFn ( struct
      type ord_key = LOCATION
      val compare  = Pair.compare (String.compare, ASM_Value.compare)
    end )
    open Set
    fun singleton x = add (empty, x)
    val fromList = List_.foldll add empty
    val UNION = List_.foldll union empty
  end

  structure ValSet = struct
    structure Set = BinarySetFn ( struct
      type ord_key = VALUE
      val compare  = ASM_Value.compare
    end )
    open Set
    fun singleton x = add (empty, x)
    val UNION    = List_.foldll union empty
    val fromList = List_.foldll add empty
  end

  structure LocMap = ASM0.LocPosMap

  fun same_value [] = NONE
    | same_value [ref (Val v)] = SOME v
    | same_value ((ref (Val v1))::t_rest) =
      ( case same_value t_rest of
	  NONE => NONE
	| SOME v => if equal_value (v, v1) then SOME v else NONE )
    | same_value _ = NONE

  fun try_else (x_opt :'a option) (if_some :'a -> 'b) (if_none :unit -> 'b) :'b =
    case x_opt of
      SOME x => if_some x
    | NONE   => if_none ()


  (* internal structure: used for computing by induction on the structure of rules *)
  type LOC_INFO' = {
    next    : PE_TERM,      (* next: rhs of the location's update *)
    changed : PE_TERM,	    (* condition under which the location has been updated *)
    cc      : PE_TERM	    (* consistency conditions for this location *)
  }

  (* loc. info to be exported: contains all information relevant for code generation *)
  type LOC_INFO = {
    init    : VALUE,	    (* initial value *)
    next    : PE_TERM,      (* next: rhs of the location's update *)
    cc      : PE_TERM,	    (* consistency conditions for this location *)
    loc_range : ValSet.set      (* 'real' loc. range: declared range of loc +
				           set of values which rhs of update can actually assume  *)
  }

  type TRANSITION_SYSTEM =
    LOC_INFO' LocMap.map

  val empty :TRANSITION_SYSTEM = 
    LocMap.empty

  fun domain (ts :TRANSITION_SYSTEM) :LocSet.set =
    LocMap.foldli (fn (loc, _, dom) => LocSet.add (dom, loc)) LocSet.empty ts

  fun value v    = ref (Val v)
  fun location l = ref (Loc l)


  (* --- collect all location occurring in term --- *)

  local
    fun collect (t :PE_TERM) :LocSet.set =
      case (!t) of
	Term t' => collect' t'
      | Loc l   => LocSet.singleton l
      | Val v   => LocSet.empty
      | Ctl (op_, t) => collect t
    and collect' t' =
      case t' of
	CondTerm G_t_list =>
	  LocSet.UNION (map (LocSet.union o (Misc.fnpair (collect, collect))) G_t_list)
      | AppTerm (f, ref (Term (TupleTerm t_list))) =>
          LocSet.UNION (map collect t_list)
      | AppTerm (f, t) => 
          collect t
      | _ => impossible "collect_locations_in_term"
  in
    val collect_locations_in_term = collect
  end


  fun simplify_cond_term G_t_list =
    IL.simplify_cond_term (map (fn (G, t) => (fn _ => G, fn _ => t)) G_t_list) ASM_Env.empty

  fun cond_term G_t_list =
    case simplify_cond_term G_t_list of
      t as ref (Term (CondTerm G_t_list)) =>
        try_else (same_value (map #2 G_t_list)) (fn v => value v) (fn _ => t)
    | t as _ => t

  fun single_update (l :LOCATION, v :VALUE) :TRANSITION_SYSTEM =
    LocMap.insert (empty, l, { next = cond_term [(TRUE, value v)], changed = TRUE, cc = TRUE })

  fun apply (ts :TRANSITION_SYSTEM) (l :LOCATION) = 
    case LocMap.find (ts, l) of
      SOME info => info
    | NONE	=> { next = location l, changed = FALSE, cc = TRUE }

  fun ts_update_rule (ref (Loc l), ref (Val v)) (ts' :TRANSITION_SYSTEM) = single_update (l, v)
    | ts_update_rule _ ts = impossible "ts_rule: UpdateRule"

  fun ts_cond_rule (G_ts_list :(PE_TERM * (TRANSITION_SYSTEM -> TRANSITION_SYSTEM)) list)
		   (ts' :TRANSITION_SYSTEM) =
    let val G_ts_list = map (fn (G_i, ts_i) => (G_i, (ts_i ts'))) G_ts_list
	val locations = LocSet.UNION (map (fn (G, ts_i') => domain ts_i') G_ts_list)
        val additional_locations =   (* location which do not occur in any ts_i, but in some condition,
				          and are not external locations *)
              LocSet.filter
                ( fn (f, x) => IL.function_kind (Id f) <> ASM_AST.External )
		( LocSet.difference
		  ( (LocSet.UNION (map (fn (G,_) => collect_locations_in_term G) G_ts_list)),
		    locations ) )
	val appts' = apply ts'
	fun one_location (l :LOCATION) :LOC_INFO' =
	  { next =
              cond_term ( (map (fn (G_i, ts_i) => (G_i, #next (apply ts_i l))) G_ts_list)
			  @ [ (TRUE, #next (appts' l)) ] ),
            changed =
	      OR (map (fn (G_i, ts_i) => G_i And (#changed (apply ts_i l))) G_ts_list),
            cc =
	      cond_term ( (map (fn (G_i, ts_i) => (G_i, #cc (apply ts_i l))) G_ts_list)
			  @ [ (TRUE, #cc (appts' l)) ] ) }
        fun one_additional_location (l :LOCATION) :LOC_INFO' =
          { next = ref (Loc l), changed = FALSE, cc = TRUE }
    in LocMap.override (
         LocSet.foldl (fn (l, ts) => LocMap.insert (ts, l, one_location l))
                      empty locations,
         LocSet.foldl (fn (l, ts) => LocMap.insert (ts, l, one_additional_location l))
		      empty additional_locations
       )
    end

  infix **
  fun (dts1 ** dts2) ts = LocMap.override ( dts2 ts, dts1 (dts2 ts) )

  fun ts_block_rule (ts_list :(TRANSITION_SYSTEM -> TRANSITION_SYSTEM) list)
		    (ts' :TRANSITION_SYSTEM) =
    let val locations = LocSet.UNION (map (fn ts_i => domain (ts_i ts')) ts_list)
	fun block2 (ts1, ts2) (ts_cont :TRANSITION_SYSTEM) :TRANSITION_SYSTEM =
	  let val (app_ts1', app_ts2') = (apply (ts1 ts'), apply (ts2 ts'))
	      val app_ts12cont = apply ((ts1 ** ts2) ts_cont)
	      fun one_location l = 
	        let val { changed = changed1, cc = cc1, ... } = app_ts1' l
		    val { changed = changed2, cc = cc2, ... } = app_ts2' l
                in { next    = #next (app_ts12cont l),
		     changed = changed1 Or changed2,
		     cc	     = cc1 And cc2 And (Not changed1 Or Not changed2) }
                end
          in LocSet.foldl (fn (l, ts) => LocMap.insert (ts, l, one_location l)) empty locations
          end
    in (List.foldr block2 Misc.id ts_list) ts'
    end

  structure TS'_Spec = struct
    structure S = struct
      type NAME = ASM_AST.NAME
      type PATT = ASM_AST.PATT
      type TERM = PE_TERM
      type RULE = PE_RULE
    end
    structure T = struct
      type NAME = ASM_AST.NAME
      type PATT = ASM_AST.PATT
      type TERM = PE_TERM
      type RULE' = TRANSITION_SYSTEM -> TRANSITION_SYSTEM
      type RULE  = TRANSITION_SYSTEM -> TRANSITION_SYSTEM
    end
    val h_name = Misc.id
    val h_patt = Misc.id
    val h_term = Misc.id

    fun h_rule h_rule' (ref (Rule R')) ts = h_rule' R' ts

    fun UpdateRule ((f,ref (Val x)), t_r) ts = ts_update_rule (ref (Loc (idStr f,x)), t_r) ts
      | UpdateRule ((f,_), t_r) ts           = impossible "ts_rule: UpdateRule" ts

    fun BlockRule Rs ts = ts_block_rule Rs ts
    fun CondRule GRs ts = ts_cond_rule GRs ts

    fun ImportRule _ _  = impossible "ts_rule: ImportRule"
    fun ChooseRule _ _  = impossible "ts_rule: ChooseRule"
    fun ForallRule _ _  = impossible "ts_rule: VarRule"
    fun LetRule _ _     = impossible "ts_rule: LetRule"
    fun CaseRule _ _    = impossible "ts_rule: CaseRule"
    fun AppRule _ _     = impossible "ts_rule: AppRule"
  end

  structure TS = RuleInduction (TS'_Spec)
  val ts' = TS.rule

  fun ts R = ts' R empty


  (* --- collect values (only values which can be assigned, e.g. not those in guards!) --- *)

  local
    fun collect (t :PE_TERM) :ValSet.set =
      case (!t) of
	Term t' => collect' t'
      | Loc l   => ValSet.empty
      | Val v   => ValSet.singleton v
      | Ctl (op_, t) => collect t
    and collect' t' =
      case t' of
	CondTerm G_t_list =>
	  ValSet.UNION (map (collect o #2) G_t_list)
      | AppTerm (f, ref (Term (TupleTerm t_list))) =>
          ValSet.empty       (* empty, as app. terms should actually occur only in guards !!! *)
      | AppTerm (f, t) => 
          ValSet.empty	      (* empty, as app. terms should actually occur only in guards !!! *)
      | _ => impossible "collect_locations_in_term"
  in
    val range_of_rhs = collect
  end


  (* --- build complete information about transition system --- *)

  fun functionKind f = valOf (ASM_Signature.functionKind (ASM_Signature.find (!ASM_Top.sign) f))

  fun some (SOME x) = x
    | some _ = impossible "some"

  exception LocInit
  fun loc_init ((s, x) :LOCATION) :VALUE =
    (ASM_Top.initState () (Id s) x)

  fun transition_system (R :PE_RULE) :(LOCATION * LOC_INFO) list =
    let fun F (loc as (s, _) :LOCATION, { next = next, changed = changed, cc = cc } :LOC_INFO') =
          let val init = loc_init loc
	      val next = simplify_term next
	  in { init = init,
	       next = next,
	       cc = (*ASM0.unfold_term*) cc,
	       loc_range = if (functionKind s = ASM_AST.External)
                           then (* for external location: specified range according to finiteness constraints *)
                                ValSet.fromList (ASM0.loc_range loc)
                           else (* for dynamic location: only the values it can actually assume, i.e. initial value or any value occurring on rhs *)
                                ValSet.union (ValSet.singleton init, range_of_rhs next) }
          end
        val trsys = ts R
    in map (fn (loc, info) => (loc :LOCATION, F (loc, info))) (LocMap.listItemsi trsys)
    end

end
