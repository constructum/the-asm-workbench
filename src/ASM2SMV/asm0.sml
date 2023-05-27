(*
##
## ASM2SMV - G. Del Castillo, K. Winter, 1999
##
## "asm0.ml", transformation ASM -> ASM0
##
##
##   To do: if all branches in the generated case distinction
##     evaluate to the same value, simplify to that value
##     (important to discard expressions such as: x = 1 and x = 2)
##
*)

(* ---------------------------------------------------------------- *)
(* ---------------------------------------------------------------- *)

structure ASM0 =
struct
  exception NotImplemented of string
  exception Impossible of string
  fun impossible s = raise (Impossible s)

  open IL
  val (split, filter) = (ListPair.unzip, List.filter)
  fun concatMap f = List.concat o (map f)
  fun comprehension f L cond = concatMap (fn x => if cond x then [f x] else []) L
  fun map2 f (x, y) = (f x, f y)
  val sum = List.foldl (op +) 0

  datatype POSITION =
    InTerm of PE_TERM
  | InRule of PE_RULE

  structure LocDict = BinaryMapFn ( struct
    type ord_key = LOCATION
    val compare  = Pair.compare (String.compare, ASM_Value.compare)
  end )
 
  structure LocPosMap = struct
    open LocDict
    fun override (m1, m2) = unionWith #2 (m1, m2)
    fun Override ms = List.foldl override empty ms
    fun fromList L  = List.foldl insert' empty L
  end
  type LOC_POS_MAP = POSITION LocPosMap.map

  structure LocValMap = LocPosMap
  type LOC_ENV = ASM_Value.VALUE LocValMap.map


  (* --- messages to the user --- *)

  local open TextIO
  in
    val verbose = false
    fun outerr s = output (stdErr, s)
    fun expanding_message plm =
      if verbose
      then ( output (stdErr, "Expanding locations:\n");
	     map (fn (_,loc) => ( outerr "  ";
				  Pretty.pr stdErr 1000 (IL_Pretty.pe_term (ref (Loc loc)) ))) plm; () )
      else ()
    fun expanding_term_message t =
      if verbose
      then ( output (stdErr, "Expanding term:\n");
	     Pretty.pr stdErr 1000 (IL_Pretty.pe_term t); () )
      else ()
    fun expanding_rule_message R =
      if verbose
      then ( output (stdErr, "Expanding rule:\n");
	     Pretty.pr stdErr 1000 (IL_Pretty.pe_rule R); () )
      else ()
  end

  (* --- find out locations to expand and their "expand positions" (sub-ASTs where expansion takes place)  --- *)

  local
    val empty = LocPosMap.empty
    val singleton = LocPosMap.singleton
    fun inTerm (t :PE_TERM) (loc :LOCATION) :LOCATION * POSITION = (loc, InTerm t)
    fun inRule (R :PE_RULE) (loc :LOCATION) :LOCATION * POSITION = (loc, InRule R)
    fun contains (table :LOC_POS_MAP) (loc :LOCATION) = (LocPosMap.find (table, loc); 1) handle _ => 0
    fun domain (table :LOC_POS_MAP) :LOCATION list = map #1 (LocPosMap.listItemsi table)
    fun domains_union (tables :LOC_POS_MAP list) :LOCATION list = domain (LocPosMap.Override tables)

    infix ++
    fun (m1 :LOC_POS_MAP) ++ (m2 :LOC_POS_MAP) = LocPosMap.override (m1, m2)

    fun more_references m_list loc = sum (map (fn m => contains m loc) m_list) > 1
  in 
    fun update_loc_pos_map inTermOrInRule self m_list_to_be_expanded condition_to_expand_here  =
      let val locations = domains_union m_list_to_be_expanded
          val locations_to_expand_here = comprehension (inTermOrInRule self) locations condition_to_expand_here
      in (LocPosMap.Override m_list_to_be_expanded) ++ (LocPosMap.fromList locations_to_expand_here)
      end

    fun lep_in_app_term lep t_self (f, t) =
      update_loc_pos_map inTerm t_self [ lep t ] (fn loc => true)
(*
      case t_self of 
        ref (Term (AppTerm' (Symbol_A "=", ref (Term (TupleTerm' [ t1, t2 ]))))) =>
	( case (t1, t2) of
	    (ref (Loc l1), ref (Loc l2)) => empty
          | (ref (Loc l1), _) => update_loc_pos_map inTerm t_self [ lep t2 ] (fn loc => true)
	  | (_, ref (Loc l2)) => update_loc_pos_map inTerm t_self [ lep t1 ] (fn loc => true)
	  | _ => update_loc_pos_map inTerm t_self [ lep t1, lep t2 ] (fn loc => true) )
      | _ => update_loc_pos_map inTerm t_self [ lep t ] (fn loc => true)
*)

    fun lep_in_tuple_term lep t_self t_list =
      update_loc_pos_map inTerm t_self (map lep t_list) (fn loc => true) (*(more_references t_list)*)

    fun lep_in_set_enum_term lep t_self t_list =
      update_loc_pos_map inTerm t_self (map lep t_list) (fn loc => true) (*(more_references t_list)*)

    fun lep_in_map_enum_term lep t_self t_list =
      let fun lep_pair (t, t') = [ lep t, lep t' ]
      in update_loc_pos_map inTerm t_self (concatMap lep_pair t_list) (fn loc => true) (*(more_references t_list)*)
      end

    fun lep_in_cond_term lep t_self G_t_list =
      let val (G_list_, t_list_) = split (map (map2 lep) G_t_list)
      in update_loc_pos_map inTerm t_self G_list_ (fn loc => true)
      end

    fun lep_in_cond_rule (lep_t, lep_R) R_self G_R_list =
      let val (G_list_, R_list_) = split (map (fn (G, R) => (lep_t G, lep_R R)) G_R_list)
      in update_loc_pos_map inRule R_self G_list_ (fn loc => true)  (*(more_references (G_list_ @ R_list_))*)
      end

    fun lep_in_let_term lep t_self (p, t1, t2) =
      update_loc_pos_map inTerm t_self [ lep t1, lep t2 ] (fn loc => true)

    fun lep_in_let_rule (lep_t, lep_R) R_self (p, t, R) =
      let val (t_, R_) = (lep_t t, lep_R R)
      in update_loc_pos_map inRule R_self [ t_ ] (fn loc => true) (*(more_references [ t_, R_ ]))*)
      end
      (* alternatively:  update_loc_pos_map inRule t_self [ lep_t t, lep_R R ] (fn loc => true) *)

    fun lep_in_case_term lep t_self (t, p_t_list) =
      update_loc_pos_map inTerm t_self [ lep t ] (fn loc => true)

    fun lep_in_case_rule (lep_t, lep_R) R_self (t, p_R_list) =
      let val (t_, R_list_) = (lep_t t, map (fn (p, R) => lep_R R) p_R_list)
      in update_loc_pos_map inRule R_self [ t_ ]
           (fn loc => (contains t_ loc > 0) orelse (more_references R_list_ loc))
      end

    fun lep_in_comprehension lep t_self (p, A, G) = 
      update_loc_pos_map inTerm t_self [ lep A ] (fn loc => true)

    fun lep_in_var_rule (lep_t, lep_R) R_self (p, A, G, R) = 
      let val (A_, G_, R_) = (lep_t A, lep_t G, lep_R R)
      in update_loc_pos_map inRule R_self [ A_ ] (more_references [ A_, G_, R_ ])
      end

    fun lep_in_block_rule (_, lep_R) R_self R_list =
      let val R_list_ = map lep_R R_list
      in update_loc_pos_map inRule R_self R_list_ (more_references R_list_)
      end

    fun lep_in_update_rule (lep_t, lep_R) R_self ((f,t), t_r) =
      update_loc_pos_map inRule R_self [ lep_t t, lep_t t_r ] (fn loc => true)
(*
      (* !!!!!!!!!!! ??????? *)
      case t_l of
        ref (Loc loc) => update_loc_pos_map inRule R_self [ lep_t t_r ] (fn loc => true)
      | _ => update_loc_pos_map inRule R_self [ lep_t t_l, lep_t t_r ] (fn loc => true)
*)


    local
      open ASM_AST
    in
      fun loc_pos_map_in_term (t :PE_TERM) :LOC_POS_MAP =
	case t of
	  ref (Term t') => loc_pos_map_in_term' t t'
	| ref (Val v)   => empty
	| ref (Loc l)   => singleton (inTerm t l)
	| ref (Ctl (op_, t)) => empty

      and loc_pos_map_in_term' (t_self :PE_TERM) (t' :(TYPE, NAME, PATT, PE_TERM) TERM') :LOC_POS_MAP =
	let val lep = loc_pos_map_in_term
	in case t' of
	     TermType (ty, t)  => lep t
	   | AppTerm (f, t)    => lep_in_app_term lep t_self (f, t)
	   | TupleTerm t_list  => lep_in_tuple_term lep t_self t_list
	   | VarTerm _         => empty
	   | CondTerm G_t_list => lep_in_cond_term lep t_self G_t_list
	   | LetTerm (p, t1, t2) => lep_in_let_term lep t_self (p, t1, t2)
	   | CaseTerm (t, p_t_list) => lep_in_case_term lep t_self (t, p_t_list)
	   | ListComprTerm (_, (p, A, G)) => lep_in_comprehension lep t_self (p, A, G)
	   | FSetComprTerm (_, (p, A, G)) => lep_in_comprehension lep t_self (p, A, G) 
	   | FMapComprTerm (_, (p, A, G)) => lep_in_comprehension lep t_self (p, A, G)
	   | FSetEnumTerm t_list  => lep_in_set_enum_term lep t_self t_list
	   | FMapEnumTerm t_list  => lep_in_map_enum_term lep t_self t_list
	   | ExistsTerm (p, A, G) => lep_in_comprehension lep t_self (p, A, G)
	   | ForallTerm (p, A, G) => lep_in_comprehension lep t_self (p, A, G)
	end

      fun loc_pos_map_in_rule (R as (ref (Rule R')) :PE_RULE) :LOC_POS_MAP =
	loc_pos_map_in_rule' R R'

      and loc_pos_map_in_rule' (R_self :PE_RULE) (R' :(NAME, PATT, PE_TERM, PE_RULE) RULE') :LOC_POS_MAP =
	let val lep = (loc_pos_map_in_term, loc_pos_map_in_rule)
	in case R' of
	     UpdateRule ((f,t), t_r) => lep_in_update_rule lep R_self ((f,t), t_r) 
	   | BlockRule R_list	     => lep_in_block_rule lep R_self R_list
	   | ForallRule ((p,A,G),R)  => lep_in_var_rule lep R_self (p, A, G, R)
	   | CondRule G_R_list	     => lep_in_cond_rule lep R_self G_R_list
	   | CaseRule (t, p_R_list)  => lep_in_case_rule lep R_self (t, p_R_list)
	   | LetRule (p, t1, R)      => lep_in_let_rule lep R_self (p, t1, R)
	   | ChooseRule _	     => raise NotImplemented "loc_pos_map_in_rule: ChooseRule"
	   | AppRule _		     => raise NotImplemented "loc_pos_map_in_rule: AppRule"
				      (* actually the AppRule's should be already expanded ! *)
	end
    end
  end


  (* --- find out range of locations: used to expand locations --- *)

  exception LocRange of LOCATION
  exception EmptyLocationRange of LOCATION

  fun compute_loc_range (loc as (f, x) :LOCATION) :VALUE list =
  ( let open ASM_Dictionary
        val finitenessConstraint = 
        ( ( case findFunction (!ASM_Top.dict, f) of
	      SOME (_, ASM_SL_DynamicFunction {constraint,...}) => valOf (#RangeConstraint constraint)
	    | SOME (_, ASM_SL_ExternalFunction {constraint})    => valOf (#RangeConstraint constraint)
	    | _ => raise LocRange (f, x) ) handle Option => raise LocRange (f, x) )
        val range = ASM_Eval.functionExpr finitenessConstraint (ASM_Top.initState ()) x
    in ASM_Value.Set.listItems range
    end handle _ => raise LocRange (f, x) )

  val loc_range_table = ref (LocDict.empty :VALUE list LocDict.map)
  fun loc_range (loc as (f, x) :LOCATION) :VALUE list =
  ( case LocDict.find (!loc_range_table, loc) of
      SOME x => x
    | NONE => let val range = compute_loc_range (f, x)
	      in loc_range_table := LocDict.insert (!loc_range_table, loc, range);
		 range
	      end )

  fun non_empty_loc_range loc =
    case (loc_range loc) of
      []    => raise (EmptyLocationRange loc)
    | range => range


  (* --- expand locations --- *)

  local
    type POS_LOC_MAP = (POSITION * LOCATION) list
    infix ++
    fun (env1 :LOC_ENV) ++ (env2 :LOC_ENV) = LocValMap.override (env1, env2)
    val empty = LocValMap.empty
    val singleton = LocValMap.singleton
  in
    fun locations_to_expand (plm :POS_LOC_MAP, pos0 :POSITION) :LOCATION list =
      comprehension (fn (_,loc) => loc) plm (fn (pos,_) => pos = pos0)

    fun eq (t1 :PE_TERM, t2 :PE_TERM) :PE_TERM =
      ref (Term (ASM_AST.AppTerm (Id "=", ref (Term (ASM_AST.TupleTerm [ t1, t2 ])))))


    fun expand_locations_in_term
           (expand_locations_in_term' : (ASM_AST.TYPE, ASM_AST.NAME, ASM_AST.PATT, PE_TERM' ref) ASM_AST.TERM'
	                                -> (POS_LOC_MAP -> LOC_ENV -> PE_TERM))
           (plm :POS_LOC_MAP) (t :PE_TERM) (env :LOC_ENV) :PE_TERM =
      let fun no_expansion t env =
            case t of
              ref (Term t') => expand_locations_in_term' t' plm env
            | ref (Val v)   => ref (Val v)
            | ref (Loc l)   => ( (ref (Val (valOf (LocValMap.find (env, l))))) handle _ => t )
            | ref (Ctl (op_, t)) =>
                ref (Ctl (op_, expand_locations_in_term expand_locations_in_term'
                                 ([] :POS_LOC_MAP) t LocValMap.empty))
          fun case_distinction [] t env =
	        no_expansion t env
            | case_distinction (loc1 :: loc_rest) t env =
	        let val G_t_list =
		      map ( fn v =>
			( eq (ref (Loc loc1), ref (Val v)),
			  unfold_term
			    expand_locations_in_term'
                            (case_distinction loc_rest t (env ++ singleton (loc1, v))) ) )
		      (non_empty_loc_range loc1)
                    fun same_value [] = NONE
		      | same_value [(_,ref (Val v))] = SOME v
		      | same_value ((_,ref (Val v1))::G_t_rest) =
		        ( case same_value G_t_rest of
			    NONE => NONE
                          | SOME v => if equal_value (v, v1) then SOME v else NONE )
                      | same_value _ = NONE
                in case same_value G_t_list of
		     NONE => ref (Term (ASM_AST.CondTerm G_t_list))
		   | SOME v => ref (Val v)
                end
          val locs = locations_to_expand (plm, InTerm t)
      in case_distinction locs t env
      end

    and unfold_term
           (expand_locations_in_term' : (ASM_AST.TYPE, ASM_AST.NAME, ASM_AST.PATT, PE_TERM' ref) ASM_AST.TERM'
	                                -> (POS_LOC_MAP -> LOC_ENV -> PE_TERM))
            t0 =
      let val t   = simplify_term t0
	  val lpm = loc_pos_map_in_term t
	  val plm = map (fn (x,y) => (y,x)) (LocPosMap.listItemsi lpm)
	  val _	  = expanding_term_message t
      in if length plm > 0
	 then ( expanding_message plm;
		expand_locations_in_term expand_locations_in_term' plm t (LocValMap.empty) )
	 else t
      end


    fun expand_locations_in_rule
           (expand_locations_in_rule' : (ASM_AST.NAME, ASM_AST.PATT, PE_TERM, PE_RULE' ref) ASM_AST.RULE'
	                                -> (POS_LOC_MAP -> LOC_ENV -> PE_RULE))
           (plm :POS_LOC_MAP) (R :PE_RULE) (env :LOC_ENV) :PE_RULE =
      let fun no_expansion (ref (Rule R')) env =
            expand_locations_in_rule' R' plm env
          fun case_distinction [] R env =
	        no_expansion R env
            | case_distinction (loc1 :: loc_rest) R env =
                ref (Rule (ASM_AST.CondRule (
                  map
                  ( fn v =>
                    ( eq (ref (Loc loc1), ref (Val v)),
                      unfold_rule
                        expand_locations_in_rule'
                        (case_distinction loc_rest R (env ++ singleton (loc1, v))) ) )
                  (non_empty_loc_range loc1) )))
          val locs = locations_to_expand (plm, InRule R)
      in case_distinction locs R env
      end

    and unfold_rule
	  (expand_locations_in_rule' : (ASM_AST.NAME, ASM_AST.PATT, PE_TERM, PE_RULE' ref) ASM_AST.RULE'
				       -> (POS_LOC_MAP -> LOC_ENV -> PE_RULE))
          R0 =
      let val R   = simplify_rule R0
	  val lpm = loc_pos_map_in_rule R
	  val plm = map (fn (x,y) => (y,x)) (LocPosMap.listItemsi lpm)
	  val _	  = expanding_rule_message R
      in if length plm > 0
	 then ( expanding_message plm;
		expand_locations_in_rule expand_locations_in_rule' plm R (LocValMap.empty) )
	 else R
      end



    (* --- common structure for term and rule expander --- *)

    structure CommonExpandSpec = struct
      structure S = struct
	type TYPE = ASM_AST.TYPE
	type NAME = ASM_AST.NAME
	type PATT = ASM_AST.PATT
	type TERM = PE_TERM
	type RULE = PE_RULE
      end
      structure T = struct
	type TYPE = ASM_AST.TYPE
	type NAME = ASM_AST.NAME
	type PATT = ASM_AST.PATT
	type TERM  = POS_LOC_MAP -> LOC_ENV -> PE_TERM
	type TERM' = POS_LOC_MAP -> LOC_ENV -> PE_TERM
	type RULE  = POS_LOC_MAP -> LOC_ENV -> PE_RULE
	type RULE' = POS_LOC_MAP -> LOC_ENV -> PE_RULE
      end
      val h_type = Misc.id
      val h_name = Misc.id
      val h_patt = Misc.id
    end


    (* --- term expander (inductive) --- *)

    structure ExpandTermSpec = struct
      open CommonExpandSpec

      fun h_term h_term' t plm env =
	expand_locations_in_term h_term' plm t env

      fun TermType (T, t) = t

      fun T t' = ref (Term t')

      fun VarTerm s plm env = T (ASM_AST.VarTerm s)
      fun LetTerm (p, t1, t2) plm env = T (ASM_AST.LetTerm (p, t1 plm env, t2 plm env))

      fun AppTerm (f, t :T.TERM) plm env =
        let val ctl_op = case f of Id "AG" => true | Id "AF" => true | Id "AX" => true
                                 | Id "EG" => true | Id "EF" => true | Id "EX" => true
                                 | _ => false
        in if not ctl_op
           then T (ASM_AST.AppTerm (f, t plm env))
           else T (ASM_AST.AppTerm (f, t ([] :POS_LOC_MAP) LocValMap.empty))
                (* T (ASM_AST.AppTerm (f, t ([] :POS_LOC_MAP) LocValMap.empty)) *)
        end


      fun TupleTerm ts plm env   = T (ASM_AST.TupleTerm (map (fn t => t plm env) ts))
      fun CondTerm Gts plm env   = T (ASM_AST.CondTerm (map (fn (G,t) => (G plm env, t plm env)) Gts))
      fun CaseTerm (t,pts) plm env   = T (ASM_AST.CaseTerm (t plm env, map (fn (p,t) => (p, t plm env)) pts))
      fun FSetEnumTerm ts plm env    = T (ASM_AST.FSetEnumTerm (map (fn t => t plm env) ts))
      fun FMapEnumTerm ts plm env    = T (ASM_AST.FMapEnumTerm (map (map2 (fn t => t plm env)) ts))
      fun ForallTerm (p,A,G) plm env = T (ASM_AST.ForallTerm (p, A plm env, G plm env))
      fun ExistsTerm (p,A,G) plm env = T (ASM_AST.ExistsTerm (p, A plm env, G plm env))
      fun ListComprTerm (t,(p,A,G)) plm env = T (ASM_AST.ListComprTerm (t plm env, (p, A plm env, G plm env)))
      fun FSetComprTerm (t,(p,A,G)) plm env = T (ASM_AST.FSetComprTerm (t plm env, (p, A plm env, G plm env)))

      fun FMapComprTerm ((t,t'),(p,A,G)) plm env =
	T (ASM_AST.FMapComprTerm ((t plm env, t' plm env), (p, A plm env, G plm env)))
    end

    structure ExpandTerm = TermInduction (ExpandTermSpec)


    (* --- rule expander (inductive) --- *)

    structure ExpandRuleSpec = struct
      open CommonExpandSpec

      val h_term = ExpandTerm.term

      fun h_rule h_rule' R plm env =
	expand_locations_in_rule h_rule' plm R env

      fun R R' = ref (Rule R')

      fun UpdateRule ((f, t), t') plm env = R (ASM_AST.UpdateRule ((f, t plm env), t' plm env))

      fun BlockRule Rs plm env = R (ASM_AST.BlockRule (map (fn R => R plm env) Rs))

      fun LetRule (p, R1, R2) plm env = R (ASM_AST.LetRule (p, R1 plm env, R2 plm env))
      fun CondRule GRs plm env        = R (ASM_AST.CondRule (map (fn (G,R) => (G plm env, R plm env)) GRs))
      fun CaseRule (t,pRs) plm env    = R (ASM_AST.CaseRule (t plm env, map (fn (p,R) => (p, R plm env)) pRs))

      fun ForallRule ((p,A,G),R') plm env = R (ASM_AST.ForallRule ((p, A plm env, G plm env), R' plm env))

      fun ChooseRule _ _ _ = raise NotImplemented "ExpandRule: ChooseRule"
      fun AppRule _ _ _    = raise NotImplemented "ExpandRule: AppRule"     (* should never occur  ! *)
    end

    structure ExpandRule = RuleInduction (ExpandRuleSpec)
  end

  val unfold_term = simplify_term o (unfold_term ExpandTerm.term')
  val unfold_rule = simplify_rule o (unfold_rule ExpandRule.rule')

  fun init () = ( loc_range_table := LocDict.empty )
end
