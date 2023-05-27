(*
##
## ASM2SMV - G. Del Castillo, K. Winter, 1999
##
## "il.ml", intermediate language
##
##   Includes:
##     partially evaluated terms and rules
##
##   To do: try to improve simplifier by introducing symbolic pattern
##     matching (match patterns against PE_TERMs, not only values)
##
*)

(* ---------------------------------------------------------------- *)
(* ---------------------------------------------------------------- *)


signature IL =
sig
  exception NotImplemented of string

  type VALUE
  type LOCATION

  val Id :string -> ASM_AST.NAME
  val equal_value :ASM_Value.VALUE * ASM_Value.VALUE -> bool

  datatype PE_TERM' =
    Val of VALUE |
    Loc of LOCATION |
    Ctl of string * PE_TERM' ref |
    Term of (ASM_AST.TYPE, ASM_AST.NAME, ASM_AST.PATT, PE_TERM' ref) ASM_AST.TERM'

  type PE_TERM

  datatype PE_RULE' =
    Rule of (ASM_AST.NAME, ASM_AST.PATT, PE_TERM, PE_RULE' ref) ASM_AST.RULE'

  type PE_RULE

  val TRUE  :PE_TERM
  val FALSE :PE_TERM
  val And   :PE_TERM * PE_TERM -> PE_TERM

  val pe_term :ASM_AST.TERM -> PE_TERM
  val pe_rule :ASM_AST.RULE -> PE_RULE

  type ENV = ASM_Env.ENV
  val simplify_cond :((ENV -> PE_TERM) * (ENV -> PE_TERM)) list -> (ENV -> PE_TERM)
  
  val simplify_term :PE_TERM -> PE_TERM
  val simplify_rule :PE_RULE -> PE_RULE
end



structure IL (*:IL*) =
struct
  (* --- imported types and values --- *)

  open ASM_AST ASM_Value

  type LOCATION = string * VALUE

  type TYPE = ASM_AST.TYPE
  type NAME = ASM_AST.NAME
  type PATT = ASM_AST.PATT
  type TERM = ASM_AST.TERM
  type RULE = ASM_AST.RULE
  type ENV  = ASM_Env.ENV


  (* --- miscellaneous --- *)

  val id = Misc.id

  exception FunctionKind
  fun function_kind (Id f)  =
      ( (valOf (ASM_Signature.functionKind (ASM_Signature.find (!ASM_Top.sign) f)))
	handle Option => raise FunctionKind )
    | function_kind _ = Static

  fun interpretation f = ASM_Top.currState () f

  fun const k _ = k


  (* --- errors and exceptions --- *)

  exception NotImplemented of string
  exception IL_Error of string


  (* --- data conversions --- *)

  fun ASM2ML_set2list UNDEF = raise IL_Error "ASM2ML_set2list UNDEF"
    | ASM2ML_set2list x     = ASM_Value.Set.listItems x

  fun ASM2ML_list UNDEF = raise IL_Error "ASM2ML_list UNDEF"
    | ASM2ML_list x     = ASM_Value.ASM2ML.list x


  (* --- environment and pattern-matching related stuff --- *)

  fun inEnv (env :ENV) obj = obj env
  fun inEnv2 (env :ENV) (obj1, obj2) = (obj1 env, obj2 env)

  infix ++ \\

  fun (env :ENV) ++ (env' :ENV) =
    ASM_Env.override (env, env')

  fun (env :ENV) \\ (p :PATT) = 
    let val vars = ASM_Collect.variablesInPatt p
    in ASM_Env.filter (fn (s, _) => not (ASM_Collect.VarSet.member (vars, s))) env
    end

  fun try_match (p, x) on_success on_failure =
    case ASM_Eval.match p x of
      ASM_Env.Success env => on_success (ASM_Env.Success env)
    | ASM_Env.Failure     => on_failure ()

  fun match_list (_ :PATT, [] :VALUE list) = []
    | match_list (p :PATT, (x :: xs) :VALUE list) :ENV list =
      ( case ASM_Eval.match p x of
	 ASM_Env.Success env => ASM_Env.Success env :: match_list (p, xs)
       | ASM_Env.Failure     => match_list (p, xs) )

  (* --- partially evaluated terms --- *)

  datatype PE_TERM' =
    Val of VALUE |
    Loc of LOCATION |
    Ctl of string * PE_TERM' ref |
    Term of (TYPE, NAME, PATT, PE_TERM' ref) TERM'

  type PE_TERM = PE_TERM' ref

  structure T_Lift = struct
      type TYPE = ASM_AST.TYPE
      type NAME = ASM_AST.NAME
      type PATT = ASM_AST.PATT
      type TERM = PE_TERM
      val h_type = Misc.id
      val h_name = Misc.id
      val h_patt = Misc.id
  end

  structure LiftTerm =
    LiftTermFn ( struct
      open T_Lift
      val F_term = ref o Term
    end )

  val pe_term :TERM -> PE_TERM = LiftTerm.term

  exception GetVal
  fun getVal (ref (Val x)) = x
    | getVal _ = raise GetVal


  (* --- partially evaluated rules (rules containing p.e. terms) --- *)

  datatype PE_RULE' =
    Rule of (NAME, PATT, PE_TERM, PE_RULE' ref) RULE'

  type PE_RULE = PE_RULE' ref

  structure LiftRule =
    LiftRuleFn ( struct
      open T_Lift
      type TERM = PE_TERM
      type RULE = PE_RULE
      val h_term = pe_term
      val F_rule = ref o Rule
    end )

  val pe_rule :RULE -> PE_RULE = LiftRule.rule


  (* --- rewriting rules to improve the transformation --- *)

  val (apply :string -> PE_TERM -> PE_TERM) =
    fn f => fn t => ref (Term (AppTerm (Id f, t)))
  val (apply2 :string -> PE_TERM * PE_TERM -> PE_TERM) =
    fn f => fn (t1, t2) => ref (Term (AppTerm (Id f, ref (Term (TupleTerm [ t1, t2 ])))))

  val TRUE  = ref (Val (BOOL true))
  val FALSE = ref (Val (BOOL false))
  val NIL   = ref (Val (CELL ("nil", TUPLE [])))
  val EMPTYSET = ref (Val (ASM_Value.Set.empty))
  val EMPTYMAP = ref (Val (ASM_Value.Map.empty))

  infix 3 And
  infix 2 Or

  fun Not (ref (Term (AppTerm (Id "not", t)))) = t
    | Not (ref (Val (BOOL false))) = TRUE
    | Not (ref (Val (BOOL true)))  = FALSE
    | Not t = apply "not" t

  fun (ref (Val (BOOL false))) And _ = FALSE
    | _ And (ref (Val (BOOL false))) = FALSE
    | (ref (Val (BOOL true))) And t2 = t2
    | t1 And (ref (Val (BOOL true))) = t1
    | t1 And t2                      = apply2 "and" (t1, t2)

  fun (ref (Val (BOOL true))) Or _   = TRUE
    | _ Or (ref (Val (BOOL true)))   = TRUE
    | (ref (Val (BOOL false))) Or t2 = t2
    | t1 Or (ref (Val (BOOL false))) = t1
    | t1 Or t2			     = apply2 "or" (t1, t2)

  fun equal_value (x, y) =
    (ASM_Value.compare (x, y) = EQUAL)

  (* 'Union' is used for simplifying set comprehensions... *)
  fun Union (t1 as ref (Val v), t2) = if equal_value (v, getVal EMPTYSET) then t2 else apply2 "union" (t1, t2)
    | Union (t1, t2 as ref (Val v)) = if equal_value (v, getVal EMPTYSET) then t1 else apply2 "union" (t1, t2)
    | Union (t1, t2)		    = apply2 "union" (t1, t2)

  (* 'MapUnion' is used for simplifying map comprehensions... *)
  fun MapUnion (t1 as ref (Val v), t2) = if equal_value (v, getVal EMPTYMAP) then t2 else apply2 "union" (t1, t2)
    | MapUnion (t1, t2 as ref (Val v)) = if equal_value (v, getVal EMPTYMAP) then t1 else apply2 "union" (t1, t2)
    | MapUnion (t1, t2)	    	       = apply2 "map_union" (t1, t2)

  (* 'Append' is used for simplifying list comprehensions... *)
  fun Append (t1 as ref (Val (CELL ("nil",_))), t2) = t2
    | Append (t1, t2 as ref (Val (CELL ("nil",_)))) = t1
    | Append (t1, t2)				    = apply2 "@" (t1, t2)

  fun SingletonSet t      = ref (Term (FSetEnumTerm [t]))
  fun SingletonMap (t,t') = ref (Term (FMapEnumTerm [(t,t')]))
  fun SingletonList t     = apply2 "::" (t, NIL)

  fun AND t_list = List_.foldll (op And) TRUE t_list
  fun OR  t_list = List_.foldll (op Or) FALSE t_list
  fun UNION t_list  = List_.foldll Union EMPTYSET t_list
  fun MAP_UNION t_list  = List_.foldll MapUnion EMPTYMAP t_list
  fun APPEND t_list = List_.foldr Append NIL t_list

  fun apply_rewriting ("not",   t) = Not t
    | apply_rewriting ("and",   ref (Term (TupleTerm [t1,t2]))) = t1 And t2
    | apply_rewriting ("or",    ref (Term (TupleTerm [t1,t2]))) = t1 Or t2
    | apply_rewriting ("union", ref (Term (TupleTerm [t1,t2]))) = Union (t1, t2)
    | apply_rewriting ("@",	ref (Term (TupleTerm [t1,t2]))) = Append (t1, t2)
    | apply_rewriting (f, t) = apply f t

  (* --- simplification: functions used for both terms and rules --- *)

  fun simplify_cond (constructor, default) G_B_list env =
    let val G_B_list = map (inEnv2 env) G_B_list
        fun F [] = []
          | F ((G,B)::rest) = 
            ( case G of
                ref (Val (BOOL true))  => [(G,B)]
              | ref (Val (BOOL false)) => F rest
              | _		       => (G,B) :: F rest )
    in case (F G_B_list) of
         [] => default
       | [(ref (Val (BOOL true)), B)] => B
       | G_B_list => constructor G_B_list
    end

  fun simplify_let (constructor, default) (p, t1_, t2_) env =
    case (p, t1_ env) of
      (p, ref (Val v1)) => try_match (p, v1) (fn env' => t2_ (env ++ env')) (const default)
    | (p, t1)		=> constructor (p, t1, t2_ (env \\ p))

  fun simplify_case (constructor, default) (t_, p_B_list_) env =
    let fun F v p_B_list_ = case p_B_list_ of
            [] => default
          | ((p,B_)::rest_) => try_match (p,v) (fn env' => B_ (env ++ env')) (fn _ => F v rest_)
    in case (t_ env) of
	 ref (Val v) => F v p_B_list_
       | t	     => constructor (t, map (fn (p, B_) => (p, B_ (env \\ p))) p_B_list_)
    end


  (* --- term simplification --- *)

  fun simplify_app_term (f :NAME, t_ :ENV -> PE_TERM) (env :ENV) :PE_TERM =
    case (function_kind f, t_ env) of
      (Static, ref (Val v))   => ref (Val (interpretation f v))
    | (Static, t)	      => apply_rewriting (idStr f, t)      (* idStr ok: it can not be a constant! *)
    | (Dynamic, ref (Val v))  => ref (Loc (idStr f, v))
    | (External, ref (Val v)) => ref (Loc (idStr f, v))
    | (Derived, t)	      => raise NotImplemented "derived functions"
    | (CTL, ref (Val v))      => ref (Ctl (idStr f, ref (Val v)))
    | (CTL, t)                => ref (Ctl (idStr f, t))
    | (_, t)		      => ref (Term (AppTerm (f, t)))

  fun simplify_tuple_term t_list_ env =
    let val t_list = map (inEnv env) t_list_
    in case (SOME (map getVal t_list) handle GetVal => NONE) of
         SOME v_list => ref (Val (TUPLE v_list))         (* all terms in t_list are values: simplify *)
       | NONE => ref (Term (TupleTerm t_list))    (* some of them are not values: leave unevaluated *)
    end

  fun simplify_variable v env =
    case ASM_Env.find (env, v) of
      SOME x => ref (Val x)
    | NONE   => ref (Term (VarTerm v))

  val simplify_cond_term = simplify_cond (ref o Term o CondTerm, ref (Val UNDEF))
  val simplify_let_term  = simplify_let (ref o Term o LetTerm, ref (Val UNDEF))
  val simplify_case_term = simplify_case (ref o Term o CaseTerm, ref (Val UNDEF))

  fun simplify_quantifier (constructor, bool_op) (p, A_, G_) env =
    case (A_ env) of
      ref (Val v) => bool_op (map (fn env' => G_ (env ++ env')) (match_list (p, ASM2ML_set2list v)))
    | A => ref (Term (constructor (p, A, G_ (env \\ p))))

  fun simplify_compr (constructor, range, combine, singleton, empty) simplify_term_env (t_, (p, A_, G_)) env =
    case (A_ env) of
      ref (Val v) => let val L = match_list (p, range v)
			 fun elem env = ref (Term (CondTerm [ (G_ env, singleton (t_ env)), (TRUE, empty) ]))
	             in simplify_term_env (combine (map (fn env' => elem (env ++ env')) L)) env end
    | A => ref (Term (constructor (t_ (env \\ p), (p, A, G_ (env \\ p)))))

  val simplify_set_compr  = simplify_compr (FSetComprTerm, ASM2ML_set2list, UNION, SingletonSet,  EMPTYSET)
  val simplify_list_compr = simplify_compr (ListComprTerm, ASM2ML_list, APPEND, SingletonList, NIL)


  fun simplify_map_compr simplify_term_env ((t_,t_'), (p, A_, G_)) env =
    case (A_ env) of
      ref (Val v) =>
        let val L = match_list (p, ASM2ML_set2list v)
	    fun elem env = ref (Term (CondTerm [ (G_ env, SingletonMap (t_ env, t_' env)), (TRUE, EMPTYMAP) ]))
        in simplify_term_env (MAP_UNION (map (fn env' => elem (env ++ env')) L)) env
        end
    | A => ref (Term (FMapComprTerm ((t_ (env \\ p), t_' (env \\ p)), (p, A, G_ (env \\ p)))))




  fun simplify_fset_enum_term ts_ env =
    let val ts = map (inEnv env) ts_
    in case (SOME (map getVal ts) handle GetVal => NONE) of
	 SOME xs => ref (Val (ASM_Value.Set.fromList xs))
       | NONE    => ref (Term (FSetEnumTerm ts))
    end

  fun simplify_fmap_enum_term ts_ env =
    let val ts = map (inEnv2 env) ts_
    in case (SOME (map (Pair.map getVal) ts) handle GetVal => NONE) of
	 SOME xs => ref (Val (ASM_Value.Map.fromList xs))
       | NONE    => ref (Term (FMapEnumTerm ts))
    end


  (* --- common structure for term and rule simplifier --- *)

  structure CommonSimplifySpec = struct
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
      type TERM  = ENV -> PE_TERM
      type TERM' = ENV -> PE_TERM
      type RULE  = ENV -> PE_RULE
      type RULE' = ENV -> PE_RULE
    end
    val h_type = Misc.id
    val h_name = Misc.id
    val h_patt = Misc.id
  end

  (* --- term simplifier (inductive) --- *)

  structure SimplifyTermSpec = struct
    open CommonSimplifySpec

    exception SimplifyTermEnv
    val simplify_term_env = ref (fn (t :PE_TERM) => fn (env :ENV) => (raise SimplifyTermEnv) :PE_TERM)

    fun h_term h_term' t env =
      case t of
	ref (Term t') => h_term' t' env
      | ref (Val v) => ref (Val v)
      | ref (Loc l) => ref (Loc l)
      | ref (Ctl (op_, t)) => ref (Ctl (op_, t))

    fun TermType (T, t) = t

    fun VarTerm v      = simplify_variable v
    fun AppTerm (f, t) = simplify_app_term (f, t)
    fun TupleTerm ts   = simplify_tuple_term ts

    fun CondTerm Gts       = simplify_cond_term Gts
    fun LetTerm (p, t, t') = simplify_let_term (p, t, t')
    fun CaseTerm (t, pts)  = simplify_case_term (t, pts)

    fun ListComprTerm (t, qual) = simplify_list_compr (!simplify_term_env) (t, qual)
    fun FSetComprTerm (t, qual) = simplify_set_compr (!simplify_term_env) (t, qual)
    fun FMapComprTerm (t, qual) = simplify_map_compr (!simplify_term_env) (t, qual)

    fun ForallTerm qual = simplify_quantifier (ASM_AST.ForallTerm, AND) qual
    fun ExistsTerm qual = simplify_quantifier (ASM_AST.ExistsTerm, OR) qual

    fun FSetEnumTerm ts = simplify_fset_enum_term ts
    fun FMapEnumTerm ts = simplify_fmap_enum_term ts
  end


  structure SimplifyTerm = TermInduction (SimplifyTermSpec)

  val _ = ( SimplifyTermSpec.simplify_term_env := SimplifyTerm.term )


  (* --- rule simplification --- *)

  val skip = ref (Rule (BlockRule []))

  fun block_rule [] = skip
    | block_rule [R] = R
    | block_rule R_list = ( case List_.filter (fn R => R <> skip) R_list of 
			      [R] => R
			    | R_list' => ref (Rule (BlockRule R_list')) )

  fun cond_rule [] = skip
    | cond_rule G_R_list = ( case (List_.filter (fn (G, R) => R <> skip) G_R_list) of
			       [] => skip
			     | G_R_list' => ref (Rule (CondRule G_R_list')) )

  fun simplify_update_rule ((f, t_l_), t_r_) env =
    ref (Rule (UpdateRule ((f, t_l_ env), t_r_ env)))

  fun simplify_block_rule R_list_ env =
    block_rule (map (inEnv env) R_list_)

  fun simplify_cond_rule G_R_list_ env =
    case (simplify_cond (ref o Rule o CondRule, skip) G_R_list_ env) of
      ref (Rule (CondRule G_R_list)) => cond_rule G_R_list
    | R => R

  val simplify_let_rule  = simplify_let (ref o Rule o LetRule, skip)
  val simplify_case_rule = simplify_case (ref o Rule o CaseRule, skip)

  fun simplify_forall_rule ((p, A_, G_), R_) env =
    case (A_ env) of
      ref (Val v) => let val L = match_list (p, ASM2ML_set2list v)
		     in ref (Rule (BlockRule (map (fn env' => R_ (env ++ env')) L))) end
    | A => ref (Rule (ForallRule ((p, A, G_ (env \\ p)), R_ (env \\ p))))


  (* --- rule simplifier (inductive) --- *)

  structure SimplifyRuleSpec = struct
    open CommonSimplifySpec

    val h_term = SimplifyTerm.term

    fun h_rule h_rule' (ref (Rule R')) env = h_rule' R' env

    fun UpdateRule ((f,t),t') = simplify_update_rule ((f,t),t')
    fun BlockRule Rs          = simplify_block_rule Rs
    fun CondRule GRs          = simplify_cond_rule GRs
    fun LetRule (p, t, R')    = simplify_let_rule (p, t, R')
    fun CaseRule (t, pRs)     = simplify_case_rule (t, pRs)
    fun ChooseRule _          = raise IL_Error "not implemented: choose rule"
    fun ForallRule (qual, R)  = simplify_forall_rule (qual, R)
    fun AppRule _             = raise IL_Error "error: application rule should be already expanded"
  end

  structure SimplifyRule = RuleInduction (SimplifyRuleSpec)


  (* --- exportable functions --- *)

  fun simplify_term t = SimplifyTerm.term t ASM_Env.empty
  fun simplify_rule R = SimplifyRule.rule R ASM_Env.empty
end
