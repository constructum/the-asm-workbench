(* ******************************************************************* *\
 *
 *   The ASM Workbench for SML/NJ 1.10
 *
 *   File:         asm-check.sml
 *
 *   Description: 
 *     Type checker for ASM-SL (mostly inductive)
 *
 *   Author:       $Author: giusp $
 *   Date:         $Date: 2001/04/28 13:14:53 $
 *   Revision:     $Revision: 1.8 $
 *
\* ******************************************************************* *)

structure ASM_Check =
struct

  open Misc ASM_AST
  structure Sgn = ASM_Signature
  structure Env = ASM_TypeEnv
  type SGN = Sgn.SIGNATURE
  type ENV = Env.TYPE_ENV

  fun println s = ( print s; print "\n" )


  (* =================================================================== *\

	error handling

  \* =================================================================== *)

  datatype PROBLEM =
    Impossible of string
  | NotImplemented of string
  | UndefinedTypeConstructor of string
  | TypeConstructorArity of string * int * int
  | UnboundTypeParam of TYPE
  | NotConstructor of NAME
  | NotFuncOrRuleName of NAME
  | UnboundName of NAME
  | DuplicatePattVar of string
  | UnboundVariable of string
  | IncompatibleTypes of NAME * GET_TYPEALIAS * TYPE * TYPE
  | TypeMismatch of GET_TYPEALIAS * TYPE * TYPE
  | TypeMismatchRangeConstr of GET_TYPEALIAS * TYPE * TYPE
  | NonStaticRangeConstr
  | TypeMismatchTypeConstr of GET_TYPEALIAS * TYPE * TYPE
  | InvalidQualifierRange of TYPE
  | IllegalOperator of string * NAME
  | UpdateLhsNotDynamic of NAME
  | StaticTermExpected (*of NAME*)
  | DuplicateDefinition of string
  | ExternalFunctionConstraint
  | LambdaDefNotAllowed
  | PolymorphicExtensionalDef
  | LambdaDefExpected
  | DuplicateTypeParamInType of string

  exception Error of (LOCATION option * POSITION option * PROBLEM) Error.ERROR

  fun message (location, position, problem) =
    let fun spaces n = implode (List.tabulate (n, fn _ => #" "))
        val R = String_.replace
	fun showType T = Pretty.toString 1000 (ASM_Pretty.type_ T)
	val where_ = String_.replace "$1:$2:\n"
		       [ ASM_Location.toErrorString (valOf location) handle Option => "--",
			 ASM_Position.toErrorString (valOf position) handle Option => "--" ]
	val what_  = case problem of
	  Impossible s     => R "Impossible! This cannot happen! (Probably a bug) [$1]" [s]
	| NotImplemented s => R "$1: not implemented yet" [s]
	| UndefinedTypeConstructor typecon => R "undefined type constructor '$1'" [ typecon ]
	| TypeConstructorArity (typecon, arity, n_args) =>
	    R "type constructor '$1' has wrong number of arguments ($2 expected, $3 found)"
	      ([ typecon ] @ (map Int.toString [ arity, n_args ]))
	| UnboundTypeParam T => R "unbound type parameter: $1" [ showType T ]
	| NotConstructor nm  => R "badly formed pattern: '$1' defined, but not a constructor"
                                  [ showName nm ]
	| NotFuncOrRuleName nm  => R "not a function or rule name ('$1')" [ showName nm ]
	| UnboundName nm  => R "unbound name ('$1')" [ showName nm ]
	| DuplicatePattVar a => R "duplicate variable '$1' in pattern" [ a ]
	| UnboundVariable s => R "unbound variable '$1'" [ s ]
        | InvalidQualifierRange T =>
            R "qualifier range must be a list, a finite set, or a finite map,\n\
              \    its actual (inferred) type is $1 instead" [ showType T ]
	| StaticTermExpected (*f*)     => R "static term expected in definition of static function" (*('$1' is not static)*) [(* showName f*) ]
	| IllegalOperator (which, f) =>
	    R "illegal $1 application ('$2' is not a dynamic function name)" [which, showName f]
	| IncompatibleTypes (f, typealias, T_f, T_args) =>
	    R "function $1 of type: $2\n$3has argument of type: $4"
	      [ showName f,
                showType (fullyExpand typealias (typeClosure T_f)),
                spaces (15 + size (showName f)),
                showType (fullyExpand typealias (domain T_args)) ]
	| TypeMismatch (typealias, T1, T2) =>
	    R "expected type: $1,\n\
	      \         actual (inferred) type: $2" 
	     (map (showType o (fullyExpand typealias)) [ T1, T2 ])
	| TypeMismatchRangeConstr (typealias, T1, T2) =>
	    R "type of finiteness constraint incompatible with function type:\n\
              \                    expected type: $1,\n\
	      \           actual (inferred) type: $2" 
	     (map (showType o (fullyExpand typealias)) [ T1, T2 ])
        | NonStaticRangeConstr =>
            R "non-static term in finiteness constraint" []
	| TypeMismatchTypeConstr (typealias, T1, T2) =>
	    R "type constraint: $1,\n\
	      \           actual (inferred) type: $2" 
	     (map (showType o (fullyExpand typealias)) [ T1, T2 ])
	| UpdateLhsNotDynamic f     => R "update rule lhs: '$1' is not a dynamic function name" [ showName f ]
	| DuplicateDefinition s     => R "duplicate definition for '$1'" [ s ]
	| ExternalFunctionConstraint => "external function declaration must include (monomorphic) \
					\type constraint"
	| LambdaDefNotAllowed       => "intensional definition not allowed here \n\
                                       \                  (use MAP_TO_FUN or SET_TO_REL instead)"
	| LambdaDefExpected         => "intensional definition expected here"
	| PolymorphicExtensionalDef => "external and extensionally defined functions can not have\n\
                                       \                  polymorphic type\
				       \ (please provide monomorphic type constraint)"
	| DuplicateTypeParamInType s => R "duplicate type parameter in definition of type $1" [ s ]
    in where_ ^ "  Typing error -- " ^ what_ ^ "\n"
  end

  val currLoc = ref (NONE :LOCATION option)

  fun error fct pos what =
    raise Error
    { module = "ASM_Check",
      function = fct,
      problem = (!currLoc, pos, what),
      message = message,
      cause = NONE }


  (* =================================================================== *\

	types

  \* =================================================================== *)

  fun onlyValueType pos T =
    if (isSome (selFuncType T))
    then error "onlyValueType" pos (Impossible "(1)")
    else T

  fun checkBasetype (pos :POSITION option) (sgn :SGN) (C, Ts) =
    let val ERROR = error "checkBaseType" pos
        val onlyValueType = onlyValueType pos
    in if C = "_TUPLE"
       then (C, map onlyValueType Ts)
       else let val C' = Sgn.find sgn C
            in if Sgn.idKind C' <> SOME TypeKind
               then ERROR (UndefinedTypeConstructor C)
	       else if valOf (Sgn.typeArity C') <> length Ts
               then ERROR (TypeConstructorArity (C, valOf (Sgn.typeArity C'), length Ts))
	       else (C, map onlyValueType Ts)
            end   handle Option => ERROR (Impossible "Option (1)")
    end

  fun checkType (pos :POSITION option) (sgn :SGN) (typeParams :ParSet.set) =
    let val ERROR = error "checkType" pos
    in inductive
       { RuleType  = RuleType,
	 BaseType  = BaseType o (checkBasetype pos sgn),
	 FuncType  = FuncType o (Pair.map (onlyValueType pos)),
	 TypeParam = TypeParam o ( fn s => if ParSet.member (typeParams, s) then s
					   else ERROR (UnboundTypeParam (TypeParam s)) ),
	 TypeVar   = TypeVar  (* fn _ => bug ("type variable") *)  }
    end

  fun type_ T (pos, sgn)  = checkType pos sgn (typeparamsSet T) T

  (* =================================================================== *\

	auxiliary functions (mostly shared for terms/rules)

  \* =================================================================== *)

  fun reduceKind [] = Static
    | reduceKind (x :: xs) = if x = Static then reduceKind xs else Derived

  fun checkConstructor _ (pos, IntConst _) _ = ()
    | checkConstructor _ (pos, FloatConst _) _ = ()
    | checkConstructor _ (pos, StringConst _) _ = ()
    | checkConstructor sgn (pos, Id s) ERROR =
        if Sgn.isConstructor (Sgn.find sgn s) then () else ERROR pos (NotConstructor (Id s))
    | checkConstructor sgn (pos, FunToMap f) ERROR =
        ERROR pos (NotConstructor (FunToMap f))
    | checkConstructor sgn (pos, RelToSet f) ERROR =
        ERROR pos (NotConstructor (RelToSet f))

  fun checkApp sgn ((pos, f as (f_name, _, f_type)), arg_type) ERROR =
    let val res_type = freshTypeVar ()
	val fct_type = FuncType (arg_type, res_type)
	val _ = (unify (Sgn.typealias sgn) (f_type, fct_type))
		handle _ => ERROR pos (IncompatibleTypes (f_name, (Sgn.typealias sgn), f_type, fct_type))
    in res_type
    end

  fun checkConstraint sgn ((pos, actual_type), constraint) ERROR =
    let val _ = unify (Sgn.typealias sgn) (constraint, actual_type)
                handle _ => ERROR pos (TypeMismatchTypeConstr (Sgn.typealias sgn, constraint, actual_type))
    in actual_type
    end

  fun check sgn ((pos, actual_type), expected_type) ERROR =
    let val _ = unify (Sgn.typealias sgn) (expected_type, actual_type)
          handle _ => ERROR pos (TypeMismatch (Sgn.typealias sgn, expected_type, actual_type))
    in actual_type
    end

  fun dropKind (pos, kind, type_) = (pos, type_)

  fun checkT sgn ((pos, actual_kind, actual_type), expected_type) ERROR =
    check sgn ((pos, actual_type), expected_type) ERROR

  fun checkCase (t0 as (t0_pos, t0_kind, t0_type), pxs) (pos, sgn, env) ERROR =
    let val pxs = map (fn (p, x_) => (p, x_ ())) pxs
        val (p_type_0, x_type_0) = (t0_type, freshTypeVar ()) 
	fun casePair (p as (p_pos, p_env, p_type), x as (x_pos, _, x_type))  =
	  (check sgn ((p_pos, p_type), p_type_0) ERROR, checkT sgn (x, x_type_0) ERROR)
        val res_kind = reduceKind (t0_kind :: (map (#2 o #2) pxs))
    in map casePair pxs; (pos, res_kind, x_type_0)
    end

  fun checkCond GBs (pos, sgn, _) ERROR =
    let val B_type          = freshTypeVar ()
        val GBs             = map (fn (G,B) => (G(),B())) GBs
	fun condPair (G, B) = (checkT sgn (G, Bool) ERROR, checkT sgn (B, B_type) ERROR)
        val res_kind = reduceKind (map (fn ((_,G_kind,_), (_,B_kind,_)) => reduceKind [ G_kind, B_kind ]) GBs)
    in map condPair GBs; (pos, res_kind, B_type)
    end

  (* =================================================================== *\

       NOTE: the rest of the type checker is defined using the
	 context-dependent transformation functors. First of all, the
	 target types of the type-checking functions for all syntactic
	 categories are defined (structure "T").  Source types
	 (structure "S") are simply the ASTs.

  \* =================================================================== *)

  fun updatePos pos (newPos as SOME _) = newPos
    | updatePos pos (newPos as NONE)   = pos

  structure S = ASM_AST
  structure CTX = struct
    type TYPE = POSITION option * SGN
    type NAME = POSITION option * SGN
    type PATT = POSITION option * SGN * ENV
    type TERM = POSITION option * SGN * ENV
    type RULE = TERM
    type FUNCTION_EXPR   = POSITION option * SGN
    type TRANSITION_EXPR = POSITION option * SGN
    type CONSTRAINT      = POSITION option * SGN
    type DEF             = POSITION option * SGN

    fun patt2type (pos, sgn, env) = (pos, sgn)
    fun patt2name (pos, sgn, env) = (pos, sgn)

    fun term2type (pos, sgn, env) = (pos, sgn)
    fun term2name (pos, sgn, env) = (pos, sgn)
    fun term2patt (pos, sgn, env) = (pos, sgn, env)

    fun rule2name (pos, sgn, env) = (pos, sgn)
    fun rule2patt (pos, sgn, env) = (pos, sgn, env)
    fun rule2term (pos, sgn, env) = (pos, sgn, env)

    fun functionExpr2patt (pos, sgn) = (pos, sgn, Env.empty)
    fun functionExpr2term (pos, sgn) = (pos, sgn, Env.empty)

    fun transitionExpr2patt (pos, sgn) = (pos, sgn, Env.empty)

    val def2constraint     = Misc.id
    val def2functionExpr   = Misc.id
    val def2transitionExpr = Misc.id
  end


  structure T = struct
    type TYPE  = S.TYPE
    type NAME  = S.NAME * S.FUNCTION_KIND option * S.TYPE
    type FUNCTION_KIND = FUNCTION_KIND
    type OP_STATUS     = OP_STATUS
    type PATT  = POSITION option * ENV * S.TYPE
    type PATT' = PATT

    type QUALIFIER = POSITION option * ENV * S.FUNCTION_KIND * S.TYPE

    type TERM  = POSITION option * S.FUNCTION_KIND * S.TYPE  (* kind = Static/Derived: check for static terms!*)
    type TERM' = TERM
    type RULE  = POSITION option * S.FUNCTION_KIND * S.TYPE
    type RULE' = RULE

    type FUNCTION_EXPR  = POSITION option * S.FUNCTION_KIND * S.TYPE               (* kind: same as for terms *)
    type FUNCTION_EXPR' = FUNCTION_EXPR

    type TRANSITION_EXPR  = POSITION option * S.TYPE
    type TRANSITION_EXPR' = TRANSITION_EXPR

    type CONSTRAINT = POSITION option
    type DEF  = POSITION option * SGN
    type DEF' = DEF
  end

  (* =================================================================== *\

	names

  \* =================================================================== *)

  fun name_ nm (pos, sgn) =
    let fun check_special_operator (ast_constructor_name (* "FunToMap" or "RelToSet" *), operator_name (* "FUN_TO_MAP" or "REL_TO_SET" *), f) =
      let val _ = Trace.outr "ASM_Check.name_ / $1 $2" [ ast_constructor_name, f ]
	  val f_kind = Sgn.functionKind (Sgn.find sgn f)
      in if not (f_kind = SOME Dynamic)   (* !!!!! in principle it could also be static, as long as extensionally defined !!!! *)
	 then error ast_constructor_name pos (IllegalOperator (operator_name, Id f))
	 else case Sgn.typeOf (Sgn.find sgn f)	of
		  NONE => error ast_constructor_name pos (UnboundName (Id f))
		| SOME f_type => 
		  let val (dom_type, ran_type) = (freshTypeVar (), if operator_name = "FUN_TO_MAP" then freshTypeVar () else Bool)
		      val _ = check sgn ((pos, f_type), FuncType (dom_type, ran_type)) (error ast_constructor_name)
		  in (nm, if f_kind = SOME Static then SOME Static else SOME Derived, FuncType (Unit, Map (dom_type, ran_type)))
		  end
      end
    in case nm of
           IntConst _     => (nm, SOME Static, FuncType (Unit, Int))
	 | FloatConst _   => (nm, SOME Static, FuncType (Unit, Float))
	 | StringConst _  => (nm, SOME Static, FuncType (Unit, String))
	 | Id id          => ( Trace.outr "ASM_Check.name_ / Id $1" [ id ];
			       ( nm, Sgn.functionKind (Sgn.find sgn id),
				 ( ASM_Type.instanceOf (valOf (Sgn.typeOf (Sgn.find sgn id)))
				   handle Sgn.Error _ => error "name_" pos (NotFuncOrRuleName nm) ))
                               handle Option => error "name_" pos (UnboundName nm) )
	 | FunToMap f     => check_special_operator ("FunToMap", "FUN_TO_MAP", f)
	 | RelToSet f     => check_special_operator ("RelToSet", "REL_TO_SET", f)
    end				

  (* =================================================================== *\

	patterns

  \* =================================================================== *)

  structure PattCheckSpec :PATT_TRANSF_SPEC = struct
    structure S   = ASM_AST
    structure T   = T
    structure CTX = CTX

    val (h_type, h_name) = (type_, name_)

    fun h_patt patt' (PattPos (newPos, p')) (pos, sgn, env) =
      patt' p' (updatePos pos newPos, sgn, env)

    fun PattType (T, (p_pos, p_env, p_type)) (pos, sgn, _) =
      ( pos, p_env, checkConstraint sgn ((p_pos, p_type), T) (error "PattType") )

    fun Placeholder (pos, _, _) =
      ( pos, Env.empty, freshTypeVar () )

    fun VarPatt varId (pos, _, _) =
      let val alpha = freshTypeVar ()
      in ( pos, Env.singleton (varId, alpha), alpha )
      end

    fun AppPatt (f as (f_name, _, _), (p_pos, p_env, p_type)) (pos, sgn, _) =
      let val ERROR    = error "AppPatt"
          val _        = checkConstructor sgn (pos, f_name) ERROR
          val res_type = checkApp sgn ((pos, f), p_type) ERROR
      in ( pos, p_env, res_type )
      end

    fun TuplePatt ps (pos, _, _) =
      let val (poss, envs, types) = unzip3 ps
      in ( pos, Env.Union envs, Tuple types )
         handle Env.Clash x => error "TuplePatt" pos (DuplicatePattVar x)
      end
  end

  structure PattCheck = PattTransf (PattCheckSpec)
  val patt = PattCheck.patt


  (* =================================================================== *\

	terms

  \* =================================================================== *)

  structure TermCheckSpec :TERM_TRANSF_SPEC = struct
(* !!!!   type ('patt, 'term, 'guard) QUALIFIER = ('patt, 'term, 'guard) ASM_AST.QUALIFIER*)

    structure S   = ASM_AST
    structure T   = T
    structure CTX = CTX

    val (h_type, h_name, h_patt) = (type_, name_, patt)

    fun bind (pos, sgn, env) (p as (p_pos, p_env, p_type), t as (t_pos, t_kind, t_type)) =
      let val _ = unify (Sgn.typealias sgn) (p_type, t_type)
            handle _ => error "bind" t_pos (TypeMismatch (Sgn.typealias sgn, p_type, t_type))
      in (pos, sgn, Env.override (env, p_env))
      end

    fun qualifier ( ( p as (p_pos, p_env, p_type), A as (A_pos, A_kind, A_type), G_)
                    :(T.PATT,T.TERM,T.TERM -> T.TERM) QUALIFIER ) (pos, sgn, env)=
      let val ERROR = error "qualifier"
          val _     = case (selBaseType A_type) of
                        SOME ("SET", _)  => check sgn ((A_pos, A_type), Set p_type) ERROR
                      | SOME ("LIST", _) => check sgn ((A_pos, A_type), List p_type) ERROR
(*                    | SOME ("MAP", _)  => check sgn ((A_pos, A_type), Map p_type) ERROR
                                           -- add 'MapItem' to implement this *)
                      | _                => check sgn ((A_pos, A_type), freshTypeVar ()) ERROR
          val _     = checkT sgn (G_ (p_pos, Static (* ???!!! *), p_type), Bool) ERROR
      in ( A_pos, p_env, A_kind, A_type )
      end

    fun qualBind (pos, sgn, env) (qual as (q_pos, q_env, q_kind, q_type)) =
      (pos, sgn, Env.override (env, q_env))


    fun h_term term' (TermPos (newPos, t')) (pos, sgn, env) =
      term' t' (updatePos pos newPos, sgn, env)

    fun TermType (T, t as (t_pos, t_kind, t_type)) (pos, sgn, _) =
      ( pos, t_kind, checkConstraint sgn ((t_pos, t_type), T) (error "TermType") )

    fun VarTerm varId (pos, sgn, env) =
      let fun ERROR x = error "VarTerm" pos x
      in (pos, Static, case (Env.find (env, varId)) of
           SOME T => T
         | NONE   => ERROR (UnboundVariable varId))
      end

    fun AppTerm (f as (f_name, f_kind, f_type), t as (t_pos, t_kind, t_type)) (pos, sgn, _) =
      let val res_kind = case f_kind of SOME Static => t_kind | _ => Derived
          val res_type = checkApp sgn ((pos, f), t_type) (error "AppTerm")
      in (pos, res_kind, res_type)
      end

    fun TupleTerm ts (pos, _, _) =
      (fn (_, ts_kind, ts_type) => (pos, reduceKind ts_kind, Tuple ts_type)) (Misc.unzip3 ts)

    fun CondTerm Gts ctx = checkCond Gts ctx (error "CondTerm")
    fun LetTerm (p, t1, t2) _  = t2
    fun CaseTerm (t0, pts) ctx = checkCase (t0, pts) ctx (error "CaseTerm")

    fun ListComprTerm (t_, qual as (q_pos, q_env, q_kind, q_type)) (pos, sgn, env) =
      let val _ = check sgn ((q_pos, q_type), List (freshTypeVar ())) (error "ListComprTerm")
                    (* only lists are allowed as ranges of list comprehensions! *)
	  val (t_pos, t_kind, t_type) = t_ (qualBind (pos, sgn, env) qual)
      in (pos, reduceKind [ t_kind, q_kind ], List t_type)
      end

    fun FSetComprTerm (t_, qual as (q_pos, q_env, q_kind, q_type)) (pos, sgn, env) =
      let val _ = check sgn ((q_pos, q_type), Set (freshTypeVar ())) (error "FSetComprTerm")
	  val (t_pos, t_kind, t_type) = t_ (qualBind (pos, sgn, env) qual)
      in (pos, reduceKind [ t_kind, q_kind ], Set t_type)
      end

    fun FMapComprTerm ( (t1_, t2_), qual as (q_pos, q_env, q_kind, q_type)) (pos, sgn, env) =
      let val _ = check sgn ((q_pos, q_type), Set (freshTypeVar ())) (error "FMapComprTerm")
          val env' = qualBind (pos, sgn, env) qual
	  val ((_, t1_kind, t1_type), (_, t2_kind, t2_type)) = (t1_ env', t2_ env')
      in (pos, reduceKind [ t1_kind, t2_kind, q_kind ], Map (t1_type, t2_type))
      end

    fun ForallTerm (qual as (_, _, q_kind, _)) (pos, _, _) = (pos, q_kind, Bool)
    fun ExistsTerm (qual as (_, _, q_kind, _)) (pos, _, _) = (pos, q_kind, Bool)

    fun FSetEnumTerm ts (pos, sgn, _) =
      let val elem_type   = freshTypeVar ()
          val _ = map (fn t => checkT sgn (t, elem_type) (error "FSetEnumTerm")) ts
      in (pos, reduceKind (map #2 ts), Set elem_type)
      end

    fun FMapEnumTerm tts (pos, sgn, _) =
      let val ERROR = error "FMapEnumTerm"
          val (dom_type, ran_type) = (freshTypeVar (), freshTypeVar ())
          val _ = map (fn (t, t') => (checkT sgn (t, dom_type) ERROR, checkT sgn (t', ran_type) ERROR)) tts
      in (pos, reduceKind (map (fn (t, t') => reduceKind [ #2 t, #2 t' ]) tts), Map (dom_type, ran_type))
      end
  end

  structure TermCheck = TermTransf (TermCheckSpec)
  val term = TermCheck.term


  (* =================================================================== *\

	transition rules

  \* =================================================================== *)

  structure RuleCheckSpec :RULE_TRANSF_SPEC = struct
    type ('patt, 'term, 'guard) QUALIFIER = ('patt, 'term, 'guard) ASM_AST.QUALIFIER

    structure S   = ASM_AST
    structure T   = T
    structure CTX = CTX

    val (h_name, h_patt, h_term) = (name_, patt, term)

    val bind      = TermCheckSpec.bind
    val qualifier = TermCheckSpec.qualifier

    fun h_rule rule' (RulePos (newPos, R')) (pos, sgn, env) =
      rule' R' (updatePos pos newPos, sgn, env)

    fun UpdateRule ((f as (f_name, f_kind, f_type), t as (t_pos, _, t_type)), t' as (t'_pos, _, t'_type))
                   (pos, sgn, env) =
      if not (f_kind = SOME Dynamic)
      then error "UpdateRule" pos (UpdateLhsNotDynamic f_name)
      else let val lhs_type = checkApp sgn ((pos, f), t_type) (error "UpdateRule")
	       val _        = checkT sgn (t', lhs_type) (error "UpdateRule")
	   in (pos, Derived, RuleType)
	   end

    fun BlockRule Rs (pos,_,_) = (pos, Derived, RuleType)
    fun CondRule GRs ctx       = checkCond GRs ctx (error "CondRule")

    fun LetRule (p, t, R) _    = R
    fun CaseRule (t0, pRs) ctx = checkCase (t0, pRs) ctx (error "CaseRule")

    fun ChooseRule (qual as (q_pos, q_env, q_kind, q_type), R) (pos, sgn, env) =
      let val _ = check sgn ((q_pos, q_type), Set (freshTypeVar ())) (error "ChooseRule")
      in (pos, Derived, RuleType)
      end

    fun ForallRule (qual, R_) ctx           = ChooseRule (qual, R_) ctx
    fun AppRule (r, (_,_,t_type)) (pos,sgn,_) = (pos, Derived, checkApp sgn ((pos, r), t_type) (error "AppRule"))
  end

  structure RuleCheck = RuleTransf (RuleCheckSpec)
  val rule = RuleCheck.rule


  (* =================================================================== *\

	function expressions & transition expressions

  \* =================================================================== *)

  structure FunctionExprCheckSpec :FUNCTION_EXPR_TRANSF_SPEC = struct
    structure S   = ASM_AST
    structure T   = T
    structure CTX = CTX

    val (h_patt, h_term) = (patt, term)

    fun h_functionExpr functionExpr' (FunctionExprPos (newPos, FE')) (pos, sgn) =
      functionExpr' FE' (updatePos pos newPos, sgn)

    fun MapToFun (t as (t_pos, t_kind, t_type)) (ctx as (pos, sgn)) =
      let val (dom_type, ran_type) = (freshTypeVar (), freshTypeVar ())
	  val _ = check sgn ((t_pos, t_type), Map (dom_type, ran_type)) (error "MapToFun")
      in (pos, t_kind, FuncType (dom_type, ran_type))
      end         (* !!!!!!! [[ add monomorphism check here ]] *)

    fun SetToRel (t as (t_pos, t_kind, t_type))  (ctx as (pos, sgn)) =
      let val dom_type = freshTypeVar ()
	  val _ = check sgn ((t_pos, t_type), Set dom_type) (error "SetToRel")
      in (pos, t_kind, FuncType (dom_type, Bool))
      end         (* !!!!!!! [[ add monomorphism check here ]] *)

    fun LambdaTerm (p as (p_pos, p_env, p_type), t_) (ctx as (pos, sgn)) =
      let val (t_pos, t_kind, t_type) = t_ (p_pos, sgn, p_env)
      in (pos, t_kind, FuncType (p_type, t_type))
      end
  end

  structure TransitionExprCheckSpec :TRANSITION_EXPR_TRANSF_SPEC = struct
    structure S   = ASM_AST
    structure T   = T
    structure CTX = CTX

    val (h_patt, h_rule) = (patt, rule)

    fun h_transitionExpr transitionExpr' (TransitionExprPos (newPos, FE')) (pos, sgn) =
      transitionExpr' FE' (updatePos pos newPos, sgn)

    fun LambdaRule (p as (p_pos, p_env, p_type), R_) (ctx as (pos, sgn)) =
      let val (R_pos, _, R_type) = R_ (p_pos, sgn, p_env)
      in (pos, FuncType (p_type, R_type))
      end
  end

  structure FunctionExprCheck = FunctionExprTransf (FunctionExprCheckSpec)
  val functionExpr = FunctionExprCheck.functionExpr

  structure TransitionExprCheck = TransitionExprTransf (TransitionExprCheckSpec)
  val transitionExpr = TransitionExprCheck.transitionExpr



  (* =================================================================== *\

       definitions (not inductively defined)

  \* =================================================================== *)

  fun err x = error "checkDef" x
  fun bug x s = error "checkDef" x (Impossible s)

  fun no_duplicate pos sgn f =
    if Sgn.defines (sgn, f) then err pos (DuplicateDefinition f) else ()

  fun insert_in_signature pos sgn (f, f_info) =
    if Sgn.defines (sgn, f) then err pos (DuplicateDefinition f) else Sgn.insert (sgn, f, f_info)

  fun list2sgn pos f_T_list =
    List_.foldll (fn (sgn',sgnItem) => insert_in_signature pos sgn' sgnItem) ASM_Signature.empty f_T_list

  fun enforceMonomorphicType pos T_f =
    if not (isMonomorphicType T_f)
    then err pos PolymorphicExtensionalDef
    else ()

  fun normalize sign T =
    (fullyExpand (Sgn.typealias sign)) (typeClosure T)

  fun checkDefSeq (checkDef :ASM_AST.DEF -> CTX.DEF -> T.DEF) (pos, sgn0) Ds  =
    let fun checkDefs (sgn0 :SGN) ([] :DEF list) = Sgn.empty
          | checkDefs (sgn0 :SGN) (D :: D_rest) =
	      let val (pos', sgn') = checkDef D (pos, sgn0)
                  val sgn''        = checkDefs (Sgn.union (sgn0, sgn')) D_rest
              in Sgn.override (sgn', sgn'')
              end
    in (pos, checkDefs sgn0 Ds)
    end

  fun checkTypealiasDef (pos, sgn0) (TName, params, T) =
    let val params_set = typeparamsSet (BaseType ("", map TypeParam params))
	val _ = if ParSet.numItems (params_set) < length params
		then err pos (DuplicateTypeParamInType TName)
		else ()
	val _ = no_duplicate pos sgn0 TName
        val T = checkType pos sgn0 params_set T
    in ( pos, Sgn.singleton (TName, Sgn.TypeAlias { lhs = BaseType (TName, map TypeParam params), rhs = T }) )
    end

  fun checkFreetypeDef (pos, sgn0) tys =
    let fun typeparams_set params_list = typeparamsSet (BaseType ("", map TypeParam params_list))
        fun insert_one_type (sgn, (T, params, _)) =
          if ParSet.numItems (typeparams_set params) < length params
          then err pos (DuplicateTypeParamInType T)
          else ( no_duplicate pos sgn0 T;
                 insert_in_signature pos sgn ( T, Sgn.Type { arity = length params } ) )
        val sgn_type = List_.foldll insert_one_type Sgn.empty tys
        val sgn'     = Sgn.override (sgn0, sgn_type)
        fun insert_one_cons_list (sgn, (T_name, params, cons_list)) = let val params_set = typeparams_set params
              fun insert_one_cons (sgn, ((cons_opstatus, cons_name), cons_dom)) =
                    let val _        = no_duplicate pos sgn' cons_name
                        val cons_dom = checkType pos sgn' params_set cons_dom
                    in insert_in_signature pos sgn ( cons_name, Sgn.Func
			  { functionKind = Sgn.Static, constructor = true, opStatus = cons_opstatus,
			    type_ = FuncType (cons_dom, BaseType (T_name, map TypeParam params)) } )
                    end
          in List_.foldll insert_one_cons sgn cons_list
          end
        val sgn_conss  = List_.foldll insert_one_cons_list Sgn.empty tys
        val sgn_result = Sgn.override (sgn_type, sgn_conss)
    in (pos, sgn_result)
    end

  fun checkRuleDef (pos, sgn0) (r, rhs) =
    let val _     = no_duplicate pos sgn0 r
        val (pos_rhs, T_rhs) = transitionExpr rhs (pos, sgn0)
        val T_rhs = normalize sgn0 T_rhs
    in (pos, Sgn.singleton (r, Sgn.Rule { type_ = ASM_Type.cleanupType T_rhs }))
    end

  fun function (fk :FUNCTION_KIND, fOpSt :OP_STATUS, T :TYPE) =
    Sgn.Func { functionKind = fk, constructor = false,
	       opStatus = fOpSt,  type_ = ASM_Type.cleanupType T }

  fun strengthenTypeConstraint (pos, sgn0 :SGN) (T_f :TYPE) (constr :CONSTRAINT) :CONSTRAINT =
    let val new_TC =
	  case (#TypeConstraint constr) of
            NONE => T_f
          | SOME T_constr => check sgn0 ((pos, T_constr), T_f) (error "strengthenTypeConstraint")
    in { TypeConstraint = SOME new_TC,
         RangeConstraint = #RangeConstraint constr,
	 DomainConstraint = #DomainConstraint constr }
    end

  fun getTypeConstraint (constr :CONSTRAINT) =
    case #TypeConstraint constr of
      SOME T => ASM_Type.instanceOf T
    | NONE   => FuncType (freshTypeVar (), freshTypeVar ())

  fun checkRangeConstraint (pos, sgn0 :SGN) (T_f :TYPE) (constr :CONSTRAINT) =
    case (#RangeConstraint constr) of
      NONE => ()
    | SOME fExpr =>
      ( let val (pos', kind, T) = functionExpr fExpr (pos, sgn0)
	    val _ = if kind <> Static then err pos NonStaticRangeConstr else ()
            val _ = (unify (Sgn.typealias sgn0) (FuncType (domain T_f, Set (range T_f)), T))
		    handle _ => err pos (TypeMismatchRangeConstr
					 (Sgn.typealias sgn0, FuncType (domain T_f, Set (range T_f)), T))
        in () end )

  fun checkRecDef (checkFunctionExpr :FUNCTION_EXPR -> CTX.FUNCTION_EXPR -> T.FUNCTION_EXPR)
                  (pos, sgn0)
                  (kind, f_constr_fExpr_list :((OP_STATUS * string) * CONSTRAINT * FUNCTION_EXPR) list) =
    let val f_constr_fExpr_list_0 =
          map (fn (f, constr, fExpr as FunctionExprPos (_, LambdaTerm _)) => (f,
		    { TypeConstraint = SOME (getTypeConstraint constr),
		      RangeConstraint = #RangeConstraint constr,
		      DomainConstraint = #DomainConstraint constr }, fExpr)
                | (f, constr, FunctionExprPos (pos',_)) => err pos' LambdaDefExpected) f_constr_fExpr_list
        fun checkOneFunction sgn' (f, constr as { TypeConstraint = TC, ... }, fExpr) =
          let val (pos', fExpr_kind, T_fExpr) = checkFunctionExpr fExpr (pos, sgn') 
	      val _ = if kind = Static andalso fExpr_kind <> Static then err pos' StaticTermExpected else ()
              val constr' = strengthenTypeConstraint (pos', sgn') T_fExpr constr
          in (f, constr', fExpr)
          end
        fun step (f_constr_fExpr_list :((OP_STATUS * string) * CONSTRAINT * FUNCTION_EXPR) list) =
          let fun f_T_pair ((fOpSt, fName), constr :CONSTRAINT, fExpr) =
	        (fName, function (kind, fOpSt, valOf (#TypeConstraint constr)))
	      val sgn' =
		Sgn.override (sgn0, list2sgn pos (map f_T_pair f_constr_fExpr_list))
          in map (checkOneFunction sgn') f_constr_fExpr_list
          end
        val f_constr_fExpr_list' =
	  iterate (length f_constr_fExpr_list_0 + 1) step f_constr_fExpr_list_0
	val f_constr_fExpr_list_final =
	  map (fn (f, constr, fExpr) =>
	        ( f,
		  addTypeConstraint constr (SOME (normalize sgn0 (valOf (#TypeConstraint constr)))),
		  fExpr )) f_constr_fExpr_list'
    in (pos, list2sgn pos (map (fn ((fOpSt, fName), constr, fExpr) =>
		      (fName, function (kind, fOpSt, valOf (#TypeConstraint constr)))) f_constr_fExpr_list_final))
    end

  fun checkExtensionalFExpr constr (pos, sgn0) fExpr =
    let val (pos', fExpr_kind, T_fExpr) = functionExpr fExpr (pos, sgn0)
	val T0 = getTypeConstraint constr
	val T1 = checkConstraint sgn0 ((pos, T_fExpr), T0) err
	val _ = checkRangeConstraint (pos', sgn0) T1 constr
	val _ = enforceMonomorphicType pos' T1
	val T = normalize sgn0 T1
    in  ((pos', T_fExpr), T)
    end


  fun checkStaticFunctionDef (pos, sgn0) f_constr_fExpr_list =
  ( case f_constr_fExpr_list of
      [] =>
        bug pos "list of simultaneous functions has length 0!"
    | [((fOpStatus, fName), constr, fExpr as FunctionExprPos (fExpr_pos, LambdaTerm (p, t)))] =>
        checkRecDef functionExpr (pos, sgn0) (Static, f_constr_fExpr_list)
    | [((fOpStatus, fName), constr, fExpr as FunctionExprPos (fExpr_pos, fExpr'))] =>
        let val ((pos', T_fExpr), T) = checkExtensionalFExpr constr (pos, sgn0) fExpr
	in (pos, Sgn.singleton ( fName, function (Static, fOpStatus, T) ))
	end
    | _ =>
        checkRecDef functionExpr (pos, sgn0) (Static, f_constr_fExpr_list) )

  fun checkDerivedFunctionDef  (pos, sgn0) f_constr_fExpr_list =
    checkRecDef functionExpr (pos, sgn0) (Derived, f_constr_fExpr_list)

  fun checkExternalFunctionDef (pos, sgn0) [(f as (fOpSt, fName), constr, fExpr)] =
	let val T_f = getTypeConstraint constr
	    val _   = checkRangeConstraint (pos, sgn0) (T_f :TYPE) (constr :CONSTRAINT)
	    val _   = enforceMonomorphicType pos T_f
	    val T   = normalize sgn0 T_f
	in ( pos, Sgn.singleton ( fName, function (External, fOpSt, T) ) )
	end
    | checkExternalFunctionDef (pos,_) _ = 
        bug pos "simultaneous dynamic functions should be rejected by the parser!"

  fun checkDynamicFunctionDef (pos, sgn0) f_constr_fExpr_list =
  ( case f_constr_fExpr_list of
      [(_, _, FunctionExprPos (fExpr_pos, LambdaTerm _))] =>
        err fExpr_pos LambdaDefNotAllowed
    | [((fOpStatus, fName), constr, fExpr as FunctionExprPos (fExpr_pos, fExpr'))] =>
        let val ((pos', T_fExpr), T) = checkExtensionalFExpr constr (pos, sgn0) fExpr
	in (pos, Sgn.singleton ( fName, function (Dynamic, fOpStatus, T) ))
	end
    | _ => bug pos "simultaneous dynamic functions should be rejected by the parser!" )

  fun checkFunctionDef (pos, sgn0) (kind, f_constr_fExpr_list) =
    let val _ = map (fn ((_,f), _, _) => no_duplicate pos sgn0 f) f_constr_fExpr_list
    in case kind of
         CTL      => bug pos "CTL can not occur here"
       | Static   => checkStaticFunctionDef   (pos, sgn0) f_constr_fExpr_list
       | Derived  => checkDerivedFunctionDef  (pos, sgn0) f_constr_fExpr_list
       | External => checkExternalFunctionDef (pos, sgn0) f_constr_fExpr_list
       | Dynamic  => checkDynamicFunctionDef  (pos, sgn0) f_constr_fExpr_list
    end


 fun checkDef (D as DefPos (newPos, D') :ASM_AST.DEF) ((pos, sgn0) :CTX.DEF) : T.DEF =
    checkDef' D' (updatePos pos newPos, sgn0)

  and checkDef' D' (pos, sgn0) : T.DEF' =
    case D' of
      DefSeq Ds               => checkDefSeq checkDef (pos, sgn0) Ds
    | TypealiasDef args       => checkTypealiasDef (pos, sgn0) args
    | FreetypeDef tys         => checkFreetypeDef (pos, sgn0) tys
    | FunctionDef (kind, fns) => checkFunctionDef (pos, sgn0) (kind, fns)
    | RuleDef (name, TE)      => checkRuleDef (pos, sgn0) (name, TE)


  (* =================================================================== *\

       definition block (usually a sequence of definitions from a file)

  \* =================================================================== *)

  fun checkDefBlock (DefBlock (loc, D)) ((pos, sgn0) :CTX.DEF) : T.DEF =
    let val oldLoc = !currLoc
        val _      = currLoc := SOME loc
        val result = checkDef D (pos, sgn0)
        val _      = currLoc := oldLoc
    in result
    end


  (* =================================================================== *\

       exported functions

  \* =================================================================== *)

  val type_ :TYPE -> SGN -> TYPE =
    fn T => fn sgn => type_ T (NONE, sgn)

  val patt :PATT -> SGN -> ENV -> ENV * S.TYPE =
    fn p => fn sgn => fn env => ((fn p => (#2 p, #3 p)) (patt p (NONE, sgn, env)))

  val term :TERM -> SGN -> ENV -> S.TYPE =
    fn t => fn sgn => fn env => #3 (term t (NONE, sgn, env))

  val rule :RULE -> SGN -> ENV -> S.TYPE =
    fn t => fn sgn => fn env => #3 (rule t (NONE, sgn, env))

  val functionExpr :FUNCTION_EXPR -> SGN -> S.TYPE =
    fn FE => fn sgn => #3 (functionExpr FE (NONE, sgn))

  val transitionExpr :TRANSITION_EXPR -> SGN -> S.TYPE =
    fn TE => fn sgn => #2 (transitionExpr TE (NONE, sgn))

  val def :DEF -> SGN -> SGN =
    fn D => fn sgn => #2 (checkDef D (NONE, sgn))

  val defBlock :DEF_BLOCK -> SGN -> SGN =
    fn D => fn sgn => #2 (checkDefBlock D (NONE, sgn))
end
