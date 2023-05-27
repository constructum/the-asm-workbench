(* ******************************************************************* *\
 *
 *   The ASM Workbench for SML/NJ 1.10
 *
 *   File:         asm-eval.sml
 *
 *   Description: 
 *     Evaluator (interpreter) for ASM-SL
 *
 *   Author:       $Author: giusp $
 *   Date:         $Date: 2001/04/28 13:14:53 $
 *   Revision:     $Revision: 1.10 $
 *
\* ******************************************************************* *)

structure ASM_Eval =
struct

  open Misc ASM_AST

  structure Env  = ASM_Env
  structure Dict = ASM_Dictionary
  structure Run  = ASM_Run

  type SIGNATURE = ASM_Signature.SIGNATURE

  type VALUE = ASM_Value.VALUE

  type DICTIONARY = Dict.DICTIONARY

  type STATE = Run.STATE
  type ENV   = Env.ENV

  type UPDATE_SET = Run.UPDATE_SET

  (* =================================================================== *\

	error handling

  \* =================================================================== *)

  datatype PROBLEM =
    Impossible of string
  | InconsistentUpdates of ((string * VALUE) * VALUE * VALUE)
  | FunctionUndefined of (string * VALUE list)
  | TermUndefined
  | NotImplemented of string

  exception Error of (LOCATION option * POSITION option * PROBLEM) Error.ERROR

  fun message (location, position, problem) =
    let val R = String_.replace
	fun showType T = Pretty.toString 1000 (ASM_Pretty.type_ T)
	val where_ = String_.replace "$1:$2:\n"
		       [ ASM_Location.toErrorString (valOf location) handle Option => "--",
			 ASM_Position.toErrorString (valOf position) handle Option => "--" ]
	val what_  = case problem of
	  Impossible s     =>
            R "Impossible! This should not happen! (Probably a bug) [$1]" [s]
        | InconsistentUpdates ((f,x),y,y') => 
	    R "Inconsistent updates: $1\n\
              \                                          $2"
              (map ASM_Run.Update.toString [ ((f,x),y), ((f,x),y') ])
        | FunctionUndefined (f, xs) => 
            R "Function '$1' called with arguments: $2"
              [ f, ListFormat.fmt {final=" )\n", fmt=ASM_Value.toString, init="( ", sep=", "} xs ]
        | TermUndefined => 
            R "Value of term is undefined" []
        | NotImplemented s => 
            R "NotImplemented: $1" [ s ]
    in where_ ^ "  Run-time error -- " ^ what_ ^ "\n"
  end

  val currLoc = ref (NONE :LOCATION option)
  val _ = (ASM_Run.currLoc := currLoc)

  fun error fct pos what =
    raise Error
    { module = "ASM_Eval",
      function = fct,
      problem = (!currLoc, pos, what),
      message = message,
      cause = NONE }

  (* =================================================================== *\

	auxiliary functions (mostly shared for terms/rules)

  \* =================================================================== *)

  infix U
  fun env1 U env2 = Env.union (env1, env2)


  (* =================================================================== *\

       NOTE: the rest of the evaluator is defined using the
	 context-dependent transformation functors. Here the
         source, target and context types are defined.

  \* =================================================================== *)

  fun updatePos pos (newPos as SOME _) = newPos
    | updatePos pos (newPos as NONE)   = pos

  open ASM_Value
  type RUN = ASM_Run.RUN

  structure S = ASM_AST
  structure CTX = struct
    type TYPE = unit
    type NAME = unit

    type PATT = POSITION option
    type TERM = POSITION option * STATE * ENV
    type RULE = POSITION option * DICTIONARY * STATE * ENV

    type FUNCTION_EXPR   = POSITION option * STATE
    type TRANSITION_EXPR = POSITION option * DICTIONARY * STATE

    fun patt2type _ = ()
    fun patt2name _ = ()

    fun term2type (pos, S, env) = ()
    fun term2name (pos, S, env) = ()
    fun term2patt (pos, S, env) = pos

    fun rule2name (pos, dict, S, env) = ()
    fun rule2patt (pos, dict, S, env) = pos
    fun rule2term (pos, dict, S, env) = (pos, S, env)

    fun functionExpr2patt (pos, S) = pos
    fun functionExpr2term (pos, S) = (pos, S, Env.empty)

    fun transitionExpr2patt (pos, dict, S) = pos
  end


  structure T = struct
    type TYPE  = S.TYPE
    type NAME  = S.NAME  (* * S.FUNCTION_KIND option * S.TYPE *)

    type PATT  = VALUE -> ENV
    type PATT' = PATT

    type QUALIFIER = (int * ENV list) option      (* first elem. is card. of the whole set: used for univ. quantif. *)

    type TERM  = VALUE
    type TERM' = TERM

    type RULE  = UPDATE_SET
    type RULE' = RULE

    type FUNCTION_EXPR = VALUE -> VALUE
    type FUNCTION_EXPR' = FUNCTION_EXPR

    type TRANSITION_EXPR = VALUE -> UPDATE_SET
    type TRANSITION_EXPR' = TRANSITION_EXPR
  end

  (* =================================================================== *\

	pattern matching

  \* =================================================================== *)

  fun err x = error "match" x
  fun bug x s = error "match" x (Impossible s)

  structure PattEvalSpec :PATT_TRANSF_SPEC = struct
    structure S   = ASM_AST
    structure T   = T
    structure CTX = CTX

    fun h_type x _ = x
    fun h_name x _ = x

    fun h_patt patt' (PattPos (newPos, p')) pos =
      patt' p' (updatePos pos newPos)

    fun PattType (T, p) _ = p

    fun Placeholder _ x = Env.empty

    fun VarPatt v _ x = Env.singleton (v, x)

    fun AppPatt (f, p) _ x =
      let fun succeed cond = if cond then Env.empty else Env.FAIL
	  fun float_eq (r, r') = (Real.abs (r' - r)) < Real.minPos
      in case f of
           IntConst i    => succeed ( case x of INT i' => IntInf.fromInt i = i' | _ => false )
         | FloatConst r  => succeed ( case x of FLOAT r' => float_eq (r, r') | _ => false )
         | StringConst s => succeed ( case x of STRING s' => s = s' | _ => false )
         | Id f => ( case x of
                       UNDEF        => succeed ( f = "undef" )
                     | BOOL true    => succeed ( f = "true" )
                     | BOOL false   => succeed ( f = "false" )
		     | CELL (f', y) => if f = f' then p y else Env.FAIL
                     | _            => Env.FAIL )
	 | FunToMap f => Env.FAIL
	 | RelToSet f => Env.FAIL
      end

    fun TuplePatt ps _ x =
      case x of
	TUPLE xs => ( (Env.Union (Misc.zipWith (fn (p, x) => p x) (ps, xs))) handle _ => Env.FAIL )
      | _        => Env.FAIL
  end

  structure PattMatch = PattTransf (PattEvalSpec)
  fun match (p :PATT) (x :VALUE) :ENV = PattMatch.patt p NONE x


  (* =================================================================== *\

	evaluation of terms

  \* =================================================================== *)

  fun err x = error "term" x
  fun bug x s = error "term" x (Impossible s)


  (* =================================================================== *)

  fun function_to_finite_map f (pos, S, env) = ()   (* tbd *)
	
  (* =================================================================== *)
		      
  structure TermEvalSpec :TERM_TRANSF_SPEC = struct
    structure S   = ASM_AST
    structure T   = T
    structure CTX = CTX

    exception Undefined

    fun undefined defaultVal = 
      if !ASM_Global.undefDiscipline = ASM_Global.Restrictive
      then raise Undefined
      else defaultVal

    fun h_type x _ = x
    fun h_name x _ = x
    val h_patt     = PattMatch.patt

    open ASM_Global

    fun h_term term' (TermPos (newPos, t')) (pos, S, env) =
    ( ( debugMsg "h_term";
        if Env.isFAIL env
        then undefined UNDEF          (* !!! may be boolean or tuple !!! *)
        else term' t' (updatePos pos newPos, S, env) )
      handle ASM_Value.Prim.Prim (f, xs) => err (updatePos pos newPos) (FunctionUndefined (f, xs))
           | Undefined => err (updatePos pos newPos) TermUndefined )

    fun bind (pos, S, env) (p, t) = (pos, S, Env.override (env, p t))

    fun qualifier (p, UNDEF, G) (pos, S, env) = NONE
      | qualifier (p, A, G) (pos, S, env) =
          let fun F [] = []
		| F (x::rest) = let val env' = p x in
		    if not (Env.isFAIL env') andalso
                       (case G x of BOOL true => true | _ => false)
		    then env' :: F rest
		    else F rest end
	  in if isList A
	     then let val ML_A = ASM2ML.list A
                  in SOME (length ML_A, F ML_A)
                  end
	     else if isFSet A
	     then SOME (ASM_Value.Set.numItems A, F (Set.listItems A))
	     else NONE
	  end

    fun TermType (T, t) _ = t

    fun VarTerm v (pos, S, env)      = (debugMsg "VarTerm";   valOf (Env.find (env, v)) )
    fun AppTerm (f, t) (pos, S, env) = (debugMsg "AppTerm";   (S f) t  )
    fun TupleTerm ts (pos, S, env)   = (debugMsg "TupleTerm"; TUPLE ts )

    fun CondTerm ((G,t)::rest) (pos, S, env) =
        ( case G () of BOOL true => t () | _ => CondTerm rest (pos, S, env) )
      | CondTerm [] (pos, S, env) = undefined UNDEF      (* !!! may be boolean or tuple !!! *)

    fun LetTerm (p, t1, t2) (pos, S, env) = t2

    fun CaseTerm (t0, (p,t)::rest) (pos, S, env) =
          if not (Env.isFAIL (p t0)) then t () else CaseTerm (t0, rest) (pos, S, env)
      | CaseTerm (_, []) (pos, S, env) = undefined UNDEF    (* !!! may be boolean or tuple !!! *)

    fun ListComprTerm (t, qual as NONE) (pos, S, env) = undefined UNDEF
      | ListComprTerm (t, SOME (_, qEnvs)) (pos, S, env)   =
          ML2ASM.list (map (fn env' => t (pos, S, Env.override (env, env'))) qEnvs)

    fun FSetComprTerm (t, qual as NONE) (pos, S, env) = undefined UNDEF
      | FSetComprTerm (t, SOME (_, qEnvs)) (pos, S, env) =
          Set.fromList (map (fn env' => t (pos, S, Env.override (env, env'))) qEnvs)

    fun FMapComprTerm (t, qual as NONE) (pos, S, env) = undefined UNDEF
      | FMapComprTerm ((t1,t2), SOME (_, qEnvs)) (pos, S, env) =
          Map.fromList ( map (fn env' => let val env'' = Env.override (env, env')
                                         in (t1 (pos, S, env''), t2 (pos, S, env'')) end)
                             qEnvs )

    fun FSetEnumTerm  ts  _ = ASM_Value.Set.fromList ts
    fun FMapEnumTerm  tps _ = ASM_Value.Map.fromList tps

    fun ForallTerm (qual as NONE) _      = undefined (BOOL false)
      | ForallTerm (SOME (card, qEnvs)) (pos, S, env) = if length qEnvs = card then Prim.True else Prim.False

    fun ExistsTerm (qual as NONE) _      = undefined (BOOL false)
      | ExistsTerm (SOME (_, qEnvs)) (pos, S, env) = if length qEnvs <> 0 then Prim.True else Prim.False
  end

  structure TermEval = TermTransf (TermEvalSpec)
  val term = TermEval.term


  (* =================================================================== *\

	evaluation of rules

  \* =================================================================== *)

  fun err x = error "rule" x
  fun bug x s = error "rule" x (Impossible s)

  structure RuleEvalSpec :RULE_TRANSF_SPEC = struct
    structure S   = ASM_AST
    structure T   = T
    structure CTX = CTX

    fun h_type x _ = x
    fun h_name x _ = x
    val h_patt     = PattMatch.patt
    val h_term     = TermEval.term

    open ASM_Global
    structure UpdateSet = ASM_Run.UpdateSet
    structure ValueMap  = ASM_Run.ValueMap

    fun h_rule rule' (RulePos (newPos, t')) (pos, dict, S, env) =
    ( ( debugMsg "h_rule";
        if Env.isFAIL env
        then UpdateSet.empty
        else rule' t' (updatePos pos newPos, dict, S, env) )
      handle UpdateSet.Inconsistent ((f,x),y,y') => err (updatePos pos newPos) (InconsistentUpdates ((f,x),y,y')) )

    fun bind (pos, dict, S, env) (p, t) = (pos, dict, S, Env.override (env, p t))

    fun qualifier qual (pos, dict, S, env)  = TermEvalSpec.qualifier qual (pos, S, env)

    fun UpdateRule ((Id fName, t), t') _ = UpdateSet.singleton (fName, ValueMap.singleton (t, t'))
      | UpdateRule _ _ = bug NONE "constant on lhs of update rule"

    fun BlockRule Rs (pos,_,_,_) =
    ( ( UpdateSet.Union Rs ) )
      

    fun CondRule ((G,R)::rest) (pos, dict, S, env) =
        ( case G () of BOOL true => R () | _ => CondRule rest (pos, dict, S, env) )
      | CondRule [] _ = UpdateSet.empty

    fun LetRule (p, R1, R2) _ = R2

    fun CaseRule (t0, (p,R)::rest) (pos, dict, S, env) =
          if not (Env.isFAIL (p t0)) then R () else CaseRule (t0, rest) (pos, dict, S, env)
      | CaseRule (_, []) (pos, dict, S, env) = UpdateSet.empty

    fun ForallRule (qual as NONE, R) (pos, dict, S, env) = UpdateSet.empty
      | ForallRule (SOME (_, qEnvs), R) (pos, dict, S, env) =
          UpdateSet.Union (map (fn env' => R (pos, dict, S, Env.override (env, env'))) qEnvs)

    fun ChooseRule (qual as NONE, R) (pos, dict, S, env) = UpdateSet.empty
      | ChooseRule (SOME (_, qEnvs), R) (pos, dict, S, env) =
          let val env' = List.nth (qEnvs, RandomChoice.random (0, length qEnvs - 1))
          in R (pos, dict, S, Env.override (env, env'))
          end

    fun AppRule (Id rName, R) (pos, dict, S, env) =
        ( (Run.transitionInterpretation (dict, S) rName R)
          handle Run.TransitionInterpretation s => bug NONE ( String_.replace
			         "unbound rule name '$1' (type checker should reject this)" [ s ]) )
      | AppRule _ _ = bug NONE "constant on lhs of rule application"
  end

  structure RuleEval = RuleTransf (RuleEvalSpec)
  val rule = RuleEval.rule


  (* =================================================================== *\

	evaluation of function expressions & transition expressions

  \* =================================================================== *)

  structure FunctionExprEvalSpec :FUNCTION_EXPR_TRANSF_SPEC = struct
    structure S   = ASM_AST
    structure T   = T
    structure CTX = CTX

    fun h_type x _ = x
    fun h_name x _ = x
    val h_patt     = PattMatch.patt
    val h_term     = TermEval.term
    val h_rule     = RuleEval.rule

    fun h_functionExpr functionExpr' (FunctionExprPos (newPos, FE')) (pos, S) =
      functionExpr' FE' (updatePos pos newPos, S)

    fun LambdaTerm (p, t) (pos, S) =
      fn x => t (pos, S, p x)

    fun SetToRel t (pos, S) = fn x =>
      ( (BOOL (Set.member (t, x)))      (* ATTENTION: args of Set.member not as expected! *)
        handle _ => BOOL false )

    fun MapToFun t (pos, S) = fn x =>
      (( case Map.find (t, x) of
           SOME x => x
         | NONE   => UNDEF ) handle _ => UNDEF )
  end


  structure TransitionExprEvalSpec :TRANSITION_EXPR_TRANSF_SPEC = struct
    structure S   = ASM_AST
    structure T   = T
    structure CTX = CTX

    fun h_type x _ = x
    fun h_name x _ = x
    val h_patt     = PattMatch.patt
    val h_term     = TermEval.term
    val h_rule     = RuleEval.rule


    fun h_transitionExpr transitionExpr' (TransitionExprPos (newPos, FE')) (pos, dict, S) =
      transitionExpr' FE' (updatePos pos newPos, dict, S)

    fun LambdaRule (p, R) (pos, dict, S) =
      fn x => R (pos, dict, S, p x)
  end

  structure FunctionExprEval = FunctionExprTransf (FunctionExprEvalSpec)
  val functionExpr = FunctionExprEval.functionExpr

  structure TransitionExprEval = TransitionExprTransf (TransitionExprEvalSpec)
  val transitionExpr = TransitionExprEval.transitionExpr


  (* =================================================================== *\

       definitions (not inductively defined)

  \* =================================================================== *)

  fun err x = error "def" x
  fun bug x s = error "def" x (Impossible s)

  fun evalDefSeq evalDef dict0 Ds =
    let fun evalDefs (dict0 :DICTIONARY) ([] :DEF list) = Dict.empty
          | evalDefs dict0 (D :: D_rest) =
              let val dict'  = evalDef D dict0
	          val dict'' = evalDefs (Dict.override (dict0, dict')) D_rest
              in Dict.override (dict', dict'')
	      end
    in evalDefs dict0 Ds
    end

  fun evalTypealiasDef dict0 pos (TName, params, T) =
    let val aliasPair = (BaseType (TName, map TypeParam params), T)
    in Dict.addType (Dict.empty, TName, ((!currLoc, pos), Dict.TypeAlias aliasPair))
    end

  fun evalFreetypeDef dict0 pos name_pars_conss_list =
    let fun addConstructors dict0 op_name_type_list =
          List_.foldll
            (fn (dict, ((_,name),_)) => Dict.addFunction (dict, name, ((!currLoc, pos), Dict.ASM_SL_DataConstructor)))
	    dict0 op_name_type_list
        fun addOneDatatype (dict, (name, params, conss)) = 
	  let val dict' = addConstructors dict conss
          in Dict.addType ( dict', name,
			    ((!currLoc, pos), Dict.FreetypeCarrier (map (fn ((_,name),_) => name) conss)) )
          end
    in List_.foldll addOneDatatype dict0 name_pars_conss_list
    end

  fun evalFunctionDef (sign :SIGNATURE) dict0 pos (kind, fns) =
    let val makeFuncDict = 
          List_.foldll (fn (dict,(key,fct)) => Dict.addFunction (dict,key,fct)) Dict.empty
    in ( case kind of
	 Static => makeFuncDict
           (map (fn ((_,f),_,FE) => (f, ((!currLoc, pos), Dict.ASM_SL_StaticFunction { definition = FE }))) fns)
       | Dynamic => makeFuncDict
           ( map (fn ((_,f),C,FE) => (f, ((!currLoc, pos),
             Dict.ASM_SL_DynamicFunction
               { constraint = addTypeConstraint C (ASM_Signature.typeOf (ASM_Signature.find sign f)),
		 initially = FE }))) fns )
       | Derived => makeFuncDict
           (map (fn ((_,f),_,FE) => (f, ((!currLoc, pos), Dict.ASM_SL_DerivedFunction { definition = FE }))) fns)
       | External => makeFuncDict
           (map (fn ((_,f),C,_) => (f, ((!currLoc, pos), Dict.ASM_SL_ExternalFunction { constraint = C }))) fns)
       | CTL => bug pos "CTL can not occur here" )
    end

				   
  fun evalRuleDef dict0 pos (rName, TE) =
    Dict.addNamedRule (dict0, rName, ((!currLoc, pos), Dict.ASM_SL_NamedRule { definition = TE }))



  fun evalDef (sign :SIGNATURE)
              (D as DefPos (pos, D') :DEF) (dict :DICTIONARY) :DICTIONARY =
    evalDef' sign D' dict pos

  and evalDef' (sign :SIGNATURE)
               D (dict0 :DICTIONARY) (pos :POSITION option) :DICTIONARY =
    case D of
      DefSeq Ds               => evalDefSeq (evalDef sign) dict0 Ds
    | TypealiasDef args       => evalTypealiasDef dict0 pos args
    | FreetypeDef tys         => evalFreetypeDef dict0 pos tys
    | FunctionDef (kind, fns) => evalFunctionDef sign dict0 pos (kind, fns)
    | RuleDef (name, TE)      => evalRuleDef dict0 pos (name, TE)


  (* =================================================================== *\

       definition block (usually a sequence of definitions from a file)

  \* =================================================================== *)

  fun evalDefBlock (sign :SIGNATURE) (DefBlock (loc, D)) (dict :DICTIONARY) :DICTIONARY =
    let val oldLoc = !currLoc
        val _      = currLoc := SOME loc
        val result = evalDef sign D dict
        val _      = currLoc := oldLoc
    in result
    end


  (* =================================================================== *\

       exported functions

  \* =================================================================== *)

  val term :TERM -> STATE * ENV -> VALUE =
    fn t => fn (S, env) => term t (NONE, S, env)

  val rule :RULE -> DICTIONARY * STATE * ENV -> UPDATE_SET =
    fn R => fn (dict, S, env) => rule R (NONE, dict, S, env)

  val functionExpr :FUNCTION_EXPR -> STATE -> (VALUE -> VALUE) =
    fn FE => fn S => functionExpr FE (NONE, S)

  val transitionExpr :TRANSITION_EXPR -> DICTIONARY * STATE -> (VALUE -> UPDATE_SET) =
    fn TE => fn (dict, S) => transitionExpr TE (NONE, dict, S)

  val def :SIGNATURE -> DEF -> DICTIONARY -> DICTIONARY =
    evalDef

  val defBlock :SIGNATURE -> DEF_BLOCK -> DICTIONARY -> DICTIONARY =
    fn D => fn dict => evalDefBlock D dict

  val _ = (ASM_Run.evalTerm  := term)
  val _ = (ASM_Run.evalFExpr := functionExpr)
  val _ = (ASM_Run.evalTExpr := transitionExpr)
end (* of structure ASM_Eval *)
