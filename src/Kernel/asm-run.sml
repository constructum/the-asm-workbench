signature ASM_RUN =
sig
  type PROBLEM
  exception Error of (ASM_AST.LOCATION option * ASM_AST.POSITION option * PROBLEM) Error.ERROR
  val message :ASM_AST.LOCATION option * ASM_AST.POSITION option * PROBLEM -> string

  type VALUE      = ASM_Value.VALUE
  type DICTIONARY = ASM_Dictionary.DICTIONARY

  structure ValueSet :sig
    include ORD_SET
(*  val fromList :VALUE list -> set *)
  end

  structure ValueMap :sig
    include ORD_MAP
    val fromList :(VALUE * VALUE) list -> VALUE map 
  end

  type FINITE_SET = ValueSet.set
  type FINITE_MAP = VALUE ValueMap.map

  structure Update :sig
    type UPDATE = ((string * VALUE) * VALUE)

    val toString :UPDATE -> string
  end

  structure UpdateSet :sig
    type UPDATE = Update.UPDATE
    type UPDATE_SET = FINITE_MAP StringMap.map
    exception Inconsistent of ((string * VALUE) * VALUE * VALUE)

    val empty     :UPDATE_SET
    val singleton :string * FINITE_MAP -> UPDATE_SET

    val union     :UPDATE_SET * UPDATE_SET -> UPDATE_SET
    val Union     :UPDATE_SET list -> UPDATE_SET

    val toList    :UPDATE_SET -> UPDATE list
    val toString  :UPDATE_SET -> string
  end

  structure ExternalStateElem :sig
    type EXTERNAL_STATE_ELEM = ((string * VALUE) * VALUE)

    val toString :EXTERNAL_STATE_ELEM -> string
  end

  structure ExternalState :sig
    type EXTERNAL_STATE_ELEM = ExternalStateElem.EXTERNAL_STATE_ELEM
    type EXTERNAL_STATE = FINITE_MAP StringMap.map

    val empty     :EXTERNAL_STATE

    val toList    :EXTERNAL_STATE -> EXTERNAL_STATE_ELEM list
    val toString  :EXTERNAL_STATE -> string
  end

  type UPDATE_SET     = UpdateSet.UPDATE_SET
  type EXTERNAL_STATE = ExternalState.EXTERNAL_STATE

  type STATE = ASM_AST.NAME -> (VALUE -> VALUE)
  type ENV   = ASM_Env.ENV

  exception TransitionInterpretation of string
  val transitionInterpretation :DICTIONARY * STATE -> string -> VALUE -> UPDATE_SET

  val currLoc   :ASM_AST.LOCATION option ref ref
  val evalTerm  :(ASM_AST.TERM -> STATE * ENV -> VALUE) ref
  val evalFExpr :(ASM_AST.FUNCTION_EXPR -> STATE -> VALUE -> VALUE) ref
  val evalTExpr :(ASM_AST.TRANSITION_EXPR -> DICTIONARY * STATE -> VALUE -> UPDATE_SET) ref

  type RUN

  datatype ORACLE_OPTION = OracleProcess | RandomChoice

  val setOracleOption :ORACLE_OPTION -> unit

  val newRun        :DICTIONARY -> RUN
  val initialState  :RUN -> STATE
  val currentState  :RUN -> STATE
  val step          :RUN * (unit -> UPDATE_SET) -> RUN * EXTERNAL_STATE * UPDATE_SET
  val back          :RUN -> RUN
  val reinit        :RUN -> RUN
  val gotoStage     :int -> RUN * (unit -> UPDATE_SET) -> RUN
  val discardFuture :RUN -> RUN
  val currStage     :RUN -> int
  val lastStage     :RUN -> int
  val dictionary    :RUN -> DICTIONARY
  val lastExternalStateAndUpdateSet :RUN -> EXTERNAL_STATE * UPDATE_SET
  val currentExternalState          :RUN -> EXTERNAL_STATE

  val dynamicFunctionToMap :RUN * string -> FINITE_MAP
  val dynamicRelationToSet :RUN * string -> FINITE_SET
end



structure ASM_Run (* !!!! :ASM_RUN !!!! *) =
struct
  val debugMsg = ASM_Global.debugMsg
  open ASM_AST

  type VALUE = ASM_Value.VALUE


  structure Location = struct
    type LOCATION = string * VALUE

    fun toString (f, ASM_Value.TUPLE [])  = f
      | toString (f, ASM_Value.TUPLE [x]) = f ^ " (" ^ (ASM_Value.toString x) ^ ")"
      | toString (f, args as ASM_Value.TUPLE _) = f ^ " " ^ (ASM_Value.toString args)
      | toString (f, arg) = f ^ " (" ^ (ASM_Value.toString arg) ^ ")"
  end

  (* =================================================================== *\

	error handling

  \* =================================================================== *)

  datatype PROBLEM =
    Impossible of string
  | ExternalFunctionInInitialState of string
  | RandomChoiceWithoutFinitenessConstraint of string * VALUE
  | OracleProcessUndefined

  exception Error of (LOCATION option * POSITION option * PROBLEM) Error.ERROR

  fun message (location, position, problem) =
    let val R = String_.replace
	fun showType T = Pretty.toString 1000 (ASM_Pretty.type_ T)
	val where_ = String_.replace "$1:$2:\n"
		       [ ASM_Location.toErrorString (valOf location) handle Option => "--",
			 ASM_Position.toErrorString (valOf position) handle Option => "--" ]
	val what_  = case problem of
	  Impossible s =>
            R "Impossible! This should not happen! (Probably a bug) [$1]" [s]
        | ExternalFunctionInInitialState fName =>
            R "value of external function $1 in initial state not available" [ fName ]
        | RandomChoiceWithoutFinitenessConstraint (f, x) =>
            R "Trying to choose random value for $1,\n\
              \    but finiteness constraint for '$2' is empty or not defined"
              [ Location.toString (f, x), f ]
        | OracleProcessUndefined =>
            R "Oracle process not defined" [ ]
    in where_ ^ "  Run-time error -- " ^ what_ ^ "\n"
  end

  val currLoc = ref (ref (NONE :LOCATION option))        (* should point to currLoc of structure ASM_Eval *)

  fun error fct pos what =
    raise Error
    { module = "ASM_Run",
      function = fct,
      problem = (!(!currLoc), pos, what),
      message = message,
      cause = NONE }

  fun impossible fct s =
    error fct NONE (Impossible s)


  (* =================================================================== *)

  structure ValueSet = struct
    structure Set = BinarySetFn ( struct
      type ord_key = VALUE
      val compare  = ASM_Value.compare
    end )
    open Set
    fun fromList L = List.foldl add' empty L
  end

  structure ValueMap = struct
    structure Map = BinaryMapFn ( struct
      type ord_key = VALUE
      val compare  = ASM_Value.compare
    end )
    open Map
    fun fromList L = List.foldl insert' empty L
  end

  type FINITE_SET = ValueSet.set
  type FINITE_MAP = VALUE ValueMap.map

  fun mapToSet (map :FINITE_MAP) :FINITE_SET =
    let open ASM_Value
    in ValueMap.foldli
	 (fn (x, y, set) => if compare (y, Prim.True) = EQUAL then ValueSet.add (set, x) else set) ValueSet.empty
	 map
    end

  fun ML_FiniteMap_to_ASM_Value_Map m =          (* to be used for implementing FUN_TO_MAP *)
    (ASM_Value.Map.fromList o ValueMap.listItemsi) m

  fun ML_FiniteMap_to_ASM_Value_Set m =          (* to be used for implementing REL_TO_SET *)
    (ASM_Value.Set.fromList o ValueSet.listItems o mapToSet) m


  (* --- consistent update sets are represented by the following type UPDATE_SET --- *)
  (* --- inconsistent updates sets are not explicitly represented, inconsistency generates exception --- *)
  structure Update = struct
    type UPDATE = ((string * VALUE) * VALUE)

    fun toString (((f, x), y) :UPDATE) :string =
      let open ASM_Value
      in case (x, y) of
	   (TUPLE [], y)  => f ^ " := " ^ (toString y)
	 | (TUPLE [x], y) => f ^ " (" ^ (toString x) ^ ") := " ^ (toString y)
	 | (TUPLE _, y)   => f ^ " " ^ (toString x) ^ " := " ^ (toString y)
	 | (_, _)         => f ^ " (" ^ (toString x) ^ ") := " ^ (toString y)
      end
  end

  structure UpdateSet = struct
    type UPDATE = Update.UPDATE
    type UPDATE_SET = FINITE_MAP StringMap.map
    exception Inconsistent of ((string * VALUE) * VALUE * VALUE)

    val empty     = StringMap.empty
    val singleton = StringMap.singleton

    fun union (US, US') = 
      let fun conflict f (x, y, y') =
            raise Inconsistent ((f, x), y, y') 
          fun union_US_f (f, US_f) =
	    case StringMap.find (US', f) of
              SOME US_f' => ValueMap.unionWithi (conflict f) (US_f, US_f')
            | NONE       => US_f
          val only_in_US' =
            StringMap.filteri (fn (f,_) => not (isSome (StringMap.find (US, f)))) US'
      in StringMap.unionWith #2 ( StringMap.mapi union_US_f US, only_in_US' )
      end

    fun Union USs =
      List.foldl union empty USs

    fun toList (US :UPDATE_SET) :UPDATE list =
      let fun G (f, upds) = map (fn (x, y) => ((f, x), y)) (ValueMap.listItemsi upds)
      in List.concat (map G (StringMap.listItemsi US))
      end

    fun toString (US :UPDATE_SET) =
      let fun F f (x, y) = Update.toString ((f, x), y)
          fun G (f, upds) = ListFormat.fmt { init="", sep=", ", final="", fmt = F f } (ValueMap.listItemsi upds)
      in ListFormat.fmt { init="{ ", sep=", ", final=" }", fmt = Misc.id } (map G (StringMap.listItemsi US))
      end
  end


  structure ExternalStateElem = struct
    type EXTERNAL_STATE_ELEM = ((string * VALUE) * VALUE)

    fun toString (((f, x), y) :EXTERNAL_STATE_ELEM) :string =
      let open ASM_Value
      in case (x, y) of
	   (TUPLE [], y)  => f ^ " = " ^ (toString y)
	 | (TUPLE [x], y) => f ^ " (" ^ (toString x) ^ ") = " ^ (toString y)
	 | (TUPLE _, y)   => f ^ " " ^ (toString x) ^ " = " ^ (toString y)
	 | (_, _)         => f ^ " (" ^ (toString x) ^ ") = " ^ (toString y)
      end
  end

  structure ExternalState = struct
    type EXTERNAL_STATE_ELEM = ExternalStateElem.EXTERNAL_STATE_ELEM
    type EXTERNAL_STATE = FINITE_MAP StringMap.map
    open StringMap

    val toList = UpdateSet.toList

    fun toString (ES :EXTERNAL_STATE) =
      let fun F f (x, y) = ExternalStateElem.toString ((f, x), y)
          fun G (f, upds) = ListFormat.fmt { init="", sep=", ", final="", fmt = F f } (ValueMap.listItemsi upds)
      in ListFormat.fmt { init="{ ", sep=", ", final=" }", fmt = Misc.id } (List.map G (StringMap.listItemsi ES))
      end
  end


  type UPDATE_SET = UpdateSet.UPDATE_SET
  type EXTERNAL_STATE = ExternalState.EXTERNAL_STATE


  open ASM_Dictionary

  datatype ORACLE_OPTION =       (* where do values of external functions come from ? *)
    OracleProcess
  | RandomChoice

  type OPTIONS =
  { memoFlag     : bool,
    saveHistory  : bool,
    oracleOption : ORACLE_OPTION }

  val defaultOptions :OPTIONS ref =
    ref { memoFlag     = true,
	  saveHistory  = true,
	  oracleOption = OracleProcess }

  fun setMemoFlag b    = let val {memoFlag,saveHistory,oracleOption} = !defaultOptions
			 in defaultOptions := {memoFlag=b, saveHistory=saveHistory, oracleOption=oracleOption} end

  fun setSaveHistory b = let val {memoFlag,saveHistory,oracleOption} = !defaultOptions
			 in defaultOptions := {memoFlag=memoFlag, saveHistory=b, oracleOption=oracleOption} end

  fun setOracleOption O = let val {memoFlag,saveHistory,oracleOption} = !defaultOptions
		   	  in defaultOptions := {memoFlag=memoFlag, saveHistory=saveHistory, oracleOption=O} end


  (* --- FUNCTION_HISTORY: history of ONE function --- *)

  type STATIC_FUNCTION_HISTORY = 
  { interpretation : VALUE -> VALUE,
    cache          : (FINITE_MAP ref) option }

  type EXTERNAL_FUNCTION_HISTORY =
  { type_                 : TYPE,
    finitenessConstraint  : (VALUE -> VALUE) option,    (* range is a finite set *)
    pastInterpretations   : FINITE_MAP list,            (* incomplete interpretations: *)
    currentInterpretation : FINITE_MAP ref,             (* only locations needed to compute this step *)
    futureInterpretations : FINITE_MAP list }           (* transition at the given state *)

  datatype HISTORY = History of
  { static   : STATIC_FUNCTION_HISTORY StringMap.map,
    dynamic  : { initialInterpretation : RUN -> FINITE_MAP,
		 pastUpdates           : FINITE_MAP list,
		 currentInterpretation : RUN -> FINITE_MAP,
		 futureUpdates         : FINITE_MAP list,
                 defaultValue          : VALUE } StringMap.map,
    external : EXTERNAL_FUNCTION_HISTORY StringMap.map,
    derived  : { initialInterpretation : RUN -> VALUE -> VALUE,
		 currentInterpretation : RUN -> VALUE -> VALUE } StringMap.map }

  and RUN = Run of
  { dict : DICTIONARY ref,
    hist : HISTORY,
    currStage : int,
    lastStage : int,
    options   : OPTIONS ref }

  type DYNAMIC_FUNCTION_HISTORY =
  { initialInterpretation : RUN -> FINITE_MAP,
    pastUpdates           : FINITE_MAP list,
    currentInterpretation : RUN -> FINITE_MAP,
    futureUpdates         : FINITE_MAP list,
    defaultValue          : VALUE }

  type DERIVED_FUNCTION_HISTORY =
  { initialInterpretation : RUN -> VALUE -> VALUE,
    currentInterpretation : RUN -> VALUE -> VALUE }

  datatype FUNCTION_HISTORY =
    StaticFunction   of STATIC_FUNCTION_HISTORY
  | DynamicFunction  of DYNAMIC_FUNCTION_HISTORY
  | ExternalFunction of EXTERNAL_FUNCTION_HISTORY
  | DerivedFunction  of DERIVED_FUNCTION_HISTORY



  fun findInHistory (hist as History hist' :HISTORY, name :string) =
  ( case StringMap.find (#static hist', name) of
      SOME x => SOME (StaticFunction x)
    | NONE =>
      ( case StringMap.find (#derived hist', name) of
	  SOME x => SOME (DerivedFunction x)
	| NONE =>
	  ( case StringMap.find (#dynamic hist', name) of
	    SOME x => SOME (DynamicFunction x)
	  | NONE =>
	    ( case StringMap.find (#external hist', name) of
		SOME x => SOME (ExternalFunction x)
	      | NONE => NONE ) ) ) )

  fun staticHistory   (History {static,...})   = static
  fun dynamicHistory  (History {dynamic,...})  = dynamic
  fun externalHistory (History {external,...}) = external
  fun derivedHistory  (History {derived,...})  = derived

  fun historyOverride (hist1 as History hist1', hist2 as History hist2') =
    History { static   = StringMap.unionWith #2 (#static hist1',   #static hist2'),
	      dynamic  = StringMap.unionWith #2 (#dynamic hist1',  #dynamic hist2'),
	      external = StringMap.unionWith #2 (#external hist1', #external hist2'),
	      derived  = StringMap.unionWith #2 (#derived hist1',  #derived hist2') }


  (* --- a state is given by interpretations of function names --- *)

  type STATE = NAME -> (VALUE -> VALUE)
  type ENV   = ASM_Env.ENV


  local
    open ASM_Value
  in
    (* --- "evalFExpr" below must be set in the structure ASM_Eval ("backpatching"),  --- *)
    (* --- as soon as the corresponding function of the evaluator is defined          --- *)
    val evalTerm :(TERM -> STATE * ENV -> VALUE) ref =
      ref ( fn _ => fn _ => impossible "ASM_Run.evalTerm" "still undefined [bug]" )

    (* --- "evalFExpr" below must be set in the structure ASM_Eval ("backpatching"),  --- *)
    (* --- as soon as the corresponding function of the evaluator is defined          --- *)
    val evalFExpr :(FUNCTION_EXPR -> STATE -> (VALUE -> VALUE)) ref =
      ref ( fn _ => fn _ => fn _ => impossible "ASM_Run.evalFEexpr" "still undefined [bug]" )

    (* --- same for "evalTExpr" --- *)
    val evalTExpr :(TRANSITION_EXPR -> DICTIONARY * STATE -> (VALUE -> UPDATE_SET)) ref =
      ref ( fn _ => fn _ => fn _ => impossible "ASM_Run.evalTEexpr" "still undefined [bug]" )

    (* --- !!!!!!!! memo flag not implemented yet !!!!!!!! *)
    fun conditionalCache (cond :bool) = 
      if cond then SOME (ref ValueMap.empty) else NONE

    fun constructorInterpretation fName (x as TUPLE _) = CELL (fName, normalized x)
      | constructorInterpretation fName (x as _)       = CELL (fName, x)

(*
    fun staticFunctionInterpretation state fName x =
      (!evalFExpr) fExpr state x
      (* !!!! add caching at some point !!!! *)
*)

    fun skipLoc (SOME ((optLoc,optPos), x)) = SOME x
      | skipLoc NONE = NONE

    fun updateLoc (SOME ((optLoc,optPos), x)) = (!currLoc := optLoc; SOME x)
      | updateLoc NONE = NONE


    fun determineDefaultValueForDynamicFunction fName (constraint :CONSTRAINT) :VALUE =
    ( let val T  = ASM_Type.range (valOf (#TypeConstraint constraint))
          val R  = String_.replace
      in ASM_Value.defaultValueOfType T
      end handle Option => impossible "ASM_Run.determineDefaultValueForDynamicFunction"
	                              ("no type constraint for dynamic function '"^fName^"'") )


    fun getNewExternalFunctionValue (fName :string, dict :DICTIONARY ref) (options :OPTIONS ref)
                                    ({finitenessConstraint,type_,...} :EXTERNAL_FUNCTION_HISTORY)
                                    (x :VALUE) =
      let val error = error "ASM_Run.getExternalFunctionValue"
      in case (#oracleOption (!options)) of
	   RandomChoice  =>
	   ( let val range  = (valOf finitenessConstraint) x
		 val elemNo = RandomChoice.random (0, Set.numItems range - 1)
	     in List.nth (Set.listItems range, elemNo)
	     end handle _ => error (SOME (#2 (valOf (ASM_Dictionary.locate (!dict, fName)))) handle _ => NONE)
		                   (RandomChoiceWithoutFinitenessConstraint (fName, x)) )
	 | OracleProcess =>
            (* error NONE OracleProcessUndefined *)
           ( let val T = ASM_Type.range type_
             in ASM_Oracle.consult_oracle_process T (fName, x)
             end )
      end
	  
    fun special_operator calling_fct (operator_name, fName) (interpretation_field, conversion_to_asm_value) (run as Run {dict,hist,options,...}) =
       fn _ =>   (* this is for FUN_TO_MAP and REL_TO_ SET *)
	  ( case (findInHistory (hist, fName)) of
		SOME (DynamicFunction dynamic_function_history) => conversion_to_asm_value ((interpretation_field dynamic_function_history) run)
	             (* !!! special case if domain of function is BOOL ????? !!!!!! *)
	      | SOME (StaticFunction _) => ASM_Global.error calling_fct (operator_name^" applied to static function "^fName) NONE
	             (* !!! should be actually allowed if static fct extensionally defined [tbd] *)
	      | SOME (DerivedFunction _) => ASM_Global.error calling_fct (operator_name^" applied to derived function "^fName) NONE
	      | SOME (ExternalFunction _) => ASM_Global.error calling_fct (operator_name^" applied to external function "^fName) NONE
	      | NONE => ASM_Global.error calling_fct ("function not found: "^fName) NONE )
	  
    fun staticState (dict :DICTIONARY ref, memoFlag :bool) :STATE = 
      let fun thisState () = staticState (dict, memoFlag)
	  val _ = debugMsg "staticState"
      in fn IntConst i    => ( fn _ => INT (IntInf.fromInt i) )
          | FloatConst r  => ( fn _ => FLOAT r )
          | StringConst s => ( fn _ => STRING s )
	  | FunToMap fName => ( fn _ => (* !!!!!! *)  ASM_Global.error "ASM_Run.staticState" ("not static: FUN_TO_MAP " ^ fName) NONE )
	  | RelToSet fName => ( fn _ => (* !!!!!! *)  ASM_Global.error "ASM_Run.staticState" ("not static: REL_TO_SET " ^ fName) NONE )
          | Id fName      => ( fn x => 
	    ( case skipLoc (ASM_Dictionary.findFunction (!dict, fName)) of
	       SOME (ML_StaticFunction f)     => (#interpretation f) x
	     | SOME (ASM_SL_DataConstructor)  => constructorInterpretation fName x
	     | SOME (ASM_SL_StaticFunction f) => (!evalFExpr) (#definition f) (thisState ()) x
	     | _ => ASM_Global.error "ASM_Run.staticState" ("function name not static: "^fName^"[bug]") NONE ) )
      end

    and initialState (run as Run {dict,hist,options,...}) :STATE = 
      let val _ = debugMsg "initialState"
      in fn Id fName => ( fn x => 
	    ( case (findInHistory (hist, fName)) of
	       SOME (StaticFunction {interpretation,cache}) =>       (*!!!!! cache to be implemented !!! !*)
                 interpretation x
	     | SOME (DynamicFunction {initialInterpretation,defaultValue,...}) =>
               ( ( case ValueMap.find (initialInterpretation run, x) of
		     SOME y => y
                   | NONE   => defaultValue ) handle _ => defaultValue )
	     | SOME (DerivedFunction {initialInterpretation,...}) =>
	         initialInterpretation run x
	     | SOME (ExternalFunction (extFctHist as {pastInterpretations,currentInterpretation,finitenessConstraint,...})) =>
               ( case pastInterpretations of
                   [] =>        (* initial state: choice of new external function values possible! *)
                   ( case ValueMap.find (!currentInterpretation, x) of
		       SOME y => y
		     | NONE =>
			 let val y = getNewExternalFunctionValue (fName, dict) options extFctHist x
			     val _ = currentInterpretation := ValueMap.insert (!currentInterpretation, x, y)
			 in y end )
                 | _ =>         (* not initial state: look if appropriate value available in history, otherwise fail! *)
                   ( let val initialInterpretation = List.last pastInterpretations
		     in case ValueMap.find (initialInterpretation, x) of
			  SOME y => y
			| NONE => error "ASM_Run.initialState" NONE (ExternalFunctionInInitialState fName)
		     end ) )
             | NONE => ASM_Global.error "ASM_Run.initialState" ("function not found: "^fName) NONE ) )
       | FunToMap f => special_operator "ASM_Run.initialState" ("FUN_TO_MAP", f) (#initialInterpretation, ML_FiniteMap_to_ASM_Value_Map) run
       | RelToSet f => special_operator "ASM_Run.initialState" ("REL_TO_SET", f) (#initialInterpretation, ML_FiniteMap_to_ASM_Value_Set) run
       | constant as IntConst _ => staticState (dict, false) constant
       | constant as FloatConst _ => staticState (dict, false) constant
       | constant as StringConst _ => staticState (dict, false) constant
      end

    and currentState (run as Run {dict,hist,options,...}) :STATE = 
      let fun thisState () = currentState run
	  val _ = debugMsg "currentState"
          val error = ASM_Global.error "ASM_Run.currentState"
      in fn Id fName => ( fn x => 
	     case (findInHistory (hist, fName)) of
	       SOME (StaticFunction {interpretation,cache}) =>       (*!!!!! cache to be implemented !!! !*)
                 interpretation x
	     | SOME (DynamicFunction {currentInterpretation,defaultValue,...}) =>
               ( ( case ValueMap.find (currentInterpretation run, x) of
		     SOME y => y
                   | NONE   => defaultValue ) handle _ => defaultValue )
	     | SOME (DerivedFunction {currentInterpretation,...}) =>
	         currentInterpretation run x
	     | SOME (ExternalFunction (extFctHist as {currentInterpretation,finitenessConstraint,...})) =>
	       ( case ValueMap.find (!currentInterpretation, x) of
		   SOME y => y
		 | NONE   =>
		     let val y = getNewExternalFunctionValue (fName, dict) options extFctHist x
			 val _ = currentInterpretation := ValueMap.insert (!currentInterpretation, x, y)
		     in y end )
             | NONE =>
                 ASM_Global.error "ASM_Run.currentState" ("function not found:"^fName) NONE )
       | FunToMap f => special_operator "ASM_Run.currentState" ("FUN_TO_MAP", f) (#currentInterpretation, ML_FiniteMap_to_ASM_Value_Map) run
       | RelToSet f => special_operator "ASM_Run.currentState" ("REL_TO_SET", f) (#currentInterpretation, ML_FiniteMap_to_ASM_Value_Set) run
       | constant as IntConst _ => staticState (dict, false) constant
       | constant as FloatConst _ => staticState (dict, false) constant
       | constant as StringConst _ => staticState (dict, false) constant
      end

    and ASM_SL_staticFunction (dict, memoFlag) (fExpr as FunctionExprPos (_, fExpr')) =
      let val cache' =
            case fExpr' of
              LambdaTerm _ => conditionalCache memoFlag
            | _ => NONE     (* caching does not make sense for extensional defs.: they are already tables! *)
      in ( StaticFunction { interpretation = (!evalFExpr) fExpr (staticState (dict, memoFlag)),
                            cache = cache' } )
      end

    and ASM_SL_dynamicFunction fName (constraint :CONSTRAINT, fExpr as FunctionExprPos (_, fExpr')) =
      let fun M0 run = 
            case fExpr' of
	      MapToFun t => ValueMap.fromList (Map.listItemsi (!evalTerm t (initialState run, ASM_Env.empty)))
	    | SetToRel t => ValueMap.fromList ( map (fn x => (x, BOOL true))
                              (Set.listItems (!evalTerm t (initialState run, ASM_Env.empty))) )
            | _ => ASM_Global.error "ASM_Run.ASM_SL_dynamicFunction" "MAP_TO_FUN or SET_TO_REL expected" NONE
      in ( DynamicFunction { initialInterpretation = M0, pastUpdates   = [],
		  	     currentInterpretation = M0, futureUpdates = [],
			     defaultValue = determineDefaultValueForDynamicFunction fName constraint } )
      end

    and ASM_SL_externalFunction dict fName (constraint :CONSTRAINT) =
    ( let val T  = valOf (#TypeConstraint constraint)
          val fc = case (#RangeConstraint constraint) of
	             SOME fExpr => SOME (fn x => (!evalFExpr) fExpr (staticState (dict, true)) x)
                   | NONE       => NONE
      in ExternalFunction { type_ = T,
			    finitenessConstraint  = fc,
			    pastInterpretations   = [],
			    currentInterpretation = ref ValueMap.empty,
			    futureInterpretations = [] }
      end handle Option => ASM_Global.error "ASM_Run.ASM_SL_externalFunction"
			     ("external function '"^fName^"' without type constraint") NONE )

    and ASM_SL_derivedFunction fName (fExpr as FunctionExprPos (_, fExpr')) =
      let fun F0 run = (!evalFExpr) fExpr (initialState run)
          fun F run  = (!evalFExpr) fExpr (currentState run)
      in ( DerivedFunction { initialInterpretation = F0, currentInterpretation = F } )
      end

    fun newFunctionHistory (dict, options :OPTIONS ref) (fName : string, (loc, f :FUNCTION)) :FUNCTION_HISTORY =
      let open ASM_Value
	  val _ = !currLoc := ( (#1 (#1 (valOf (ASM_Dictionary.findFunction (!dict, fName)))))
	             handle _ => ASM_Global.error "ASM_Run.newFunctionHistory" ("not found") NONE )
      in case f of
	   ML_StaticFunction { interpretation, memoFlag } =>
	     StaticFunction { interpretation = interpretation, cache = conditionalCache memoFlag }
	 | ASM_SL_DataConstructor =>
	     StaticFunction { interpretation = constructorInterpretation fName, cache = NONE }
	 | ASM_SL_StaticFunction { definition } =>
	     ASM_SL_staticFunction (dict, #memoFlag (!options)) definition
	 | ASM_SL_DynamicFunction { constraint, initially } =>
             ASM_SL_dynamicFunction fName (constraint, initially)
	 | ASM_SL_DerivedFunction { definition } =>
	     ASM_SL_derivedFunction fName definition
	 | ASM_SL_ExternalFunction { constraint } =>
	     ASM_SL_externalFunction dict fName constraint
      end
  end (* local *)

  fun newHistory (dict, options :OPTIONS ref) :HISTORY =
    let open StringMap
        fun F (name, dictEntry, hist as {static,dynamic,external,derived}) =
	      case newFunctionHistory (dict, options) (name, dictEntry) of
		StaticFunction x =>
                  { static = StringMap.insert (static, name, x), dynamic=dynamic, external=external, derived=derived }
              | DynamicFunction x => 
                  { dynamic = StringMap.insert (dynamic,  name, x), static=static, external=external, derived=derived }
              | ExternalFunction x => 
                  { external = StringMap.insert (external, name, x), static=static, dynamic=dynamic, derived=derived }
              | DerivedFunction x =>
                  { derived = StringMap.insert (derived, name, x), static=static, dynamic=dynamic, external=external }
    in History ( StringMap.foldli F { static = empty, dynamic = empty, external = empty, derived = empty }
                                  (#funcDict (!dict)) )
    end

  fun newRun (dict :DICTIONARY) :RUN =
    let val (dictRef, optionsRef) = (ref dict, ref (!defaultOptions))
    in Run { dict      = dictRef,
	     hist      = newHistory (dictRef, optionsRef),
	     currStage = 0,
	     lastStage = 0,
	     options   = optionsRef }
    end


  (* -------------------------------------------------------------------------------------- *)
  (* --- NOTE: extendRun should only be called to extend a run in the initial state, ------ *)
  (* --------  otherwise it produces inconsistent runs !    ------------------------------- *)
  (* -------------------------------------------------------------------------------------- *)

  fun extendRun (Run (baseRun as {dict=baseDict,hist=baseHist,...}), deltaDict :DICTIONARY) :RUN =
    let val newDict = ref (ASM_Dictionary.override (!baseDict, deltaDict))
        val newHist = historyOverride (baseHist, newHistory (newDict, #options baseRun))
    in Run { dict      = newDict,
	     hist      = newHist,
             currStage = 0,
             lastStage = 0,
             options   = #options baseRun }
    end


  (* -------------------------------------------------------------------------------------- *)

  exception TransitionInterpretation of string
  fun transitionInterpretation (dict :DICTIONARY, S :STATE) (rName :string) :VALUE -> UPDATE_SET =
  ( case updateLoc (ASM_Dictionary.findNamedRule (dict, rName)) of
      SOME (ASM_Dictionary.ASM_SL_NamedRule {definition}) => (!evalTExpr) definition (dict, S)
    | NONE => raise TransitionInterpretation rName )

  (* -------------------------------------------------------------------------------------- *)

  fun update defaultValue (m, x, y) =
    case ASM_Value.compare (y, defaultValue) of
      EQUAL => (#1 (ValueMap.remove (m, x)) handle _ => m)
    | _     => ValueMap.insert (m, x, y)

  fun updateFunctionReverseUpdates defaultValue (fct_map, upds) =
    ValueMap.foldli
      ( fn (key, inf, ( fct_map', rev_upds )) =>
          ( update defaultValue (fct_map', key, inf),
            ValueMap.insert (rev_upds, key, valOf (ValueMap.find (fct_map, key)) handle _ => defaultValue ) ) )
      (fct_map, ValueMap.empty) upds


  fun step (run as Run {dict,hist,currStage,lastStage,options}, computeUpdSet :unit -> UPDATE_SET) =
    let fun updateDynamicFunctionComputing updSet (f :string, fHist :DYNAMIC_FUNCTION_HISTORY) =
          let val {initialInterpretation, pastUpdates, currentInterpretation, futureUpdates, defaultValue } = fHist
	      val (nextInterpretation, revUpds) =
		case StringMap.find (updSet, f) of
		  SOME fUpds => updateFunctionReverseUpdates defaultValue (currentInterpretation run, fUpds)
		| NONE       => (currentInterpretation run, ValueMap.empty)
	  in { initialInterpretation = initialInterpretation,
	       pastUpdates           = revUpds :: pastUpdates,
	       currentInterpretation = fn _ => nextInterpretation,
	       futureUpdates         = futureUpdates,
	       defaultValue          = defaultValue }
	  end
        fun storeExternalFunctionValues (f :string, fHist :EXTERNAL_FUNCTION_HISTORY) =
          let val { type_, finitenessConstraint,
                    pastInterpretations, currentInterpretation, futureInterpretations } = fHist
	   in { type_ = type_,
		finitenessConstraint  = finitenessConstraint,
		pastInterpretations   = (!currentInterpretation) :: pastInterpretations,
		currentInterpretation = ref ValueMap.empty,
		futureInterpretations = futureInterpretations }
           end

        fun collectUpdateSet (hist :HISTORY) :UPDATE_SET =
          let fun collectFunctionUpdates (f, {futureUpdates,...} :DYNAMIC_FUNCTION_HISTORY, updSet) =
	        StringMap.insert (updSet, f, hd futureUpdates)
          in StringMap.foldli collectFunctionUpdates UpdateSet.empty (dynamicHistory hist)
          end

        fun updateDynamicFunctionReusing (f :string, fHist :DYNAMIC_FUNCTION_HISTORY) =
          let val {initialInterpretation, pastUpdates, currentInterpretation, futureUpdates, defaultValue} = fHist
              val (nextInterpretation, revUpds) =
	      ( case futureUpdates of
		  fUpds :: futureUpdates' => updateFunctionReverseUpdates defaultValue (currentInterpretation run, fUpds)
		| [] => ASM_Global.error "ASM_Run.step" "update set should be already there [1]" NONE )
	  in { initialInterpretation = initialInterpretation,
	       pastUpdates           = revUpds :: pastUpdates,
	       currentInterpretation = fn _ => nextInterpretation,
	       futureUpdates         = tl futureUpdates,
	       defaultValue          = defaultValue }
	  end
        fun retrieveExternalFunctionValues (f :string, fHist :EXTERNAL_FUNCTION_HISTORY) =
          let val { type_, finitenessConstraint,
                    pastInterpretations, currentInterpretation, futureInterpretations} = fHist
	  in { type_ = type_,
	       finitenessConstraint  = finitenessConstraint,
	       pastInterpretations   = (!currentInterpretation) :: pastInterpretations,
	       currentInterpretation = ref (hd futureInterpretations),
	       futureInterpretations = tl futureInterpretations }
	  end

        fun collectExternalState (hist :HISTORY) :EXTERNAL_STATE =
          let fun collectExtFunction (f, {pastInterpretations,...} :EXTERNAL_FUNCTION_HISTORY, extState) =
	        StringMap.insert (extState, f, hd pastInterpretations)
          in StringMap.foldli collectExtFunction ExternalState.empty (externalHistory hist)
	  end

        val currStage' = currStage + 1
        val (hist', extState, updSet) =
          if currStage = lastStage
          then let val updSet = computeUpdSet ()
                   val History {static,dynamic,external,derived} = hist
                   val hist' = History { static=static, derived=derived,
					 external = StringMap.mapi storeExternalFunctionValues external,
					 dynamic  = StringMap.mapi (updateDynamicFunctionComputing updSet) dynamic }
               in ( hist', collectExternalState hist', updSet )
               end
	  else let val updSet = collectUpdateSet hist
                   val History {static,dynamic,external,derived} = hist
                   val hist' = History { static=static, derived=derived,
					 external = StringMap.mapi retrieveExternalFunctionValues external,
					 dynamic  = StringMap.mapi updateDynamicFunctionReusing dynamic }
               in ( hist', collectExternalState hist', updSet )
               end 
    in ( Run { dict      = dict,
	       hist      = hist',
	       currStage = currStage',
	       lastStage = Int.max (lastStage, currStage'),
	       options   = options },
         extState,
         updSet )
    end


  fun back (run as Run {dict,hist,currStage,lastStage,options} :RUN) :RUN =
    let fun updateDynamicFunction (f :string, fHist :DYNAMIC_FUNCTION_HISTORY) :DYNAMIC_FUNCTION_HISTORY =
          let val {initialInterpretation, pastUpdates, currentInterpretation, futureUpdates, defaultValue} = fHist
          in case pastUpdates of
	       revUpds :: pastUpdates' =>
		 let val (prevInterpretation, fUpds) =
		       if ValueMap.isEmpty revUpds
		       then (currentInterpretation run, ValueMap.empty)
		       else updateFunctionReverseUpdates defaultValue (currentInterpretation run, revUpds)
		 in { initialInterpretation = initialInterpretation,
		      pastUpdates           = pastUpdates',
		      currentInterpretation = fn _ => prevInterpretation,
		      futureUpdates         = fUpds :: futureUpdates,
		      defaultValue          = defaultValue }
		 end
	     | [] => fHist
          end
        fun updateExternalFunction (f :string, fHist :EXTERNAL_FUNCTION_HISTORY) :EXTERNAL_FUNCTION_HISTORY =
          let val { type_, finitenessConstraint,
                    pastInterpretations, currentInterpretation, futureInterpretations } = fHist
	  in case pastInterpretations of
               intp :: pastInterpretations' =>
		 { type_ = type_,
		   finitenessConstraint  = finitenessConstraint,
		   pastInterpretations   = pastInterpretations',
		   currentInterpretation = ref intp,
		   futureInterpretations = (!currentInterpretation) :: futureInterpretations }
             | [] => fHist
	  end

        val History {static,dynamic,external,derived} = hist
        fun hist' () = History { static=static, derived=derived,
				 external = StringMap.mapi updateExternalFunction external,
		                 dynamic  = StringMap.mapi updateDynamicFunction dynamic }
    in if currStage > 0
       then Run { dict      = dict,
		  hist      = hist' (),
		  currStage = currStage - 1,
		  lastStage = lastStage,
		  options   = options }
       else run
    end


  fun reinit (run as Run {currStage,...}) =
    if currStage > 0 then reinit (back run) else run

  fun gotoStage n (run as Run {currStage,...}, computeUpdateSet) =
    let fun goto run = gotoStage n (run, computeUpdateSet)
    in if n = 0 then reinit run
       else if currStage > 0 andalso n < currStage
       then goto (back run)
       else if n > currStage
       then goto (#1 (step (run, computeUpdateSet)))
       else run
    end


  fun discardFuture (run as Run {dict,hist,currStage,lastStage,options}) :RUN =
    let fun updateDynamicFunction (f :string, fHist :DYNAMIC_FUNCTION_HISTORY) :DYNAMIC_FUNCTION_HISTORY =
          let val {initialInterpretation, pastUpdates, currentInterpretation, futureUpdates, defaultValue} = fHist
          in { initialInterpretation = initialInterpretation,
	       pastUpdates           = pastUpdates,
	       currentInterpretation = currentInterpretation,
	       futureUpdates         = [],
	       defaultValue          = defaultValue }
          end
        fun updateExternalFunction (f :string, fHist :EXTERNAL_FUNCTION_HISTORY) :EXTERNAL_FUNCTION_HISTORY =
          let val { type_, finitenessConstraint,
                    pastInterpretations, currentInterpretation, futureInterpretations } = fHist
	  in { type_ = type_,
	       finitenessConstraint  = finitenessConstraint,
	       pastInterpretations   = pastInterpretations,
	       currentInterpretation = ref ValueMap.empty,
	       futureInterpretations = [] }
          end
        val History {static,dynamic,external,derived} = hist
	val hist' = History { static=static, derived=derived,
			      external = StringMap.mapi updateExternalFunction external,
		              dynamic  = StringMap.mapi updateDynamicFunction dynamic }
    in Run { dict = dict,
	     hist = hist',
             currStage = currStage,
             lastStage = currStage,
             options   = options }
    end


  fun lastExternalStateAndUpdateSet (run as Run {dict,hist,currStage,lastStage,options} :RUN) =
    if currStage > 0
    then let val (_, extState, updSet) =
               step (back run, fn _ => ASM_Global.error "ASM_Run.lastExternalStateAndUpdateSet"
			                                "update set should be already there [2]" NONE)
         in (extState, updSet)
	 end
    else (ExternalState.empty, UpdateSet.empty)


  fun currentExternalState (Run {hist,...}) :EXTERNAL_STATE =
    let fun collectExtFunction (f, {currentInterpretation,...} :EXTERNAL_FUNCTION_HISTORY, extState) =
	  StringMap.insert (extState, f, !currentInterpretation)
    in StringMap.foldli collectExtFunction ExternalState.empty (externalHistory hist)
    end


  fun currStage (Run {currStage,...}) = currStage
  fun lastStage (Run {lastStage,...}) = lastStage
  fun dictionary (Run {dict,...})     = !dict


  fun dynamicFunctionToMap (run as Run {hist=History {dynamic,...},...}, name) =
    case (StringMap.find (dynamic, name)) of
      SOME {currentInterpretation,...} => (currentInterpretation run)
    | NONE => ASM_Global.error "ASM_Run.lastUpdateSet" ("dynamic function "^name^" not defined") NONE

  fun dynamicRelationToSet (run as Run {hist=History {dynamic,...},...}, name) =
    let (* !!! remove ??? !!! open ASM_Value
        fun mapToSet map =
          ValueMap.foldli
          (fn (x, y, set) => if compare (y, Prim.True) = EQUAL then ValueSet.add (set, x) else set) ValueSet.empty
          map  *)
    in case (StringMap.find (dynamic, name)) of
	 SOME {currentInterpretation,...} => mapToSet (currentInterpretation run)
       | NONE => ASM_Global.error "ASM_Run.lastUpdateSet" ("dynamic function "^name^" not defined") NONE
    end
end
