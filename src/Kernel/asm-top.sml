structure ASM_Top =
struct
  datatype PROBLEM =
    Impossible of string
  | NoProgramDefined
  | CTL_Operator of string

  exception Error of PROBLEM Error.ERROR

  fun message (problem) =
    let val R = String_.replace
	val what_  = case problem of
	  Impossible s     =>
            R "Impossible! This cannot happen! (Probably a bug) [$1]" [s]
        | NoProgramDefined =>
	    R "No program specified (please identify a rule as the program)" []
        | CTL_Operator s =>
	    R "Can not evaluate CTL operator '$1'" [ s ]
    in "Top-level error -- " ^ what_ ^ "\n"
  end

  fun error (fct :string) (what :PROBLEM) =
    raise Error { module = "ASM_Top", function = fct, problem = what, message = message, cause = NONE }



  local
    fun ctl_operator which =
      ( (SOME ASM_AST.Primitive, NONE),
	ASM_Dictionary.ML_StaticFunction
	  { interpretation = fn _ => error "" (CTL_Operator which),
	    memoFlag = false } )

    open ASM_AST ASM_Signature ASM_Dictionary
    structure Prim = ASM_Value.Prim

    infix -->
    fun [T1_dom] --> T_ran = FuncType (T1_dom, T_ran)
      | Ts_dom   --> T_ran = FuncType (Tuple Ts_dom, T_ran)
    val (a', b') = (TypeParam "a", TypeParam "b")
    fun infix_op isCons ops T = Func { functionKind = Static, constructor = isCons, opStatus = ops, type_ = T }
    val infix_function    = infix_op false
    val infix_constructor = infix_op true
    fun function T     = Func { functionKind = Static, constructor = false, opStatus = NonInfix, type_ = T }
    fun constructor T  = Func { functionKind = Static, constructor = true,  opStatus = NonInfix, type_ = T }
    fun function0 T    = function (FuncType (Unit, T))
    fun constructor0 T = constructor (FuncType (Unit, T))
    fun ctl_op T       = Func { functionKind = CTL, constructor = false, opStatus = NonInfix, type_ = T }

    fun insertTypes (baseSign, baseDict) L =
    ( List_.foldll (fn (sign,(key,signInfo,_)) => ASM_Signature.insert   (sign, key, signInfo)) baseSign L,
      List_.foldll (fn (dict,(key,_,dictInfo)) => ASM_Dictionary.addType (dict, key, dictInfo)) baseDict L )

    fun insertFunctions (baseSign, baseDict) L =
    ( List_.foldll (fn (sign,(key,signInfo,_)) => ASM_Signature.insert       (sign, key, signInfo)) baseSign L,
      List_.foldll (fn (dict,(key,_,dictInfo)) => ASM_Dictionary.addFunction (dict, key, dictInfo)) baseDict L )


    fun arityError f = ASM_Global.error "function evaluation" ("arity error: " ^ f) NONE 

    fun ml_carrier P =
      ((SOME Primitive, NONE), ML_Carrier P)

    fun prim0 fName f =
    ( (SOME Primitive, NONE),
      ML_StaticFunction { interpretation = fn (ASM_Value.TUPLE []) => f
                                            | _ => arityError fName,
			  memoFlag = true } )

    fun prim1_ memoFlag fName f =
    ( (SOME Primitive, NONE),
      ML_StaticFunction { interpretation = fn x => f x,
			  memoFlag = memoFlag } )

    fun prim2_ memoFlag fName f =
    ( (SOME Primitive, NONE),
      ML_StaticFunction { interpretation = fn (ASM_Value.TUPLE [x,y]) => f (x,y)
                                            | _ => arityError fName,
                          memoFlag = memoFlag } )

    fun prim3 fName f =
    ( (SOME Primitive, NONE),
      ML_StaticFunction { interpretation = fn (ASM_Value.TUPLE [x,y,z]) => f (x,y,z)
                                            | _ => arityError fName,
                          memoFlag = false } )

    fun prim1 x  = prim1_ false x
    fun prim1m x = prim1_ true x
    fun prim2 x  = prim2_ false x
    fun prim2m x = prim2_ true x

    exception X
    val prim_temp = ML_StaticFunction { interpretation = fn _ => raise X, memoFlag = false }     (* !!!!! *)

  in
    (* initial context *)
    val (initialSign, initialDict) =
      let open ASM_Value in
      insertTypes (ASM_Signature.empty, ASM_Dictionary.empty)
      [ ("BOOL",    Type { arity = 0 },  ml_carrier (fn BOOL _ => true | _ => false)),
	("INT",     Type { arity = 0 },  ml_carrier (fn INT _ => true | UNDEF => true | _ => false)),
	("FLOAT",   Type { arity = 0 },  ml_carrier (fn FLOAT _ => true | UNDEF => true | _ => false)),
	("STRING",  Type { arity = 0 },  ml_carrier (fn STRING _ => true | UNDEF => true | _ => false)),
	("LIST",    Type { arity = 1 },  ml_carrier (fn UNDEF => true | x => isList x)),
	("SET",     Type { arity = 1 },  ml_carrier (fn UNDEF => true | x => isFSet x)),
	("MAP",     Type { arity = 2 },  ml_carrier (fn UNDEF => true | x => isFMap x)) ]
      end

    val (initialSign, initialDict) =
      insertFunctions (initialSign, initialDict)
      [ ("undef",   constructor0 a',      prim0 "undef" Prim.Undef),

	("true",    constructor0 Bool,    prim0 "true"  Prim.True),
	("false",   constructor0 Bool,    prim0 "false" Prim.False),

	("and",     infix_function (OpL 1) ([Bool, Bool] --> Bool),  prim2 "and" Prim.And),
	("or",      infix_function (OpL 0) ([Bool, Bool] --> Bool),  prim2 "or"  Prim.Or),
	("=>",      infix_function (OpR 0) ([Bool, Bool] --> Bool),  prim2 "or"  Prim.Implies),
	("not",     function ([Bool] --> Bool),                      prim1 "not" Prim.Not),

	("=",	    infix_function (OpL 4) ([a', a'] --> Bool),      prim2 "="  Prim.==),
	("!=",	    infix_function (OpL 4) ([a', a'] --> Bool),	     prim2 "!=" Prim.!=),
	("<",	    infix_function (OpL 4) ([a', a'] --> Bool),	     prim2 "<"  Prim.<),
	("<=",	    infix_function (OpL 4) ([a', a'] --> Bool),	     prim2 "<=" Prim.<=),
	(">",	    infix_function (OpL 4) ([a', a'] --> Bool),	     prim2 ">"  Prim.>),
	(">=",	    infix_function (OpL 4) ([a', a'] --> Bool),	     prim2 ">=" Prim.>=),

	("andb",    function ([Int, Int] --> Int),		     prim2 "andb" Prim.Andb),
	("orb",     function ([Int, Int] --> Int),		     prim2 "orb"  Prim.Orb),
	("xorb",    function ([Int, Int] --> Int),		     prim2 "xorb" Prim.Xorb),
	("notb",    function ([Int] --> Int),			     prim1 "notb" Prim.Notb),
	("lsh",     function ([Int, Int] --> Int),		     prim2 "lsh"  Prim.Lsh),
	("rsh",     function ([Int, Int] --> Int),		     prim2 "rhs"  Prim.Rsh),
        ("hex_to_int", function ([String] --> Int),		     prim1 "hex_to_int" Prim.HexToInt),
        ("bin_to_int", function ([String] --> Int),		     prim1 "bin_to_int" Prim.BinToInt),
        ("int_to_hex", function ([Int] --> String),		     prim1 "int_to_hex" Prim.IntToHex),
        ("int_to_bin", function ([Int] --> String),		     prim1 "int_to_bin" Prim.IntToBin),

	("ord",     function ([String] --> Int),		     prim1 "ord" Prim.Ord),
	("chr",     function ([Int] --> String),		     prim1 "chr" Prim.Chr),

	("+",	    infix_function (OpL 6) ([Int, Int] --> Int),     prim2 "+" Prim.+),
	("-",	    infix_function (OpL 6) ([Int, Int] --> Int),     prim2 "-" Prim.-),
	("*",	    infix_function (OpL 7) ([Int, Int] --> Int),     prim2 "*" Prim.Mul),
	("div",     infix_function (OpL 7) ([Int, Int] --> Int),     prim2 "div" Prim.div),
	("mod",     infix_function (OpL 7) ([Int, Int] --> Int),     prim2 "mod" Prim.mod),
	("~",	    function ([Int] --> Int),			     prim1 "~" Prim.~),

	("fadd",    function ([Float, Float] --> Float),	     prim2 "fadd" Prim.Fadd),
	("fsub",    function ([Float, Float] --> Float),	     prim2 "fsub" Prim.Fsub),
	("fmul",    function ([Float, Float] --> Float),	     prim2 "fmul" Prim.Fmul),
	("fdiv",    function ([Float, Float] --> Float),	     prim2 "fdiv" Prim.Fdiv),
	("fneg",    function ([Float] --> Float),		     prim1 "fneg" Prim.Fneg),

	("##",	    infix_function (OpL 1) ([String, String] --> String),  prim2 "##" Prim.##),

	("abs",	    function ([Int] --> Int),			     prim1 "abs" Prim.Abs),
	("int_to_string", function ([Int] --> String),		     prim1 "int_to_string" Prim.IntToString),

	("floor",         function ([Float] --> Int),		     prim1 "floor" Prim.Floor),
	("round",         function ([Float] --> Int),		     prim1 "round" Prim.Round),
	("int_to_float",  function ([Int] --> Float),		     prim1 "int_to_float" Prim.IntToFloat),

	("sqrt",    function ([Float] --> Float),		     prim1 "sqrt" Prim.Sqrt),
	("exp",     function ([Float] --> Float),		     prim1 "exp" Prim.Exp),
	("ln",	    function ([Float] --> Float),		     prim1 "ln" Prim.Ln),
	("sin",     function ([Float] --> Float),		     prim1 "sin" Prim.Sin),
	("cos",     function ([Float] --> Float),		     prim1 "cos" Prim.Cos),
	("arctan",  function ([Float] --> Float),		     prim1 "arctan" Prim.Arctan),

	("nil",     constructor0 (List a'),			             prim0 "nil" Prim.Nil),
	("::",	    infix_constructor (OpR 1) ([a', List a'] --> List a'),   prim2 "::"  Prim.::),
	("hd",	    function ([List a'] --> a'),			     prim1 "hd"  Prim.Hd),
	("tl",	    function ([List a'] --> List a'),			     prim1 "tl"  Prim.Tl),
	("length",  function ([List a'] --> Int),			     prim1 "length" Prim.Length),
	("append",  function ([List a', List a'] --> List a'),		     prim2 "append" Prim.@),
	("@",	    infix_function (OpR 5) ([List a', List a'] --> List a'), prim2 "@"      Prim.@),
	("concat",  function ([List (List a')] --> List a'),		     prim1 "concat" Prim.Concat),
	("list_interval", function ([Int, Int, Int] --> List Int),	     prim3 "list_interval"
										     Prim.ListInterval),


	("emptyset",	 constructor0 (Set a'),			               prim0 "emptyset" Prim.Emptyset),
        ("list_to_set",	 function ([ List a'] --> Set a'),		       prim1 "list_to_set" Prim.ListToSet),
	("set_interval", function ([Int, Int, Int] --> Set Int),	       prim3 "set_interval" Prim.SetInterval),
	("member",	 function ([a', Set a'] --> Bool),		       prim2 "member" Prim.Member),
	("prod",	 function ([Set a', Set b'] --> Set (Tuple [a',b'])),  prim2 "prod" Prim.Prod),
	("intersect",	 infix_function (OpL 6) ([Set a', Set a'] --> Set a'), prim2 "intersect" Prim.Intersect),
	("\\",		 infix_function (OpL 5) ([Set a', Set a'] --> Set a'), prim2 "\\" Prim.\),
	("union",	 infix_function (OpL 4) ([Set a', Set a'] --> Set a'), prim2 "union" Prim.Union),
	("card",	 function ([Set a'] --> Int),			       prim1 "card" Prim.Card),
	("set_to_list",  function ([Set a'] --> List a'),		       prim1 "set_to_list" Prim.SetToList),
	("element_of",	 function ([Set a'] --> a'),			       prim1 "element_of" Prim.ElementOf),
	("Union",	 function ([Set (Set a')] --> Set a'),		       prim1 "Union" Prim.UNION),
	("Intersect",	 function ([Set (Set a')] --> Set a'),		       prim1 "Intersect" Prim.INTERSECT),

	("emptymap",	 constructor0 (Map (a',b')),  prim0 "emptymap" Prim.Emptymap),
	("list_to_map",	 function ([List (Tuple [a',b'])] --> Map (a',b')),	prim1 "list_to_map" Prim.ListToMap),
	("apply",	 function ([Map (a',b'), a'] --> b'),			prim2 "apply"       Prim.Apply),
	("map_union",	 function ([Map (a',b'), Map (a',b')] --> Map (a',b')), prim2 "map_union" Prim.MapUnion),
	("override",	 function ([Map (a',b'), Map (a',b')] --> Map (a',b')), prim2 "override"  Prim.Override),
	("map_card",	 function ([Map (a',b')] --> Int),			prim1 "map_card"  Prim.MapCard),
	("domain",	 function ([Map (a',b')] --> Set a'),			prim1 "domain"    Prim.Domain),
	("range",	 function ([Map (a',b')] --> Set b'),			prim1 "range"     Prim.Range),

        ("map_to_set",	 function ([Map (a',b')] --> Set (Tuple [a',b'])),	prim1 "map_to_set" Prim.MapToSet),
	("set_to_map",	 function ([Set (Tuple [a',b'])] --> Map (a',b')),	prim1 "set_to_map" Prim.SetToMap),
	("map_to_list",  function ([Map (a',b')] --> List (Tuple [a',b'])),	prim1 "map_to_list" Prim.MapToList),
	("AG",           ctl_op ([Bool] --> Bool), ctl_operator "AG"),
	("AF",           ctl_op ([Bool] --> Bool), ctl_operator "AF"),
	("EG",           ctl_op ([Bool] --> Bool), ctl_operator "EG"),
	("EF",           ctl_op ([Bool] --> Bool), ctl_operator "EF"),
	("AX",           ctl_op ([Bool] --> Bool), ctl_operator "AX"),
	("EX",           ctl_op ([Bool] --> Bool), ctl_operator "EX")   ]
  end

  (* current context *)
  val sign   = ref initialSign
  val dict   = ref initialDict
  val run    = ref (ASM_Run.newRun (!dict))
  val extState = ref (SOME (ASM_Run.ExternalState.empty :ASM_Run.EXTERNAL_STATE))   (* last external state *)
  val updSet   = ref (SOME (ASM_Run.UpdateSet.empty :ASM_Run.UPDATE_SET))           (* last update set *)

  val program = ref (NONE :ASM_AST.RULE option)


  fun initState () = ASM_Run.initialState (!run)
  fun currState () = ASM_Run.currentState (!run)

  fun reset () =
  ( sign    := initialSign;
    dict    := initialDict;
    run     := ASM_Run.newRun (!dict);
    program := NONE;
    extState := SOME (ASM_Run.ExternalState.empty);
    updSet   := SOME (ASM_Run.UpdateSet.empty) )

  fun add (deltaSign, deltaDict) =
  ( sign := ASM_Signature.override (!sign, deltaSign);
    dict := ASM_Dictionary.override (!dict, deltaDict);
    run  := ASM_Run.newRun (!dict);
    extState := SOME (ASM_Run.ExternalState.empty);
    updSet   := SOME (ASM_Run.UpdateSet.empty) )

  fun setProgram R =
  ( ASM_Check.rule R (!sign) (ASM_TypeEnv.empty);
    program := SOME R )

  fun evalTerm t = ASM_Eval.term t (currState (), ASM_Env.empty)
  fun evalRule R = ASM_Eval.rule R (!dict, currState (), ASM_Env.empty)
  fun evalDef sign D       = ASM_Eval.def sign D (!dict)
  fun evalDefBlock sign Db = ASM_Eval.defBlock sign Db (!dict)

  fun step () =
  ( ( let fun computeUpdSet () = evalRule (valOf (!program))
	  val (newRun, newExtState, newUpdSet) = ASM_Run.step (!run, computeUpdSet)
      in  run    := newRun;
	  extState := SOME newExtState;
          updSet   := SOME newUpdSet
      end )
    handle Option => error "step" NoProgramDefined )

  fun updateRun newRun =
  ( run      := newRun;
    extState := NONE;
    updSet   := NONE )

  fun back ()   = updateRun (ASM_Run.back (!run))
  fun reinit () = updateRun (ASM_Run.reinit (!run))
  fun gotoStage stg = updateRun (ASM_Run.gotoStage stg (!run, Misc.K (evalRule (valOf (!program)))))

  fun discardFuture () =
    let val newRun = ASM_Run.discardFuture (!run)
    in run := newRun
    end


  fun computeExternalStateAndUpdateSet run =
    let val (extState', updSet') = ASM_Run.lastExternalStateAndUpdateSet (!run)
    in extState := SOME extState';
       updSet   := SOME updSet';
       (extState', updSet')
    end

  fun lastExternalState () :ASM_Run.EXTERNAL_STATE =
  ( case (!extState) of
      NONE   => #1 (computeExternalStateAndUpdateSet run)
    | SOME x => x )

  fun lastUpdateSet () :ASM_Run.UPDATE_SET =
  ( case (!updSet) of
      NONE   => #2 (computeExternalStateAndUpdateSet run)
    | SOME x => x )

  fun currentExternalState () =
    ASM_Run.currentExternalState (!run)

  fun currStage () =
    ASM_Run.currStage (!run)

  fun lastStage () =
    ASM_Run.lastStage (!run)
end
