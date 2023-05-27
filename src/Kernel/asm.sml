(* ************************************************************************** *\

     The ASM Workbench for SML/NJ 1.10 - G. Del Castillo, Nov 2000

     Main structure: ASM

\* ************************************************************************** *)

structure ASM =
struct
  structure Misc = Misc
  structure AST = ASM_AST
  structure Parser = ASM_Parser
  structure Pretty = ASM_Pretty
  val a = 1

  fun emptyContext () :ASM_Global.CONTEXT =
    ASM_Global.ASM_SL_Context (ASM_Signature.empty)

  fun topContext () :ASM_Global.CONTEXT =
    ASM_Global.ASM_SL_Context (!ASM_Top.sign)

  fun sgnContext sgn :ASM_Global.CONTEXT =
    ASM_Global.ASM_SL_Context sgn

  fun typealias () =
    ASM_Signature.typealias (!ASM_Top.sign)

  fun detailed_IO_errMsg (cause :exn) =
    case cause of
      OS.SysErr (s, _) => s
    | Subscript => "subscript"
    | IO.BlockingNotSupported  => "blocking not supported"
    | IO.NonblockingNotSupported => "non blocking not supported"
    | IO.TerminatedStream => "terminated stream"
    | IO.ClosedStream => "closed stream"
    | _ => "???"

  fun errorMessage ex =
  ( case ex of
      ASM_Global.Error cont => Error.message cont
    | ASM_Signature.Error cont => Error.message cont
    | ASM_Lexer.Error cont  => Error.message cont
    | ASM_Parser.Error cont => Error.message cont
    | ASM_Check.Error cont  => Error.message cont
    | ASM_Run.Error cont    => Error.message cont
    | ASM_Eval.Error cont   => Error.message cont
    | ASM_Top.Error cont    => Error.message cont
    | ASM_Oracle.Error cont => Error.message cont
    | ASM_Value.Prim.Prim (f, xs) =>
        String_.replace
	  "exception ASM_Value.Prim.Prim raised, function '$1', arguments: $2"
          ( [ f, ListFormat.fmt {init="", final="", fmt=ASM_Value.toString, sep=", "} xs ] )
    | IO.Io {name, function, cause} =>
	  ( String_.replace "\nI/O Error: $1 / $2 / $3\n"
		 	    [ name, function, detailed_IO_errMsg cause ] )
    | ex => "Unrecognized exception! Please report bug to 'giusp@uni-paderborn.de'" )

  fun catch f x =
  ( (f x)
    handle ex =>
    ( case ex of
        ASM_Global.Error cont => (Error.report cont; raise ex)
      | ASM_Signature.Error cont => (Error.report cont; raise ex)
      | ASM_Lexer.Error cont  => (Error.report cont; raise ex)
      | ASM_Parser.Error cont => (Error.report cont; raise ex)
      | ASM_Check.Error cont  => (Error.report cont; raise ex)
      | ASM_Run.Error cont    => (Error.message cont; raise ex)
      | ASM_Eval.Error cont   => (Error.report cont; raise ex)
      | ASM_Top.Error cont    => (Error.report cont; raise ex)
      | ASM_Oracle.Error cont => (Error.report cont; raise ex)
      | ASM_Value.Prim.Prim (f, xs) => (print (errorMessage ex); raise ex)
      | ex => raise ex ) )

  fun scanString' s = ASM_Lexer.tabulate (ASM_Lexer.testString (topContext ()) s)
  fun scanFile'   s = ASM_Lexer.tabulate (ASM_Lexer.testFile (topContext ()) s)

  fun parse' parseFct s       = ASM_Lexer.scanString (topContext ()) parseFct s
  fun ctxDepParse' parseFct s = ASM_Lexer.ctxDepScanString (topContext ()) parseFct s

  fun parseFile' parseFct s = ASM_Lexer.ctxDepScanFile (topContext ()) parseFct s
  fun parseFileWithSign' sgn parseFct s = ASM_Lexer.ctxDepScanFile (sgnContext sgn) parseFct s

  val pp = Pretty.pr TextIO.stdOut 1000

  fun testParser (parseFct, prettyFct) s =
    let val result as ((pos,p),_) = catch (fn _ => parse' parseFct s) ()
    in pp (prettyFct p)
    end

  fun type_ s = testParser (ASM_Parser.type_, ASM_Pretty.type_) s
  fun patt s = testParser (ASM_Parser.patt, ASM_Pretty.patt (topContext ())) s
  fun term s = testParser (ASM_Parser.term, ASM_Pretty.term (topContext ())) s
  fun rule s = testParser (ASM_Parser.rule, ASM_Pretty.rule (topContext ())) s

  local
    fun doParse F s            = let val ((_,e),_) = parse' F s in e end
    fun doCtxDepParse F s      = let val ((_,e),_) = ctxDepParse' F s in e end
    fun doCtxDepParseFile F s  = let val ((_,e),_) = parseFile' F s in e end
    fun doParseFileWithSign sgn F s = let val ((_,e),_) = parseFileWithSign' sgn F s in e end
    fun doPrint F e            = pp (F (topContext ()) e)
    fun doStringCvt F width    = (Pretty.toString width) o (F (topContext ()))
  in
    val printFunctionKind  = pp o ASM_Pretty.functionKind
    fun functionKindToString width = (Pretty.toString width) o ASM_Pretty.functionKind

    val parseName' = doParse ASM_Parser.name
    val parseName  = catch parseName'
    val printName  = pp o ASM_Pretty.name
    fun nameToString width = (Pretty.toString width) o ASM_Pretty.name

    val printInfixedName  = pp o ASM_Pretty.infixedName
    fun infixedNameToString width = (Pretty.toString width) o ASM_Pretty.infixedName

    val parseType' = doParse ASM_Parser.type_
    val parseType  = catch parseType'
    val printType  = pp o ASM_Pretty.type_ 
    fun typeToString width = (Pretty.toString width) o ASM_Pretty.type_

    val parsePatt' = doParse ASM_Parser.patt
    val parsePatt  = catch parsePatt'
    val printPatt  = doPrint ASM_Pretty.patt
    val pattToString = doStringCvt ASM_Pretty.patt

    val parseTerm' = doParse ASM_Parser.term
    val parseTerm  = catch parseTerm'
    val printTerm  = doPrint ASM_Pretty.term
    val termToString = doStringCvt ASM_Pretty.term

    val parseRule' = doParse ASM_Parser.rule
    val parseRule  = catch parseRule'
    val printRule  = doPrint ASM_Pretty.rule
    val ruleToString = doStringCvt ASM_Pretty.rule

    val parseFExpr' = doParse ASM_Parser.functionExpr
    val parseFExpr  = catch parseFExpr'
    val printFExpr  = doPrint ASM_Pretty.functionExpr
    val fexprToString = doStringCvt ASM_Pretty.functionExpr

    val parseTExpr' = doParse ASM_Parser.transitionExpr
    val parseTExpr  = catch parseTExpr'
    val printTExpr  = doPrint ASM_Pretty.transitionExpr
    val texprToString = doStringCvt ASM_Pretty.transitionExpr

    val parseDef'   = doCtxDepParse ASM_Parser.def
    val parseDef    = catch parseDef'
    val printDef    = doPrint ASM_Pretty.def
    val defToString = doStringCvt ASM_Pretty.def

    val parseDefFile' = fn s => AST.DefBlock (AST.File s, doCtxDepParseFile ASM_Parser.def s)
    val parseDefFileWithSign' = fn sgn => fn s => AST.DefBlock (AST.File s, doParseFileWithSign sgn ASM_Parser.def s)
    fun parseDefFileWithSign sgn s = catch (fn _ => parseDefFileWithSign' sgn s) ()

    val scanDefFile'  = ASM_Lexer.testFile (topContext ())
  end

  local open ASM_Check
  in
    fun checkType' T   = type_ T (!ASM_Top.sign)
    fun checkType  T   = catch checkType' T

    fun checkPatt' p   = patt p (!ASM_Top.sign) (Env.empty)
    fun checkPatt  p   = catch checkPatt' p

    fun checkTerm' t   = term t (!ASM_Top.sign) (Env.empty)
    fun checkTerm  t   = catch checkTerm' t

    fun checkRule' R   = rule R (!ASM_Top.sign) (Env.empty)
    fun checkRule  R   = catch checkRule' R

    fun checkFExpr' FE = functionExpr FE (!ASM_Top.sign)
    fun checkFExpr  FE = catch checkFExpr' FE

    fun checkTExpr' TE = transitionExpr TE (!ASM_Top.sign)
    fun checkTExpr  TE = catch checkTExpr' TE

    fun checkDef' D    = def D (!ASM_Top.sign)
    fun checkDef  D    = catch checkDef' D

    fun checkDefBlock' Db sgn = defBlock Db sgn
    fun checkDefBlock Db sgn = catch (fn _ => checkDefBlock' Db sgn) ()

    fun checkDefFileWithSign' sgn fileName = checkDefBlock' (parseDefFileWithSign' sgn fileName) sgn
  end

  local open ASM_Eval
  in
    val initState = ASM_Top.initState
    val currState = ASM_Top.currState

    val evalTerm' = ASM_Top.evalTerm
    val evalTerm  = catch evalTerm'

    val evalRule' = ASM_Top.evalRule
    val evalRule  = catch evalRule'

    val evalDef'  = ASM_Top.evalDef
    val evalDef   = catch evalDef'

    val evalDefBlock' = ASM_Top.evalDefBlock
    val evalDefBlock  = catch evalDefBlock'

    fun addDef' D =
      let val sign' = checkDef' D
      in ASM_Top.add (sign', evalDef' sign' D)
      end

    fun addDef  D = catch addDef' D

    fun addDefBlock' Db =
      let val sign' = checkDefBlock' Db (!ASM_Top.sign)
      in ASM_Top.add (sign', evalDefBlock' sign' Db)
      end

    fun addDefBlock  Db = catch addDefBlock' Db
  end


  structure Test =
  struct
    fun println s = print (s^"\n")
    structure S =
    struct
      val (checkType, checkPatt, checkTerm, checkRule) =
          (checkType, checkPatt, checkTerm, checkRule)
      val (checkFExpr, checkTExpr, checkDef, checkDefBlock) =
          (checkFExpr, checkTExpr, checkDef, checkDefBlock)
      val (evalTerm, evalRule, evalDef, addDef, addDefBlock) =
          (evalTerm, evalRule, evalDef, addDef, addDefBlock)
    end

    fun checkType  s = println (ASM_Type.toString (S.checkType (parseType s)))
    fun checkPatt  s = let val (env, T) = S.checkPatt (parsePatt s)
	               in println ( String_.replace "( $1, $2 )"
			 	    [ ASM_TypeEnv.toString env, ASM_Type.toString T ] )
		       end
    fun checkTerm  s = println (ASM_Type.toString (S.checkTerm (parseTerm' s)))
    fun checkRule  s = println (ASM_Type.toString (S.checkRule (parseRule' s)))
    fun checkFExpr s = println (ASM_Type.toString (S.checkFExpr (parseFExpr' s)))
    fun checkTExpr s = println (ASM_Type.toString (S.checkTExpr (parseTExpr' s)))
    fun checkDef   s = println (ASM_Signature.toString (S.checkDef (parseDef' s)))

    fun checkDefFileWithSign sgn = fn (s :string) =>
      println (ASM_Signature.toString (S.checkDefBlock (parseDefFileWithSign sgn s) sgn))
    val checkDefFile =
      checkDefFileWithSign (!ASM_Top.sign)

    fun evalTerm s =
      println (ASM_Value.toString ((fn t => #2 (S.checkTerm t, S.evalTerm t)) (parseTerm s)))

    fun evalRule s =
      println (ASM_Run.UpdateSet.toString ((fn R => #2 (S.checkRule R, S.evalRule R)) (parseRule s)))

    fun addDef s = S.addDef (parseDef s)
    fun addDefBlock s = S.addDefBlock (parseDefFileWithSign (!ASM_Top.sign) s)
  end

  fun reset' () = ASM_Top.reset ()
  fun reset ()  = catch reset' ()

  fun loadFile' fileName = 
    let val defBlock    = parseDefFileWithSign' (!ASM_Top.sign) fileName
        val loadedNames = ASM_AST.namesDefinedInDefBlock defBlock
    in addDefBlock' defBlock;
       loadedNames
    end
  fun loadFile fileName = catch loadFile' fileName

  fun step' ()  = ASM_Top.step ()
  fun step ()   = catch step' ()

  fun back' ()  = ASM_Top.back ()
  fun back ()   = catch back' ()

  fun reinit' () = ASM_Top.reinit ()
  fun reinit ()  = catch reinit' ()

  fun currStage ()  = ASM_Top.currStage ()
  fun lastStage ()  = ASM_Top.lastStage ()

  fun setProgram' s = ASM_Top.setProgram (parseRule' s)
  fun setProgram s  = catch setProgram' s

  fun discardFuture () = ASM_Top.discardFuture ()

  fun init () =
    let open TextIO
    in ( (*output (stdErr, "Welcome to the ASM Workbench... initialising...\n")*) )
    end

  fun shutdown () =
    let open TextIO
    in ( (*output (stdErr, "Terminating... good bye...\n");*)
         ASM_Oracle.quit_oracle () )
    end

  val _ = ASM_Global.debugFlag := false

  val _ =
  ( ASM_Oracle.parseTerm := parseTerm;
    ASM_Oracle.checkTerm := checkTerm;
    ASM_Oracle.typealias := typealias;
    ASM_Oracle.evalTerm  := evalTerm )
end
