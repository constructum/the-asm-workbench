(*
##
## "asm-parser.sml", G. Del Castillo, Feb 2000
##
##
##
*)


structure ASM_Parser =
struct
  open Misc ASM_ParserCombinators
  infix || until until'

  exception Error of (LOCATION option * POSITION option * string) Error.ERROR

  fun message (location, position, what_) =
    let val where_ = String_.replace "$1:$2:\n"
	               [ ASM_Location.toErrorString (valOf location) handle Option => "--",
			 ASM_Position.toErrorString (valOf position) handle Option => "--" ]
    in where_ ^ "  Parse error -- " ^ what_ ^ "\n"
    end

  fun error fct (pos, problem) =
    raise Error
    { module = "ASM_Parser",
      function = fct,
      problem = (!ASM_Lexer.sourceLoc, pos, problem),
      message = message,
      cause = NONE }


  (* ================================================================================ *
   *										      *
   *    THE ASM-SL PARSER							      *
   *										      *
   * ================================================================================ *)

  fun >>> f pos args = (pos, f args)


  (* --- names, identifiers and other small things --- *)

  fun name tokenizer =
    let fun ERROR pos msg args =
	  K (K (error "name" (SOME pos, String_.replace ("name expected here ("^msg^" found instead)") args)))
    in  Trace.out "ASM_Parser.name";
	selectNext (tokenizer :(POSITION * TOKEN, substring) tracking_reader) ( fn pos => fn
	 Int_ i     => (Trace.out "Int_"; R0 (pos, IntConst i))
       | Float_ r   => (Trace.out "Float_"; R0 (pos, FloatConst r))
       | Str_ s     => (Trace.out "Str_"; R0 (pos, StringConst s))
       | Keyw_ "FUN_TO_MAP" => (  Trace.out "Keyw_ FUN_TO_MAP";
				  R1 fctIdent (>>> ( fn (_, fct_name) => FunToMap fct_name )) )
       | Keyw_ "REL_TO_SET" => (  Trace.out "Keyw_ REL_TO_Set";
				  R1 fctIdent (>>> ( fn (_, fct_name) => RelToSet fct_name )) )
       | Keyw_ s    => ( Trace.out "Keyw_ s"; ERROR pos "keyword '$1'" [ s ] )
       | TypeVar_ s => ( Trace.out "Typevar_ s"; ERROR pos "type variable '$1'" [ s ] )
       | RuleId_ s  => ( Trace.out "RuleId_ s"; R0 (pos, Id s) )
       | FuncId_ s  => ( Trace.out "FuncId_ s"; R0 (pos, Id s) )
       | TypeId_ s  => ( Trace.out "TypeId_ s"; R0 (pos, Id s) )
       | Id_ s	    => ( Trace.out "Id_ s"; R0 (pos, Id s) )
       | Infix_ (_,_,s) => ( Trace.out "Infix_ s"; R0 (pos, Id s) ) )
    end

(*		 | Keyw_ "[" => (  R2   ($"[") ($"]")               (>> (fn (_,_) => %% nilPatt)) *)

  and ident tokenizer =
    let fun ERROR pos msg args = K (K (error "ident" (SOME pos,
          String_.replace ("identifier expected here ("^msg^" found instead)") args)))
    in Trace.out "ASM_Parser.ident";
       selectNext tokenizer ( fn pos => fn
	 Int_ i     => (Trace.out "Int_"; ERROR pos "integer constant" [] )
       | Float_ r   => (Trace.out "Float_"; ERROR pos "floating-point constant" [] )
       | Str_ s     => (Trace.out "Str_"; ERROR pos "string constant" [] )
       | Keyw_ s    => (Trace.out "Keyw_"; raise (FAIL pos) )
       | TypeVar_ s => (Trace.out "TypeVar_"; ERROR pos "type variable '$1'" [ s ] )
       | RuleId_ s  => (Trace.out "RuleId_"; R0 (pos, s) )
       | FuncId_ s  => (Trace.out "FuncId_"; R0 (pos, s) )
       | TypeId_ s  => (Trace.out "TypeId_"; R0 (pos, s) )
       | Id_ s	    => (Trace.out "Id_"; R0 (pos, s) )
       | Infix_ (_,_,s) => (Trace.out "Infix_"; ERROR pos "infix operator '$1'" [ s ] ) )
    end

  and infixOp tokenizer =
    let fun ERROR pos msg args = K (K (error "ident" (SOME pos,
          String_.replace ("infix operator expected here ("^msg^" found instead)") args)))
    in selectNext tokenizer ( fn pos => fn
	 Int_ i     => ERROR pos "integer constant" []
       | Float_ r   => ERROR pos "floating-point constant" []
       | Str_ s     => ERROR pos "string constant" []
       | Keyw_ s    => raise (FAIL pos)
       | TypeVar_ s => ERROR pos "type variable '$1'" [ s ]
       | RuleId_ s  => ERROR pos "rule name '$1'" [ s ]
       | FuncId_ s  => ERROR pos "function name '$1'" [ s ]
       | TypeId_ s  => ERROR pos "type name '$1'" [ s ]
       | Id_ s	    => R0 (pos, s)
       | Infix_ (_,_,s) => R0 (pos, s) )
    end

  and opPriority tokenizer =
    let fun ERROR pos =
          K (K (error "opPriority" (SOME pos, "operator priority must be an integer between 0 and 9")))
    in selectNext tokenizer ( fn pos => fn
	 Int_ i => if i >= 0 andalso i <= 9 then R0 (pos, i) else ERROR pos
       | _      => ERROR pos )
    end

  and fctIdent tokenizer =
    let fun ERROR pos msg args = K (K (error "fctIdent" (SOME pos,
          String_.replace ("identifier expected here ("^msg^" found instead)") args)))
    in Trace.out "ASM_Parser.fctIdent";
       selectNext tokenizer ( fn pos => fn
	 Int_ i     => ERROR pos "integer constant" []
       | Float_ r   => ERROR pos "floating-point constant" []
       | Str_ s     => ERROR pos "string constant" []
       | TypeVar_ s => ERROR pos "type variable '$1'" [ s ]
       | RuleId_ s  => R0 (pos, (NonInfix, s))
       | FuncId_ s  => R0 (pos, (NonInfix, s))
       | TypeId_ s  => R0 (pos, (NonInfix, s))
       | Id_ s	    => R0 (pos, (NonInfix, s))
       | Keyw_ "op_l" => R2   opPriority infixOp   (>>> (fn (prior, s) => (OpL prior, s)))
       | Keyw_ "op_r" => R2   opPriority infixOp   (>>> (fn (prior, s) => (OpR prior, s)))
       | Keyw_ s      => raise (FAIL pos)
       | Infix_ (_,_,s) => ERROR pos "infix operator '$1'" [ s ] )     (* [[ change to 'op' ??? ...]] *)
    end

  fun typevar tokenizer =
    let fun ERROR pos = K (K (error "typevar" (SOME pos, "type variable expected here")))
    in selectNext tokenizer ( fn pos => fn
	 TypeVar_ s => R0 (pos, s)
       | _          => ERROR pos )
    end

  fun tokenSatisfying pred tokenizer =
    let
    in selectNext tokenizer ( fn pos => fn
	 tok => if pred tok then R0 (pos, tok) else raise (FAIL pos) )
    end

  (* --- types --- *)

  local
    val >> = >>>
    fun product [t] = t
      | product ts  = Tuple ts
  in
    fun parseType_ simpleType_ product_ tokenizer =
      let fun TIMES (Infix_ (_,_,"*")) = true
            | TIMES (Id_ "*") = true
            | TIMES _ = false
      in R1  (sepSeq TIMES simpleType_)   (>> (fn ts => product_ ts))  tokenizer
      end

    fun type_ tokenizer = parseType_ simpleType_ product tokenizer
    and typeSeq tokenizer  = commaSeq type_ tokenizer
    and typeSeq0 tokenizer = commaSeq0 type_ tokenizer
    and simpleType_ (tokenizer :(POSITION * TOKEN, substring) tracking_reader) =
      let fun ERROR pos msg args = K (K (error "simpleType" ( SOME pos,
	    String_.replace ("type expected here ("^msg^" found instead)") args )))
      in selectThis tokenizer ( fn pos0 => fn
	   TypeVar_ s => (  R1   id                         (>> (fn _ => TypeParam s))  )
	 | TypeId_ T  => (  R4   id ($"(") typeSeq ($")")   (>> (fn (_,_,ts,_) => BaseType (T, ts)))
			 || R1   id                         (>> (fn _ => BaseType (T, [])))  )
         | Keyw_ "("  => (  R3   ($"(") typeSeq0 ($")")     (>> (fn (_,ts,_) => product ts))  )
         | Keyw_ "["  => (  R3   ($"[") type_ ($"]")        (>> (fn (_,t,_) => List t))  )
         | Keyw_ "{"  => (  R3   ($"{") type_ ($"}")                 (>> (fn (_,t,_) => Set t))
                         || R5   ($"{") type_ ($"->") type_ ($"}")   (>> (fn (_,t,_,t',_) => Map(t,t')))  )
	 | Keyw_ s        => raise (FAIL pos0)
	 | RuleId_ s      => raise (FAIL pos0)  (* ERROR pos0 "rule name '$1'" [ s ]*)
	 | FuncId_ s      => raise (FAIL pos0)  (* ERROR pos0 "function name '$1'" [ s ]*)
	 | Id_ s	  => raise (FAIL pos0)  (*ERROR pos0 "'$1'" [ s ]*)
	 | Infix_ (_,_,s) => ERROR pos0 "infix operator '$1'" [ s ]
	 | Int_ i    => raise (FAIL pos0)
	 | Float_ r  => raise (FAIL pos0)
	 | Str_ s    => raise (FAIL pos0) )
      end

    fun preparseType_ tokenizer = parseType_ preparseSimpleType_ (fn _ => ()) tokenizer
    and preparseTypeSeq tokenizer  = commaSeq preparseType_ tokenizer
    and preparseTypeSeq0 tokenizer = commaSeq0 preparseType_ tokenizer
    and preparseSimpleType_ tokenizer =
      let val type_ = preparseType_
          val typeSeq = preparseTypeSeq
          val typeSeq0 = preparseTypeSeq0
          fun ERROR pos msg args = K (K (error "preparseSimpleType" ( SOME pos,
	    String_.replace ("type expected here ("^msg^" found instead)") args )))
      in selectThis tokenizer ( fn pos0 => fn
	   TypeVar_ s => (  R1   id                         (>> (fn _ => ()))  )
	 | TypeId_ T  => (  R4   id ($"(") typeSeq ($")")   (>> (fn _ => ()))
			 || R1   id                         (>> (fn _ => ()))  )
         | Id_ T      => (  R4   id ($"(") typeSeq ($")")   (>> (fn _ => ()))
			 || R1   id                         (>> (fn _ => ()))  )
         | Keyw_ "("  => (  R3   ($"(") typeSeq0 ($")")     (>> (fn _ => ()))  )
         | Keyw_ "["  => (  R3   ($"[") type_ ($"]")        (>> (fn _ => ()))  )
         | Keyw_ "{"  => (  R3   ($"{") type_ ($"}")                 (>> (fn _ => ()))
                         || R5   ($"{") type_ ($"->") type_ ($"}")   (>> (fn _ => ()))  )
	 | Keyw_ s        => raise (FAIL pos0)
	 | RuleId_ s      => ERROR pos0 "rule name '$1'" [ s ]
	 | FuncId_ s      => ERROR pos0 "function name '$1'" [ s ]
	 | Infix_ (_,_,s) => ERROR pos0 "infix operator '$1'" [ s ]
	 | _              => raise (FAIL pos0) )
      end

    fun type_ x = parseType_ simpleType_ product x
    fun preParseType_ x = parseType_ preparseSimpleType_ (fn _ => ()) x

    fun funcType tokenizer =
      (R3  type_ ($"->") type_  (>> (fn (t_dom,_,t_ran) => FuncType (t_dom, t_ran)))) tokenizer
  end


  (* --- opParse: parses terms with infix operators, according to specified associativity/precedence --- *)

  local
    datatype 'a STACK_ELEM =
      Opnd of 'a                        (* operand  *)
    | Optr of ASSOC * int * string      (* operator *)

    type 'a STACK = (POSITION * 'a STACK_ELEM) list
  in
    fun opParse (parseSimple, posCons, appCons, tupleCons) tokenizer =
      let val error = fn s => error "opParse" s
          val chooseAndProceed = chooseAndProceed tokenizer
          val simple = R1 parseSimple (fn pos => fn t => (pos, t)) tokenizer
	  fun reduce [(pos, Opnd t)] = [(pos, Opnd t)]
	    | reduce ((posR, Opnd tR) :: (_, Optr (_, _, oper)) :: (posL, Opnd tL) :: rest) =
                let val pos = posL until posR
                    val t   = posCons (SOME pos, appCons (Id oper, posCons (NONE, tupleCons [ tL, tR ])))
                in (pos, Opnd t) :: rest
		end
	    | reduce [] = error (NONE, "operator parsing (reduce / empty stack)")
	    | reduce ((pos, _) :: stack) = error (SOME pos, "operator parsing (reduce)")
          fun result stack s =
            let fun F [(pos, Opnd t)] = (pos, t)
                  | F stack    = F (reduce stack)
            in SOME (F stack, s)
            end
          fun F (stack as [(p1,Opnd t1)]) old_s =
		( chooseAndProceed ( fn op_p =>
		     fn Infix_ oper =>
		       C simple (fn (p2,t2) => fn s => F [(p2,Opnd t2), (op_p,Optr oper), (p1,Opnd t1)] s s)
		   | _ => K (result stack old_s) )
                  ( result stack ) )
	    | F (stack as ((pos2, Opnd t2) :: (op1_p, Optr (assoc1, prec1, funcId1)) :: stackRest)) old_s =
	        ( chooseAndProceed ( fn op2_p =>
		     fn Infix_ (oper2 as (assoc2, prec2, funcId2)) =>
		      ( if (prec1 < prec2) orelse
			   (prec1 = prec2 andalso assoc1 = Right andalso assoc2 = Right)
			then C simple (fn (p3,t3) => fn s => F ((p3,Opnd t3)::(op2_p,Optr oper2)::stack) s s)
			else if (prec1 > prec2) orelse
				(prec1 = prec2 andalso assoc1 = Left andalso assoc2 = Left)
			then fn s => F (reduce stack) old_s old_s
			else (* prec1 = prec2 andalso assoc1 <> assoc2: resolve conflict as reduce... *)
			     fn s => F (reduce stack) old_s old_s )
		    | _ => K (result stack old_s) )
		  ( K (result stack old_s) ) )
            | F _ _ = error (NONE, "operator parsing (badly formed stack)")    (* should never occur! *)
          fun F0 stack old_s = (C simple (fn (p1, t1) => fn s => F ((p1, Opnd t1) :: stack) s s)) old_s
      in F0 []
      end
  end


  (* --- shared by terms and patterns --- *)

  fun listEnum (elem, posCons, appCons, tupleCons) tokenizer s0 =
    let val listEnum = listEnum (elem, posCons, appCons, tupleCons)
	fun nil pos = posCons (SOME (Pos { first = pos, last = pos }), appCons (Id "nil", posCons (NONE, tupleCons [])))
	fun cons pos (h, t) = posCons (SOME pos, appCons (Id "::", posCons (NONE, tupleCons [ h, t ])))
    in try_else NONE (elem tokenizer)
	 ( fn (pos1, h) => try_else (SOME pos1) ($"," tokenizer)
	   ( fn (pos2, _) => try_else (SOME pos2) (listEnum tokenizer)
	     ( fn (pos3, t) => fn s' =>
		 SOME ((pos1 until pos3, cons (pos1 until pos3) (h, t)), s') )
	     ( fn (pos, _) => raise (FAIL pos) ) )
	   ( fn (_, s) => SOME ((pos1, cons pos1 (h, nil (lst pos1))), s) ) )
	 ( fn (pos, s) => raise (FAIL pos) )
	 s0
    end

  fun typeConstraintAfter elem (posCons, typeConstrCons) tokenizer s0 =
    let val (elem, type_) = (elem tokenizer, type_ tokenizer)
    in try_else NONE elem
	 ( fn (pos1, p) => try_else (SOME pos1) ($":" tokenizer)
	   ( fn (pos2, _) => try_else (SOME pos2) type_
	     ( fn (pos3, T) => fn s' =>
		 SOME ((pos1 until pos3, posCons (SOME (pos1 until pos3), typeConstrCons (T, p))), s') )
	     ( fn (pos, _) => raise (FAIL pos) ) )
	   ( fn (_, s) => SOME ((pos1, p), s) ) )
	 ( fn (pos, s) => raise (FAIL pos) )
	 s0
    end


  (* --- patterns --- *)

  fun patt tokenizer =
    typeConstraintAfter
      (opParse (simplePatt, PattPos, AppPatt, TuplePatt))
      (PattPos, PattType) tokenizer

  and pattSeq tokenizer  = commaSeq patt tokenizer
  and pattSeq0 tokenizer = commaSeq0 patt tokenizer
  and pattList tokenizer = listEnum (patt, PattPos, AppPatt, TuplePatt) tokenizer

  and simplePatt tokenizer =
    let fun ERROR pos msg args =
	  K (K (error "simplePatt" (SOME pos, String_.replace ("pattern expected here ("^msg^" found instead)") args)))
	fun %% (PattPos (_, t)) = t
	fun >> f pos args = (pos, PattPos (SOME pos, f args))
	fun TuplePatt_ [p] = %% p
	  | TuplePatt_ ps  = TuplePatt ps
	fun AppPatt_ (f, [p]) = AppPatt (f, p)
	  | AppPatt_ (f, ps)  = AppPatt (f, PattPos (NONE, TuplePatt ps))
	fun AppPatt0 f = AppPatt_ (f, [])
    in selectThis tokenizer ( fn pos0 => fn
	   Int_ i    => (  R1   name     (>> (fn c => AppPatt0 c))  )
	 | Float_ r  => (  R1   name     (>> (fn c => AppPatt0 c))  )
	 | Str_ s    => (  R1   name     (>> (fn c => AppPatt0 c))  )

         | Id_ s     => (  R2   name ($"(")   (>> (fn (f,_) => (error "simplePatt"
                                                                (SOME pos0, "unbound name '"^(showName f)^"'"))))
	                || R1   name     (>> (fn _ => VarPatt s))  )

	 | Keyw_ "_" => (  R1   Misc.id  (>> (fn _ => Placeholder))  )
	 | Keyw_ "(" => (  R3   ($"(") pattSeq0 ($")")      (>> (fn (_,ts,_) => TuplePatt_ ts))  )
	 | FuncId_ s => (  R4   name ($"(") pattSeq0 ($")") (>> (fn (f,_,ts,_) => AppPatt_ (f, ts)))
			|| R1   name                        (>> (fn f => AppPatt0 f))  )
	 | Keyw_ "[" => (  R2   ($"[") ($"]")               (>> (fn (_,_) => %% nilPatt))
			|| R3   ($"[") pattList ($"]")      (>> (fn (_,ps,_) => %% ps))  )
	 | Keyw_ _   => raise (FAIL pos0)
	 | RuleId_ s => ERROR pos0 "rule name '$1'" [ s ]
	 | TypeId_ s => ERROR pos0 "type name '$1'" [ s ]
	 | TypeVar_ s => ERROR pos0 "type variable '$1'" [ s ]
	 | Infix_ (_,_,s) => ERROR pos0 "infix operator '$1'" [ s ] )
    end


  (* --- shared functions for terms and rules --- *)

  fun parseLet >> (term, what, letCons) =
    ( R7  ($"let") patt ($"==") term ($"in") what ($"endlet" || $"end")
	  (>> (fn (_,p,_,t,_,t',_) => letCons (p, t, t'))) )

  fun parseCond >> (term, what, condCons, defaultTail) =
    let val ifThen     = R4  ($"if") term ($"then") what               (>>> (fn (_,G,_,x) => (G,x)))
	val elseifThen = R4  ($"elseif") term ($"then") what           (>>> (fn (_,G,_,x) => (G,x)))
	val else_      = R2  ($"else") what                        (>>> (fn (_,x) => (trueTerm, x)))
	val optElse    = R1  (opt else_)    (>>> (fn SOME x => [x] | NONE => defaultTail))
    in R4  ifThen
	   (seq0 elseifThen)
	   (optElse)
	   ($"endif" || $"end")   (>> (fn (ifs, elseif, else_, _) => condCons (ifs :: (elseif @ else_))))
    end

  fun parseCase >> (term, patt, what, caseCons, defaultTail) =
    let val pattWhatPair = R3  patt ($":") what      (>>> (fn (p,_,x) => (p,x)))
	val otherwise_   = R2  ($"otherwise") what   (>>> (fn (_,x) => (PattPos(NONE,Placeholder),x)))
	val optOtherwise = R1  (opt otherwise_)
			       (>>> (fn SOME x => [x] | NONE => defaultTail))
    in R7  ($"case") term ($"of")
	     (semicolonSeq pattWhatPair) (opt ($";"))
	     (optOtherwise)
	   ($"endcase" || $"end")     (>> (fn (_,t,_,pts,_,otherw,_) => caseCons (t, pts @ otherw)))
    end


  (* --- terms --- *)

  fun with_ withKeyw tokenizer =
    let val with_   = R2   withKeyw term   (>>> (fn (_,t) => t))
    in R1   (opt with_)   (>>> (fn SOME G => G | NONE => trueTerm))   tokenizer
    end

  and qualifier tokenizer =
    R4   patt ($"in") term (with_ ($"with"))   (>>> (fn (p,_,A,G) => (p,A,G)))   tokenizer

  and term tokenizer =
    typeConstraintAfter
      (opParse (simpleTerm, TermPos, AppTerm, TupleTerm))
      (TermPos, TermType) tokenizer

  and termSeq tokenizer  = commaSeq term tokenizer
  and termSeq0 tokenizer = commaSeq0 term tokenizer
  and termList tokenizer = listEnum (term, TermPos, AppTerm, TupleTerm) tokenizer

  and simpleTerm tokenizer =
    let fun ERROR pos msg args =
	  K (K (error "simpleTerm" (SOME pos, String_.replace ("term expected here ("^msg^" found instead)") args)))

        (* --- if "undef" discipline is permissive, the parser adds default tail to case terms and to   --- *)
        (* ---   conditional terms, such that the type checker can infer that the term type is a u-type --- *)
	val ( defaultCondTail, defaultCaseTail ) =
          if !ASM_Global.undefDiscipline = ASM_Global.Permissive
	  then ( [(trueTerm, undefTerm)], [(PattPos(NONE,Placeholder),undefTerm)] )
	  else ( [], [] )

	fun %% (TermPos (_, t)) = t
	fun >> f pos args = (pos, TermPos (SOME pos, f args))
	fun TupleTerm_ [t] = %% t
	  | TupleTerm_ ts  = TupleTerm ts
	fun AppTerm_ (f, [t]) = AppTerm (f, t)
	  | AppTerm_ (f, ts)  = AppTerm (f, TermPos (NONE, TupleTerm ts))
	fun AppTerm0 f = AppTerm_ (f, [])
        fun Int i = TermPos (NONE, AppTerm0 (IntConst i))
    in selectThis tokenizer ( fn pos0 => fn
	 Int_ i    => (  R1   name   (>> (fn c => AppTerm0 c))  )
       | Float_ r  => (  R1   name   (>> (fn c => AppTerm0 c))  )
       | Str_ s    => (  R1   name   (>> (fn c => AppTerm0 c))  )

       | Id_ s     => (  R2   name ($"(")   (>> (fn (f,_) => (error "simpleTerm"
                                                                (SOME pos0, "unbound name '"^(showName f)^"'"))))
                      || R1   name          (>> (fn _ => VarTerm s))  )

       | Keyw_ "(" => (  R3   ($"(") termSeq0 ($")")
			      (>> (fn (_,ts,_) => TupleTerm_ ts))  )

       | FuncId_ s => (  R4   name ($"(") termSeq0 ($")")    (>> (fn (f,_,ts,_) => AppTerm_ (f, ts)))
		      || R2   (!!(FuncId_"not")) simpleTerm  (>> (fn(_,t) => AppTerm (Id "not", t)))
		      || R1   name                           (>> (fn f => AppTerm0 f))  )

       | Keyw_ "if"   => parseCond >> (term, term, CondTerm, defaultCondTail)
       | Keyw_ "case" => parseCase >> (term, patt, term, CaseTerm, defaultCaseTail)
       | Keyw_ "let"  => parseLet >> (term, term, LetTerm)

       | Keyw_ "["    => (  R2   ($"[") ($"]")
				 (>> (fn (_,_) => %% nilTerm))
			 || R5   ($"[") term ($"|") qualifier ($"]")
				 (>> (fn (_,t,_,q,_) => ListComprTerm (t, q)))
			 || R3   ($"[") termList ($"]")
				 (>> (fn (_,ts,_) => %% ts))
                         || R5   ($"[") term ($"..") term ($"]")
                                 (>> (fn (_,t1,_,t2,_) => AppTerm_ (Id "list_interval", [t1, t2, Int 1]))) )

       | Keyw_ "{"    => (  R2   ($"{") ($"}")
				 (>> (fn (_,_) => AppTerm0 (Id "emptyset")))
			 || R5   ($"{") term ($"|") qualifier ($"}")
				 (>> (fn (_,t,_,q,_) => FSetComprTerm (t, q)))
			 || R3   ($"{") (commaSeq term) ($"}")
				 (>> (fn (_,ts,_) => FSetEnumTerm ts))
			 || R7   ($"{") term ($"->") term ($"|") qualifier ($"}")
				 (>> (fn (_,t1,_,t2,_,q,_) => FMapComprTerm ((t1,t2), q)))
			 || R3   ($"{") (commaSeq ( R3 term ($"->") term
						    (>>> (fn (t1,_,t2) => (t1,t2))) )) ($"}")
				 (>> (fn (_,t2s,_) => FMapEnumTerm t2s))
                         || R5   ($"{") term ($"..") term ($"}")
                                 (>> (fn (_,t1,_,t2,_) => AppTerm_ (Id "set_interval", [t1, t2, Int 1])))  )

       | Keyw_ "exists" => (  R5   ($"exists") patt ($"in") term (with_ ($":"))
			           (>> (fn (_,p,_,A,G) => ExistsTerm (p,A,G)))  )
       | Keyw_ "forall" => (  R5   ($"forall") patt ($"in") term (with_ ($":"))
			           (>> (fn (_,p,_,A,G) => ForallTerm (p,A,G)))  )
       | Keyw_ "FUN_TO_MAP" => (  R1   name   (>> (fn (id) => AppTerm0 id )) )
       | Keyw_ "REL_TO_SET" => (  R1   name   (>> (fn (id) => AppTerm0 id )) )
       | Keyw_ _   => raise (FAIL pos0)
       | RuleId_ s => ERROR pos0 "rule name '$1'" [ s ]
       | TypeId_ s => ERROR pos0 "type name '$1'" [ s ]
       | TypeVar_ s => ERROR pos0 "type variable '$1'" [ s ]

       (* special case *)
       | unaryMinus as Infix_ (_,_,"-") =>
	   R2   (!!unaryMinus) simpleTerm   (>> (fn(_,t) => AppTerm_ (Id "~", [t])))

       | Infix_ (_,_,s) => ERROR pos0 "infix operator '$1'" [ s ] )
    end


  (* --- rules --- *)

  fun rule tokenizer = implicitBlockRule  tokenizer
  and implicitBlockRule tokenizer =
    R1 (seq simpleRule)
       (fn pos => fn [R] => (pos, R) | Rs => (pos, RulePos (SOME pos, BlockRule Rs))) tokenizer
  and ruleSeq0 tokenizer = seq0 rule tokenizer
  and simpleRule tokenizer =
    let fun ERROR pos msg args = K (K (error "simpleRule" ( SOME pos,
	      String_.replace ("rule expected here ("^msg^" found instead)") args )))
	fun %% (RulePos (_, t)) = t
	fun >> f pos args = (pos, RulePos (SOME pos, f args))
	fun UpdateRule_ ((f, [t]), t') = UpdateRule ((f, t), t')
	  | UpdateRule_ ((f, ts), t') = UpdateRule ((f, TermPos (NONE, TupleTerm ts)), t')
	fun UpdateRule0 (f, t') = UpdateRule_ ((f, []), t')
	fun AppRule_ (f, [t]) = AppRule (f, t)
	  | AppRule_ (f, ts)  = AppRule (f, TermPos (NONE, TupleTerm ts))
	fun AppRule0 f = AppRule_ (f, [])
    in selectThis tokenizer ( fn pos0 => fn
	 RuleId_ s => (  R4   name ($"(") termSeq0 ($")")    (>> (fn (f,_,ts,_) => AppRule_ (f, ts)))
		      || R1   name                           (>> (fn f => AppRule0 f))  )

       | Id_ s     => (  R2   name ($"(")   (>> (fn (f,_) => (error "simpleRule"
                                                                (SOME pos0, "unbound name '"^(showName f)^"'"))))
                      || R2   name ($":=")   (>> (fn (f,_) => (error "simpleRule"
                                                                (SOME pos0, "unbound name '"^(showName f)^"'"))))
                      || R1   name   (>> (fn (f) => (error "simpleRule"
                                                           (SOME pos0, "unbound name '"^(showName f)^"'")))) )
(*
                      || (raise (FAIL pos0))  )
*)

       | FuncId_ s => (  R6   name ($"(") termSeq0 ($")") ($":=") term
			      (>> (fn (f,_,ts,_,_,t') => UpdateRule_ ((f, ts), t')))
		      || R3   name ($":=") term
			      (>> (fn (f,_,t') => UpdateRule0 (f, t')))  )

       | Keyw_ "block"  => R3   ($"block") (seq0 rule) ($"endblock" || $"end")
				(>> (fn (_,Rs,_) => BlockRule Rs))

       | Keyw_ "skip"   => R1   ($"skip")   (>> (fn _ => BlockRule []))

       | Keyw_ "if"     => parseCond >> (term, rule, CondRule, [(trueTerm,skipRule)])
       | Keyw_ "case"   => parseCase >> (term, patt, rule, CaseRule, [(PattPos(NONE,Placeholder),skipRule)])

       | Keyw_ "let"    => parseLet >> (term, rule, LetRule)
       | Keyw_ "do"     => R5  ($"do") ($"forall") qualifier rule ($"enddo" || $"end")
			       (>> (fn (_,_,q,R,_) => ForallRule (q, R)))
       | Keyw_ "choose" => R4  ($"choose") qualifier rule ($"endchoose" || $"end")
			       (>> (fn (_,q,R,_) => ChooseRule (q, R)))

       | TypeId_ s => ERROR pos0 "type name '$1'" [ s ]
       | _ => raise (FAIL pos0) )
    end


  (* --- function and transition expressions --- *)

  fun TuplePatt_ [PattPos (pos, p)] = PattPos (pos, p)
    | TuplePatt_ [] = PattPos (NONE, TuplePatt [])
    | TuplePatt_ ps = let val (PattPos (first, _), PattPos (last, _)) = (hd ps, hd (rev ps))
		      in PattPos (first until' last, TuplePatt ps) end

  fun functionExpr tokenizer =
    let fun ERROR pos msg args = K (K (error "functionExpr" ( SOME pos,
	      String_.replace ("function expression expected here ("^msg^" found instead)") args )))
	fun >> f pos args = (pos, FunctionExprPos (SOME pos, f args))
    in selectThis tokenizer ( fn pos0 => fn
	 Keyw_ "fn" => (  R6   ($"fn") ($"(") pattSeq0 ($")") ($"->") term
                               (>> (fn (_,_,ps,_,_,t) => LambdaTerm (TuplePatt_ ps, t)))  )
       | Keyw_ "MAP_TO_FUN" => (  R2   ($"MAP_TO_FUN") term   (>> (fn (_,t) => MapToFun t))  )
       | Keyw_ "SET_TO_REL" => (  R2   ($"SET_TO_REL") term   (>> (fn (_,t) => SetToRel t))  )
       | _ => raise (FAIL pos0) )
    end

  fun transitionExpr tokenizer =
    let fun ERROR pos msg args = K (K (error "transitionExpr" ( SOME pos,
	      String_.replace ("transition expression expected here ("^msg^" found instead)") args )))
	fun >> f pos args = (pos, TransitionExprPos (SOME pos, f args))
    in selectThis tokenizer ( fn pos0 => fn
	 Keyw_ "tn" => (  R6   ($"tn") ($"(") pattSeq0 ($")") ($"->") rule
                               (>> (fn (_,_,ps,_,_,R) => LambdaRule (TuplePatt_ ps, R)))  )
       | _ => raise (FAIL pos0) )
    end


  (* --- definitions --- *)

  fun optTypevars x =
    let val typevarSeq = R3   ($"(") (commaSeq typevar) ($")")   (>>> (fn (_,typevars,_) => typevars))
    in R1   (opt typevarSeq)   (>>>(fn (SOME x) => x | NONE => []))   x
    end

  fun optPatts x =
    let val patts = R3   ($"(") pattSeq ($")")   (>>> (fn (_,ps,_) => ps))
    in R1   (opt patts)   (>>>(fn (SOME ps) => TuplePatt_ ps | NONE => TuplePatt_ []))   x
    end

  fun defOpeningKeyword tok =
    case tok of
      Keyw_ s => s = "freetype" orelse s = "datatype" orelse
                 s = "simultaneous" orelse s = "{" orelse s = "typealias" orelse
	         s = "rule" orelse s = "transition" orelse
                 s = "static" orelse s = "dynamic" orelse s = "derived" orelse s = "external" orelse
                 s = "function" orelse s = "relation"
    | _ => false

  fun restOfDef x = (seq (tokenSatisfying (not o defOpeningKeyword))) x

  fun freetypeDefs (simultaneous :bool) (ctxDepTokenizer, ctx :CONTEXT) (s0 as (tokenPos0, pos0, _)) =
    let val tokenizer = ctxDepTokenizer ctx
        val _ = prescanMode ()
        val preParseConstructor =
          let val consType_ = R2   ($":") preparseType_   (>>>(fn _ => ()))
          in R2   fctIdent (opt consType_)
                  (>>> (fn (fct_id as (f_opStatus, f_name), _) => (f_name, (FuncKind, f_opStatus))))
          end
        val preParseOne =
	  (  R7   ($"freetype" || $"datatype" ) ident optTypevars ($"==") ($"{") (commaSeq preParseConstructor) ($"}")
		  (>>> (fn (_,T_name,_,_,_,conss,_) => (((T_name, (TypeKind, NonInfix))) :: conss)))  )
        val preParse =
	  if simultaneous
          then (  R1   (seq preParseOne)   (>>> List.concat)  )
          else (  R1   (preParseOne)       (>>> Misc.id)  )
        val ((_, newNames), s') =
          (valOf (preParse tokenizer s0)) handle Option => ((Pos {first = pos0, last = pos0}, []), s0)
        val ctx' = addNames ctx newNames
        val tokenizer' = ctxDepTokenizer ctx'
        val _ = rescanMode tokenPos0
        val constructor =
          let val consType_ = R2   ($":") type_   (>>>(fn (_,t) => t))
          in R2   fctIdent (opt consType_)
                  (>>>(fn (fct_id, consTypeOpt) => (fct_id, getOpt (consTypeOpt, Unit))))
          end
        val parseOne =
          (  R7   ($"freetype" || $"datatype" ) ident optTypevars ($"==") ($"{") (commaSeq constructor) ($"}")
                  (>>>( fn (_,T_name,tyvars,_,_,conss,_) => (T_name, tyvars, conss) )) )
        fun >> f pos args = (pos, (ctx', DefPos (SOME pos, f args)))
        val result =
          if simultaneous
          then (  R1   (seq parseOne)   (>> (fn xs => FreetypeDef xs))  )   tokenizer' s0
          else (  R1   parseOne         (>> (fn x  => FreetypeDef [x]))  )   tokenizer' s0
        val _ = normalScanMode ()
    in result
    end

  fun optType x      = opt (R2 ($":") type_ (>>> #2)) x
  fun abbrFuncType x = (   R1 funcType (>>> Misc.id)
                       ||  R1 type_    (>>> (fn T => FuncType (Unit, T))) ) x
  fun optFuncType x  = opt (R2 ($":") abbrFuncType (>>> #2)) x

  fun optRangeConstr x = opt
    (  R5   ($"with") fctIdent optPatts ($"in") term
            (>>>(fn (_,f,arg,_,t) => FunctionExprPos (NONE, LambdaTerm (arg, t))))  )  x


  fun staticOrDerived (attr, attrString) (simultaneous: bool) (ctxDepTokenizer, ctx :CONTEXT)
                      (s0 as (tokenPos0, pos0, _)) =
    let val tokenizer = ctxDepTokenizer ctx
        val _ = prescanMode ()
        val RELATION  = ($"relation" || $"predicate")
	val preParseOne =
	  (  R4   ($attrString) (opt ($"function" || RELATION)) fctIdent restOfDef
		  (>>> (fn (_,_,(f_opStatus, f_name),_) => (f_name, (FuncKind, f_opStatus)))) )
	val preParse =
          if simultaneous
          then (  R1   (seq preParseOne)   (>>> Misc.id)  )
	  else (  R1   preParseOne         (>>> (fn f => [f]))  )
        val ((pos, newNames), s') =
          (valOf (preParse tokenizer s0)) handle Option => ((Pos {first = pos0, last = pos0}, []), s0)
        val ctx' = addNames ctx newNames
        val _ = rescanMode tokenPos0
        val tokenizer' = ctxDepTokenizer ctx'
        val parseOne =
          (  R7   ($attrString) (opt ($"function")) fctIdent optPatts optType ($"==") term
		  (>>>( fn (_,_,f,arg,T,_,t) =>
                          let val constr' = addTypeConstraint NoConstraint
			                     (Option.map (fn T => FuncType (TypeParam "a", T)) T)
			  in (f, constr', FunctionExprPos (NONE, LambdaTerm (arg, t)))
			  end ))
          || R6   ($attrString) RELATION fctIdent optPatts ($"==") term
		  (>>>( fn (_,_,f,arg,_,t) =>
                          let val constr' = addTypeConstraint NoConstraint (SOME (FuncType (TypeParam "a", Bool)))
			  in (f, constr', FunctionExprPos (NONE, LambdaTerm (arg, t)))
			  end ))
          || R6   ($attrString) (opt ($"function")) fctIdent optFuncType ($"==") functionExpr
                  (>>>( fn (_,_,f,T,_,FE) =>
                          let val constr' = addTypeConstraint NoConstraint T
                          in (f, constr', FE)
                          end ))
          || R6   ($attrString) RELATION fctIdent optType ($"==") functionExpr
                  (>>>( fn (_,_,f,T,_,FE) =>
                          let val constr' = addTypeConstraint NoConstraint
                                              (Option.map (fn T => FuncType (T, Bool)) T)
                          in (f, constr', FE)
                          end )) )
        fun >> f pos args = (pos, (ctx', DefPos (SOME pos, f args)))
        val result =
	  if simultaneous
	  then (  R1   (seq parseOne)   (>> (fn xs => FunctionDef (attr, xs)))  )   tokenizer'  s0
	  else (  R1   parseOne         (>> (fn x => FunctionDef (attr, [x])))  )   tokenizer'  s0
        val _ = normalScanMode ()
    in result
    end

  fun ruleDef ctx >> =
    let fun RuleDef_ (r, TE) = (addNames ctx [(r, (RuleKind, NonInfix))], RuleDef (r, TE))
        fun >> f pos args = let val (ctx, D') = f args in (pos, (ctx, DefPos (SOME pos, D'))) end
    in (  R5   ($"rule" || $"transition") ident optPatts ($"==") rule
               (>>( fn (_,r,arg,_,R) => RuleDef_ (r, TransitionExprPos (NONE, LambdaRule (arg, R)))))
       || R4   ($"rule" || $"transition") ident ($"==") transitionExpr
               (>>( fn (_,r,_,TE) => RuleDef_ (r, TE))) )
    end

  local
    fun constraints (typeConstr, rangeConstr) =
      addRangeConstraint (addTypeConstraint NoConstraint typeConstr) rangeConstr

    (* finite range constraint for a relation is always "/\ x . { false, true }" *)
    val relationRangeConstraint =
      FunctionExprPos (NONE, LambdaTerm ( PattPos (NONE, Placeholder),
					  TermPos (NONE, FSetEnumTerm [falseTerm,trueTerm]) ))
  in
    fun dynamicFunctionDef ctx >> =
      let val RELATION  = ($"relation" || $"predicate")
          fun DynamicFunctionDef_ (f as (f_opStatus, f_name), constr, FE) =
	    (addNames ctx [(f_name, (FuncKind, f_opStatus))], FunctionDef (Dynamic, [(f, constr, FE)]))
	  fun >> f pos args = let val (ctx, D') = f args in (pos, (ctx, DefPos (SOME pos, D'))) end
      in (  R7   ($"dynamic") ($"function") fctIdent optFuncType optRangeConstr ($"initially") functionExpr
		 (>>( fn (_,_,f,T,rc,_,FE) => DynamicFunctionDef_ (f, constraints (T, rc), FE) ))
	 || R7   ($"dynamic") ($"function") fctIdent optType optRangeConstr ($"initially") term
		 (>>( fn (_,_,f,T,rc,_,t) =>
			DynamicFunctionDef_ ( f,
			  constraints (Option.map (fn T => FuncType (Unit, T)) T, rc),
			  FunctionExprPos (NONE, MapToFun (TermPos (NONE,
			    FMapEnumTerm [ (TermPos (NONE, TupleTerm []), t) ])))) ))
	 || R6   ($"dynamic") ($"relation") fctIdent optRangeConstr ($"initially") term
		 (>>( fn (_,_,f,rc,_,t) =>
		        DynamicFunctionDef_ ( f,
			  constraints (SOME (FuncType (Unit, Bool)), rc),
			  FunctionExprPos (NONE, MapToFun (TermPos (NONE,
			    FMapEnumTerm [ (TermPos (NONE, TupleTerm []), t) ])))) ))
	 || R6   ($"dynamic") RELATION fctIdent optType ($"initially") functionExpr
		 (>>( fn (_,_,f,T,_,FE) =>
			DynamicFunctionDef_ ( f,
			  constraints ( Option.map (fn T => FuncType (T, Bool)) T,
                                        SOME relationRangeConstraint ),
                          FE ))) )
      end

    fun externalFunctionDef ctx >> =
      let val dummyFuncExpr = FunctionExprPos(NONE,LambdaTerm (TuplePatt_ [], TermPos(NONE,TupleTerm [])))
	  fun ExternalFunctionDef_ (f as (f_opStatus, f_name), constr, FE) =
	    (addNames ctx [(f_name, (FuncKind, f_opStatus))], FunctionDef (External, [(f, constr, FE)]))
      in (  R5   ($"external") ($"function") fctIdent optFuncType optRangeConstr
		 (>>( fn (_,_,f,T,rc) =>
                        ExternalFunctionDef_ (f, constraints (T, rc), dummyFuncExpr)))
         || R5   ($"external") ($"relation") fctIdent optType optRangeConstr
		 (>>( fn (_,_,f,T,rc) =>
		        ExternalFunctionDef_ ( f,
			  constraints (Option.map (fn T => FuncType (T, Bool)) T, rc),
                          dummyFuncExpr))) )
      end
  end

  fun def (ctxDepTokenizer, ctx :CONTEXT) s0 =
    let fun >> f pos args = (pos, DefPos (SOME pos, f args))
        fun defSeq_ []  = DefPos (NONE, DefSeq [])
          | defSeq_ [D] = D
          | defSeq_ Ds  =
              let val (DefPos (first, _), DefPos (last, _)) = (hd Ds, hd (rev Ds))
              in DefPos (first until' last, DefSeq Ds) end
        fun defs (ctxDepTokenizer, ctx) s =
        ( case simpleDef (ctxDepTokenizer, ctx) s of
 	    SOME ((pos', (ctx', D)), s') =>
              ( case defs (ctxDepTokenizer, ctx') s' of
                  SOME ((pos'', (ctx'', Ds)), s'') => SOME ((pos' until pos'', (ctx'', D :: Ds)), s'')
                | NONE => SOME ((pos', (ctx', [D])), s') )
            | _ => NONE )
    in Option.map (fn ((pos, (ctx, Ds)), s) => ((pos, defSeq_ Ds), s)) (defs (ctxDepTokenizer, ctx) s0)
    end
  and simpleDef (ctxDepTokenizer, ctx :CONTEXT) =
    let fun >> f pos args = let val (ctx, D') = f args in (pos, (ctx, DefPos (SOME pos, D'))) end
        val staticFunctionDefs  = staticOrDerived (Static, "static")
        val derivedFunctionDefs = staticOrDerived (Derived, "derived")
	fun simultaneousDefs tokenizer =
	  let fun >> f pos args = (pos, DefPos (SOME pos, f args))
	  in selectThis (ctxDepTokenizer ctx) ( fn pos0 => fn
	       Keyw_ "static"   => (fn _ => staticFunctionDefs true (ctxDepTokenizer, ctx))
	     | Keyw_ "derived"  => (fn _ => derivedFunctionDefs true (ctxDepTokenizer, ctx))
	     | Keyw_ "datatype" => (fn _ => freetypeDefs true (ctxDepTokenizer, ctx))
	     | Keyw_ "freetype" => (fn _ => freetypeDefs true (ctxDepTokenizer, ctx))
             | _                => error "def" (SOME pos0, "simultaneous definitions are only allowed for freetypes, \
                                                           \static and derived functions") )
	  end
        fun TypealiasDef_ (args as (T_name, _, _)) =
          (addNames ctx [(T_name, (TypeKind, NonInfix))], TypealiasDef args)
    in selectThis (ctxDepTokenizer ctx) ( fn pos0 => fn
	 Keyw_ "typealias" =>
           (  R5   ($"typealias") ident optTypevars ($"==") type_
		   (>> (fn (_,T_name,typevars,_,T) => TypealiasDef_ (T_name, typevars, T)))  )
       | Keyw_ "freetype"     => (fn _ => freetypeDefs false (ctxDepTokenizer, ctx))
       | Keyw_ "datatype"     => (fn _ => freetypeDefs false (ctxDepTokenizer, ctx))
       | Keyw_ "static"       => (fn _ => staticFunctionDefs false (ctxDepTokenizer, ctx))
       | Keyw_ "derived"      => (fn _ => derivedFunctionDefs false (ctxDepTokenizer, ctx))
       | Keyw_ "simultaneous" => (  R4   ($"simultaneous") ($"{") simultaneousDefs ($"}")   (>>> #3)  )
       | Keyw_ "{"            => (  R3   ($"{") simultaneousDefs ($"}")   (>>> #2)  )
       | Keyw_ "dynamic"      => dynamicFunctionDef ctx >>
       | Keyw_ "external"     => externalFunctionDef ctx >>
       | Keyw_ "rule"         => ruleDef ctx >>
       | Keyw_ "transition"   => ruleDef ctx >>
       | _ => raise (FAIL pos0) )
    end


  (* --- exported functions --- *)

  local
    val (name', type_', patt', term', rule') = (name, type_, patt, term, rule)
    val (functionExpr', transitionExpr', def') = (functionExpr, transitionExpr, def)
    fun export fctName fct x y =
    ( valOf (fct x y)
      handle Option     => error fctName (NONE, "unexpected end of file/stream")
	   | FAIL pos   => error fctName (SOME pos, "syntax error") )
  in
    fun name x y  = export "name" name' x y
    fun type_ x y = export "type_" type_' x y
    fun patt x y  = export "patt" patt' x y
    fun term x y  = export "term" term' x y
    fun rule x y  = export "rule" rule' x y
    fun functionExpr x y = export "functionExpr" functionExpr' x y
    fun transitionExpr x y = export "transitionExpr" transitionExpr' x y
    fun def x y = export "def" def' x y
  end
end
