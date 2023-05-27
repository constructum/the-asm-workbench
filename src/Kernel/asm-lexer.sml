(*
##
## "asm-lexer.sml", G. Del Castillo, Feb 2000
##
##
##   
*)


(*

NOTE

The lexer and parser are context-dependent: for instance, they need to know the
arity of functions (at least which functions are 0-ary) in order to work properly.
This information must be retrieved from the appropriate signature.


NOTE

Character-reading functions are modified to keep track of positions in source
(see for instance 'getcLC' in this module, derived from 'Substring.getc').

*)


structure ASM_Lexer =
struct
  open ASM_Global ASM_Name Misc Lexer
  infix **

  type LOCATION = ASM_Location.LOCATION
  type POSITION = ASM_Position.POSITION

  infix until
  val op until = ASM_Position.until

  datatype PROBLEM =
    InvalidLexeme of char
  | InvalidNumber
  | UnclosedComment

  exception Error of (LOCATION option * POSITION * PROBLEM) Error.ERROR


  datatype ASSOC =
    Left | Right

  datatype TOKEN =
    Int_ of int
  | Float_ of real
  | Str_ of string
  | Keyw_ of string
  | Infix_ of ASSOC * int * string
  | RuleId_ of string
  | FuncId_ of string
  | TypeId_ of string
  | Id_ of string
  | TypeVar_ of string

  fun eqToken (Int_ i1, Int_ i2) = i1 = i2
    | eqToken (Float_ r1, Float_ r2) = Real.== (r1, r2)
    | eqToken (Str_ s1, Str_ s2) = s1 = s2
    | eqToken (Keyw_ s1, Keyw_ s2) = s1 = s2
    | eqToken (Infix_ (_, _, s1), Infix_ (_, _, s2)) = s1 = s2
    | eqToken (RuleId_ s1, RuleId_ s2) = s1 = s2
    | eqToken (FuncId_ s1, FuncId_ s2) = s1 = s2
    | eqToken (TypeId_ s1, TypeId_ s2) = s1 = s2
    | eqToken (Id_ s1, Id_ s2) = s1 = s2
    | eqToken (TypeVar_ s1, TypeVar_ s2) = s1 = s2
    | eqToken (_, _) = false

  fun isSymbIdentChar c =
    case c of
      #"!" => true | #"%" => true | #"&" => true | #"$" => true
    | #"#" => true | #"+" => true | #"-" => true | #"/" => true
    | #":" => true | #"<" => true | #"=" => true | #">" => true
    | #"?" => true | #"@" => true | #"\\" => true | #"~" => true
    | #"`" => true | #"^" => true | #"|" => true | #"*" => true
    | _     => false


  datatype CHECK_STATUS =
    Check | NoCheck | RecheckFrom of int

  val checkStatus =
    ref Check

  fun checkToken (ctx :CONTEXT) s =
    if isKeyword ctx s then Keyw_ s
    else if (!checkStatus = NoCheck)
	 then Id_ s
	 else case idKind ctx s of
		     SOME TypeKind => TypeId_ s
		   | SOME RuleKind => RuleId_ s
		   | SOME FuncKind => ( case opStatus ctx s of
					  SOME (OpL i) => Infix_ (Left, i, s)
					| SOME (OpR i) => Infix_ (Right, i, s)
					| _            => FuncId_ s )
		   | NONE => Id_ s

  fun recheckToken (ctx :CONTEXT) tok =
    case tok of
      Id_ name     => checkToken ctx name
    | RuleId_ name => checkToken ctx name
    | FuncId_ name => checkToken ctx name
    | Infix_ (assoc, prec, name) => checkToken ctx name
    | TypeId_ name => checkToken ctx name
    | _ => tok


  structure Buffer =
  struct
    (* adaption of the binary search functor from the SML/NJ library *)

    val len  = 4096
    type KEY = int

    structure A :MONO_ARRAY =
    struct
      open Array
      type elem   = ((((POSITION * TOKEN) * (int * (int * int) * substring)) option))
      type vector = elem Vector.vector
      type array  = elem Array.array
    end

    val min :int ref = ref 0
    val max :int ref = ref 0
    val arr :A.array = A.array (len, NONE)

    fun get (ctx :CONTEXT) tokNum =
      if tokNum >= !min andalso tokNum < !max
      then let val result = A.sub (arr, tokNum mod len)
           in case (!checkStatus) of
                RecheckFrom pos =>
                  if tokNum >= pos
                  then SOME ( Option.map
			 ( fn ((pos, tok), (tokNum, (l,c), sstr)) =>
			     ((pos, recheckToken ctx tok), (tokNum, (l,c), sstr)) ) result )
                  else SOME result
              | _ => SOME result
           end
      else NONE

    fun append e =
    ( A.update (arr, !max mod len, e);
      max := !max + 1;
      if !max - !min >= len then min := !min + 1 else () )

    fun reset ()      = ( min := 0; max := 0 )
    fun restartAt pos = ( min := pos; max := pos )
  end   (* structure Buffer *)


  (* character reader which keeps tracks of rows and columns *)
  type ('t, 'a) tracking_reader = ('t, int * (int * int) * 'a) StringCvt.reader

  val getc = Substring.getc
  val getcLC :(char, substring) tracking_reader =
    fn (tokNum, (line, col), s) =>
      case getc s of
	res as SOME (c as #"\n", s') => SOME (c, (tokNum, (line + 1, 1), s'))
      | res as SOME (c, s')     => SOME (c, (tokNum, (line, col + 1), s'))
      | NONE => NONE

  fun countLC s0 =
    let fun F (pos as (line, col), s) =
	  case getc s of
	    SOME (#"\n", s') => F ((line + 1, 1), s')
	  | SOME (_, s')     => F ((line, col + 1), s')
	  | NONE => pos
    in F ((1, 1), Substring.full s0)
    end

  fun addLC ((l1, c1), (l2, c2)) =
    if l2 = 1
    then (l1, c1 + c2 - 1)
    else (l1 + l2 - 1, c2)

  fun splitl p (tokNum :int, pos0, s) =
    case StringCvt.splitl p getc s of
      (str, s') => (str, (tokNum, addLC (pos0, countLC str), s'))

  fun takel p s = #1 (splitl p s)
  fun dropl p s = #2 (splitl p s)
  val skipWS = dropl Char.isSpace

  fun adapt scanFct (tokNum, pos0, s) =
    case scanFct getc s of
      SOME (res, s') =>
        let val ((str,x,_), (_,x',_)) = (Substring.base s, Substring.base s')
        in SOME (res, (tokNum, addLC (pos0, countLC (Substring.string (Substring.substring (str, x, x' - x)))), s')) end
    | NONE => NONE



  val (sourceLoc :LOCATION option ref) = ref NONE

  fun setLoc loc  = ( sourceLoc := SOME loc )
  fun unsetLoc () = ( sourceLoc := NONE )

  fun message (location, position, problem) =
    let val where_ = String_.replace "$1:$2:\n"
	               [ ASM_Location.toErrorString (valOf location) handle Option => "--",
			 ASM_Position.toErrorString position ]
        val what_  = 
	  case problem of
	    InvalidLexeme c => String_.replace "invalid lexeme '$1'" [ str c ]
	  | InvalidNumber   => String_.replace "invalid numeric constant (possibly an overflow)" []
	  | UnclosedComment => String_.replace "unclosed comment" []
    in where_ ^ "  Lexical error -- " ^ what_ ^ "\n"
    end

  fun error fct (firstPos, lastPos) what =
    let fun makePos () = ASM_Position.Pos { first = firstPos, last = lastPos }
    in raise Error
       { module = "ASM_Lexer",
	 function = fct,
	 problem = (!sourceLoc, makePos (), what),
	 message = message,
	 cause = NONE }
    end


  fun resetScanner () = ( Buffer.reset (); checkStatus := Check )

  fun normalScanMode () = checkStatus := Check
  fun prescanMode ()    = checkStatus := NoCheck
  fun rescanMode pos =
    if pos < !Buffer.min
    then ( Buffer.restartAt pos; checkStatus := Check )
    else checkStatus := RecheckFrom pos


  fun scanToken (context :CONTEXT) (s as (currPos,_,_) :int * (int * int) * substring)
      : ((POSITION * TOKEN) * (int * (int * int) * substring)) option =
    let fun scanAndAppend () =
	  let val result = unbufferedScanToken context s
          in Buffer.append result;
             result
          end
    in (* print "currPos = "; print (Int.toString currPos);
          print "   min = ";  print (Int.toString (!Buffer.min));
          print "   max = ";  print (Int.toString (!Buffer.max)); print "\n"; *)
       if (currPos >= !Buffer.max)
       then scanAndAppend ()
       else case Buffer.get context currPos of
              SOME result => result
            | NONE => unbufferedScanToken context s
    end

  and unbufferedScanToken (context :CONTEXT) (s :int * (int * int) * substring)
      : ((POSITION * TOKEN) * (int * (int * int) * substring)) option =
    let val ERROR = error "scanToken"
	val s as (_, firstPos, sstr) = skipComment (skipWS s)
        fun makePos (currPos as (l, c)) x = (ASM_Position.Pos { first = firstPos, last = (l, c - 1) }, x)
	fun keyw currPos s  = makePos currPos (Keyw_ s)
        fun check currPos s = makePos currPos (checkToken context s)
        fun incTok (s as tokNum, pos, sstr) = (tokNum + 1, pos, sstr)
    in case getcLC s of
         SOME (#"(", s' as (_, pos, _)) => SOME (keyw pos "(", incTok s')
       | SOME (#")", s' as (_, pos, _)) => SOME (keyw pos ")", incTok s')
       | SOME (#"[", s' as (_, pos, _)) => SOME (keyw pos "[", incTok s')
       | SOME (#"]", s' as (_, pos, _)) => SOME (keyw pos "]", incTok s')
       | SOME (#"{", s' as (_, pos, _)) => SOME (keyw pos "{", incTok s')
       | SOME (#"}", s' as (_, pos, _)) => SOME (keyw pos "}", incTok s')
       | SOME (#",", s' as (_, pos, _)) => SOME (keyw pos ",", incTok s')
       | SOME (#"_", s' as (_, pos, _)) => SOME (keyw pos "_", incTok s')
       | SOME (#";", s' as (_, pos, _)) => SOME (keyw pos ";", incTok s')
       | SOME (#".", s' as (_, pos, _)) =>
         ( case getcLC s' of
             SOME (#".", s'' as (_, pos, _)) => SOME (makePos pos (Keyw_ ".."), incTok s'')
           | _ => ERROR (firstPos, firstPos) (InvalidLexeme #".") )
       | SOME (#"\"", s' as (_, pos, _)) =>
           Option.map (fn (str, s'' as (_, pos, _)) => (makePos pos (Str_ str), incTok s'')) (scanStr s)
       | SOME (c, s') =>
         ( if (Char.isDigit c)
	      (* !!! [[problems]]: orelse (c = #"-" andalso Char.isDigit (#1 (valOf (getcLC s'))) handle Option => false) *)
	   then ( case (scanInt s) of
		    SOME (i, s' as (_, pos, _)) => SOME (makePos pos (Int_ i), incTok s')
		  | NONE =>
		    ( case (scanFloat s) of
		      SOME (r, s' as (_, pos, _)) => SOME (makePos pos (Float_ r), incTok s')
		    | NONE => ERROR (firstPos, #2 (dropl Char.isDigit s')) InvalidNumber ) )
                handle Overflow => ERROR (firstPos, #2 (dropl Char.isDigit s')) InvalidNumber
           else if Char.isAlpha c
	   then Option.map (fn (str, s' as (_, pos, _)) => (check pos str, incTok s')) (scanIdent s)
	   else if c = #"'"
	   then case getcLC s' of
                  SOME (c', s'') =>
                    if Char.isAlpha c'
		    then Option.map
                           (fn (str, sEnd as (_,pos,_)) => (makePos pos (TypeVar_ str), incTok sEnd))
                           (scanIdent s')
                    else ERROR (firstPos, firstPos) (InvalidLexeme c)
                | NONE => ERROR (firstPos, firstPos) (InvalidLexeme c)
	   else if isSymbIdentChar c
	   then Option.map (fn (str, s' as (_, pos, _)) => (check pos str, incTok s')) (scanSymbIdent s)
           else ERROR (firstPos, firstPos) (InvalidLexeme c) )
       | NONE => NONE
    end

  and scanInt s       = adapt Lexer.scanInt s
  and scanFloat s     = adapt Real.scan s
  and scanStr s       = adapt Lexer.scanString s
  and scanIdent s     = adapt Lexer.scanIdent s
  and scanSymbIdent s = SOME (splitl isSymbIdentChar s)

  and skipComment (s0 as (_, pos0, _)) =
    let fun ERROR pos = error "skipComment" (pos0, pos) UnclosedComment
	fun continueWith s = skipComment (skipWS s)
	fun skipOneLineComment s2 =
	  let val s3 = dropl (fn c => c <> #"\n") s2
	  in case getcLC s3 of
	       SOME (_, s4) => continueWith s4
	     | NONE => s3
	  end
	fun skipMultiLineComment terminator s2 =
	  let val s3 as (_,pos3,_)= dropl (fn c => c <> #"*") s2
	  in case getcLC s3 of
	       SOME (#"*", s4 as (_,pos4,_)) => 
	       ( case getcLC s4 of
	           SOME (c, s5 as (_,pos,_)) => if c = terminator
					        then continueWith s5
					        else skipMultiLineComment terminator s5
		 | _ => ERROR pos4 )
             | _ => ERROR pos3
          end
    in case getcLC s0 of
	 SOME (#"/", s1) =>
	 ( case getcLC s1 of
	     SOME (#"/", s2) => skipOneLineComment s2
	   | SOME (#"*", s2) => skipMultiLineComment #"/" s2
	   | _ => s0 )
       | SOME (#"(", s1) =>
	 ( case getcLC s1 of
	     SOME (#"*", s2) => skipMultiLineComment #")" s2
	   | _ => s0 )
       | _ => s0
    end


  fun showToken tok =
    case tok of
      Int_ i  => Int.toString i
    | Float_ r => Real.toString r
    | Str_ s  => "\"" ^ (String.toString s) ^ "\""
    | Keyw_ s => "Keyw_  \"" ^ s ^ "\""
    | Infix_ (Left, i, s)  => "op_l " ^ (Int.toString i) ^ " " ^ s
    | Infix_ (Right, i, s) => "op_r " ^ (Int.toString i) ^ " " ^ s
    | FuncId_ s => "FuncId_ \"" ^ s ^ "\""
    | RuleId_ s => "RuleId_ \"" ^ s ^ "\""
    | TypeId_ s => "TypeId_ \"" ^ s ^ "\""
    | Id_ s     => "Id_ \"" ^ s ^ "\""
    | TypeVar_ s => "TypeVar_ \"'" ^ s ^ "\""


  fun test context s =
    let fun f s = scanToken context s
	fun F (s :int * (int * int) * substring) =
          case f s of
	    SOME (x, s') => x :: F s'
	  | NONE  => []
    in resetScanner ();
       F (0, (1, 1), Substring.full s)
    end

  fun testString context s =
    let val result = (setLoc (ASM_Location.Interactive s); test context s)
    in unsetLoc ();
       result
    end

  fun testFile context name =
    let open TextIO
	val f = (setLoc (ASM_Location.File name); openIn name)
	val s = inputAll f
        val result = test context s
    in unsetLoc (); closeIn f;
       result
    end

  fun scanString context parseFct s =
    let val _ = setLoc (ASM_Location.Interactive s)
	val _ = resetScanner ()
        val result = parseFct (scanToken context) (0, (1, 1), Substring.full s)
    in unsetLoc ();
       result
    end

  (* needed for parsing definitions, which change the context during parsing *)
  fun ctxDepScanString context parseFct s =
    let val _ = setLoc (ASM_Location.Interactive s)
	val _ = resetScanner ()
        val result = parseFct (fn context => scanToken context, context) (0, (1, 1), Substring.full s)
    in unsetLoc ();
       result
    end

  fun scanFile context parseFct name =
     let open TextIO
	val f = (setLoc (ASM_Location.File name); openIn name)
	val s = inputAll f
	val _ = resetScanner ()
        val result = parseFct (scanToken context) (0, (1, 1), Substring.full s)
    in unsetLoc (); closeIn f;
       result
    end

  fun ctxDepScanFile context parseFct name =
     let open TextIO
	val f = (setLoc (ASM_Location.File name); openIn name)
	val s = inputAll f
	val _ = resetScanner ()
        val result = parseFct (fn context => scanToken context, context) (0, (1, 1), Substring.full s)
    in unsetLoc (); closeIn f;
       result
    end

  fun catch f x =
    (f x) handle ex as Error cont => (Error.report cont; raise ex) | ex => raise ex

  fun tabulate [] = ()
    | tabulate ((pos, token) :: rest) =
      ( print ((ASM_Position.toString pos) ^ "   " ^ (showToken token) ^ "\n");
        tabulate rest )
end
