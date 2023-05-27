(*
##
## "lexer.sml", G. Del Castillo, 1997-2000
##
## Last update: Jan 28, 2000
##
*)

signature LEXER =
sig
  datatype TOKEN =
    Int of int
    | Real of real
    | Str of string
    | Atom of string

  val ** : ('a -> 'b) * ('b -> 'c) -> 'a -> 'c
  val C  : ('a -> ('b * 'a) option) -> ('b -> 'a -> 'c option) -> 'a -> 'c option
  val success0 : 'a -> 'b -> ('a * 'b) option
  val success  : ('a -> 'b) -> 'a -> 'c -> ('b * 'c) option
  val failure  : 'a -> 'b -> 'c option

  val consume       : ('a -> (''b * 'c) option) -> ''b -> 'a -> (unit * 'c) option
  val consumeAtom   : ('a -> (TOKEN * 'b) option) -> string -> 'a -> (unit * 'b) option

  val scanInt	    : (char,'a) StringCvt.reader -> (int,'a) StringCvt.reader
  val scanString    : (char,'a) StringCvt.reader -> (string,'a) StringCvt.reader
  val scanIdent	    : (char,'a) StringCvt.reader -> (string,'a) StringCvt.reader

  val scanToken   : (char,'a) StringCvt.reader -> (TOKEN,'a) StringCvt.reader

  val R0 : 'a -> 'b -> 'c -> ('a * 'c) option
  val R1 : ('a -> 'b -> ('c * 'b) option)
           -> ('c -> 'd) -> 'a -> 'b -> ('d * 'b) option
  val R2 : ('a -> 'b -> ('c * 'b) option)
           -> ('a -> 'b -> ('d * 'b) option)
	   -> ('c * 'd -> 'e) -> 'a -> 'b -> ('e * 'b) option
  val R3 : ('a -> 'b -> ('c * 'b) option)
           -> ('a -> 'b -> ('d * 'b) option)
	   -> ('a -> 'b -> ('e * 'b) option)
	   -> ('c * 'd * 'e -> 'f) -> 'a -> 'b -> ('f * 'b) option
  val R4 : ('a -> 'b -> ('c * 'b) option)
           -> ('a -> 'b -> ('d * 'b) option)
	   -> ('a -> 'b -> ('e * 'b) option)
	   -> ('a -> 'b -> ('f * 'b) option)
	   -> ('c * 'd * 'e * 'f -> 'g)
	   -> 'a -> 'b -> ('g * 'b) option
  val R5 : ('a -> 'b -> ('c * 'b) option)
           -> ('a -> 'b -> ('d * 'b) option)
	   -> ('a -> 'b -> ('e * 'b) option)
	   -> ('a -> 'b -> ('f * 'b) option)
	   -> ('a -> 'b -> ('g * 'b) option)
	   -> ('c * 'd * 'e * 'f * 'g -> 'h)
	   -> 'a -> 'b -> ('h * 'b) option
  val R6 : ('a -> 'b -> ('c * 'b) option)
           -> ('a -> 'b -> ('d * 'b) option)
	   -> ('a -> 'b -> ('e * 'b) option)
	   -> ('a -> 'b -> ('f * 'b) option)
	   -> ('a -> 'b -> ('g * 'b) option)
	   -> ('a -> 'b -> ('h * 'b) option)
	   -> ('c * 'd * 'e * 'f * 'g * 'h -> 'i)
           -> 'a -> 'b -> ('i * 'b) option
  val R7 : ('a -> 'b -> ('c * 'b) option)
           -> ('a -> 'b -> ('d * 'b) option)
	   -> ('a -> 'b -> ('e * 'b) option)
	   -> ('a -> 'b -> ('f * 'b) option)
	   -> ('a -> 'b -> ('g * 'b) option)
	   -> ('a -> 'b -> ('h * 'b) option)
	   -> ('a -> 'b -> ('i * 'b) option)
	   -> ('c * 'd * 'e * 'f * 'g * 'h * 'i -> 'j)
	   -> 'a -> 'b -> ('j * 'b) option
  val R8 : ('a -> 'b -> ('c * 'b) option)
           -> ('a -> 'b -> ('d * 'b) option)
	   -> ('a -> 'b -> ('e * 'b) option)
	   -> ('a -> 'b -> ('f * 'b) option)
	   -> ('a -> 'b -> ('g * 'b) option)
	   -> ('a -> 'b -> ('h * 'b) option)
	   -> ('a -> 'b -> ('i * 'b) option)
	   -> ('a -> 'b -> ('j * 'b) option)
	   -> ('c * 'd * 'e * 'f * 'g * 'h * 'i * 'j -> 'k)
           -> 'a -> 'b -> ('k * 'b) option
  val R9 : ('a -> 'b -> ('c * 'b) option)
           -> ('a -> 'b -> ('d * 'b) option)
	   -> ('a -> 'b -> ('e * 'b) option)
	   -> ('a -> 'b -> ('f * 'b) option)
	   -> ('a -> 'b -> ('g * 'b) option)
	   -> ('a -> 'b -> ('h * 'b) option)
	   -> ('a -> 'b -> ('i * 'b) option)
	   -> ('a -> 'b -> ('j * 'b) option)
	   -> ('a -> 'b -> ('k * 'b) option)
	   -> ('c * 'd * 'e * 'f * 'g * 'h * 'i * 'j -> 'l)
	   -> 'a -> 'b -> ('l * 'b) option
end


structure Lexer :LEXER =
struct
  datatype TOKEN =
    Int of int
    | Real of real
    | Str of string
    | Atom of string

  infix **
  fun f1 ** f2 = f2 o f1

  fun C (reader1 :'a -> ('t1 * 'a) option) F =
    fn s =>
      case reader1 s of
	NONE => NONE
      | SOME (i1, s') => F i1 s'

  fun success0 x s = SOME (x, s)
  fun success f x s = SOME (f x, s)
  fun failure x s = NONE

  fun consume readFct x0 s =
    case readFct s of
      SOME (x, s') => if x = x0 then SOME ((),s') else NONE
    | NONE => NONE

  fun consumeAtom readToken atomName s =
    case readToken s of
      SOME (Atom str, s') => if str = atomName then SOME ((),s') else NONE
    | _ => NONE

  fun scanToken (readc :(char,'a) StringCvt.reader) (s :'a) =
    let val s = StringCvt.skipWS readc s
    in case readc s of
         SOME (#"[", s') => SOME (Atom (str #"["), s')
       | SOME (#"]", s') => SOME (Atom (str #"]"), s')
       | SOME (#"(", s') => SOME (Atom (str #"("), s')
       | SOME (#")", s') => SOME (Atom (str #")"), s')
       | SOME (#",", s') => SOME (Atom (str #","), s')
       | SOME (#"\"", s') => Option.map (fn (str, s') => (Str str, s')) (scanString readc s)
       | SOME (c, s') =>
         ( if Char.isDigit c
	   then case scanInt readc s of
		  SOME (i, s') => SOME (Int i, s')
		| NONE =>
		  ( case Real.scan readc s of
		    SOME (r, s') => SOME (Real r, s')
		  | NONE => NONE )
           else if Char.isAlpha c
	   then Option.map (fn (str, s') => (Atom str, s')) (scanIdent readc s)
	   else NONE )
       | NONE => NONE
    end

  and scanInt readc s =
    case Int.scan StringCvt.DEC readc s of
      SOME (i, s') =>
      ( case readc s' of
	 SOME (#".",s'') =>
         ( case readc s'' of
             SOME (#".", s''') => SOME (i, s')
           | _ => NONE )
       | SOME(#"E",_) => NONE
       | SOME(#"e",_) => NONE
       | _ => SOME (i, s') )
    | NONE => NONE

  and scanString readc =
    let (*
        fun scanStringCont s =
	  case readc s of
	    SOME (#"\"", s') => SOME ([], s)
          | SOME (#"\\", s') => C readc (fn c' =>
                                C scanStringCont (fn L' => fn s'' => SOME (#"\\" :: c' :: L', s''))) s'
          | SOME (c, s')  => C scanStringCont (fn L' => fn s' => SOME (c :: L', s')) s'
          | NONE          => NONE
        *)
        fun scanStringCont s =
	  case readc s of
	    SOME (#"\"", s') => SOME ([], s')
          | SOME (#"\\", s') =>
	    ( case readc s' of
	        SOME (c, s'') =>
		( case scanStringCont s'' of
		    SOME (L, s''') => SOME (#"\\" :: c :: L, s''')
                  | NONE => NONE )
              | NONE => NONE )
          | SOME (c, s')  =>
	    ( case scanStringCont s' of
		SOME (L, s'') => SOME (c :: L, s'')
	      | NONE => NONE )
          | NONE          => NONE
    in C (consume readc #"\"") (fn _ =>
       C (scanStringCont)      (fn L => fn s => SOME (Option.valOf (String.fromString (implode L)), s)))
       (*
       C (consume readc #"\"") (fn _ =>
       C (scanStringCont)      (fn L =>
       C (consume readc #"\"") (fn _ => fn s => SOME (Option.valOf (String.fromString (implode L)), s))))
       *)
    end

  and scanIdent readc s =
    SOME (StringCvt.splitl (fn c => Char.isAlphaNum c orelse c = #"'" orelse c = #"_") readc s)


  (* useful combinators to write parser, depending on the number of symbols in production *)

  fun R0 cons _ = success0 cons

  fun R1 f1 cons readTok =
    let val f1 = f1 readTok
    in C f1 (fn x1 => fn s => SOME (cons x1, s))
    end

  fun R2 f1 f2 cons readTok =
    let val (f1, f2) = (f1 readTok, f2 readTok)
    in C f1 (fn x1 => C f2 (fn x2 => fn s => SOME (cons (x1, x2), s)))
    end

  fun R3 f1 f2 f3 cons readTok =
    let val (f1, f2, f3) = (f1 readTok, f2 readTok, f3 readTok)
    in C f1 (fn x1 => C f2 (fn x2 =>
       C f3 (fn x3 => fn s => SOME (cons (x1, x2, x3), s))))
    end

  fun R4 f1 f2 f3 f4 cons readTok =
    let val (f1, f2, f3, f4) = (f1 readTok, f2 readTok, f3 readTok, f4 readTok)
    in C f1 (fn x1 => C f2 (fn x2 =>
       C f3 (fn x3 => C f4 (fn x4 => fn s => SOME (cons (x1, x2, x3, x4), s)))))
    end

  fun R5 f1 f2 f3 f4 f5 cons readTok =
    let val (f1, f2, f3, f4, f5) = (f1 readTok, f2 readTok, f3 readTok, f4 readTok, f5 readTok)
    in C f1 (fn x1 => C f2 (fn x2 =>
       C f3 (fn x3 => C f4 (fn x4 =>
       C f5 (fn x5 => fn s => SOME (cons (x1, x2, x3, x4, x5), s))))))
    end

  fun R6 f1 f2 f3 f4 f5 f6 cons readTok =
    let val (f1, f2, f3, f4, f5, f6) = (f1 readTok, f2 readTok, f3 readTok, f4 readTok, f5 readTok, f6 readTok)
    in C f1 (fn x1 => C f2 (fn x2 =>
       C f3 (fn x3 => C f4 (fn x4 =>
       C f5 (fn x5 => C f6 (fn x6 => fn s => SOME (cons (x1, x2, x3, x4, x5, x6), s)))))))
    end

  fun R7 f1 f2 f3 f4 f5 f6 f7 cons readTok =
    let val (f1, f2, f3, f4, f5, f6, f7) =
	  ( f1 readTok, f2 readTok, f3 readTok, f4 readTok, f5 readTok, f6 readTok, f7 readTok )
    in C f1 (fn x1 => C f2 (fn x2 =>
       C f3 (fn x3 => C f4 (fn x4 =>
       C f5 (fn x5 => C f6 (fn x6 => 
       C f7 (fn x7 => fn s => SOME (cons (x1, x2, x3, x4, x5, x6, x7), s))))))))
    end

  fun R8 f1 f2 f3 f4 f5 f6 f7 f8 cons readTok =
    let val (f1, f2, f3, f4, f5, f6, f7, f8) =
	  ( f1 readTok, f2 readTok, f3 readTok, f4 readTok, f5 readTok, f6 readTok, f7 readTok, f8 readTok )
    in C f1 (fn x1 => C f2 (fn x2 =>
       C f3 (fn x3 => C f4 (fn x4 =>
       C f5 (fn x5 => C f6 (fn x6 => 
       C f7 (fn x7 => C f8 (fn x8 => fn s => SOME (cons (x1, x2, x3, x4, x5, x6, x7, x8), s)))))))))
    end

  fun R9 f1 f2 f3 f4 f5 f6 f7 f8 f9 cons readTok =
    let val (f1, f2, f3, f4, f5, f6, f7, f8, f9) =
	  ( f1 readTok, f2 readTok, f3 readTok, f4 readTok, f5 readTok, f6 readTok, f7 readTok, f8 readTok, f9 readTok )
    in C f1 (fn x1 => C f2 (fn x2 =>
       C f3 (fn x3 => C f4 (fn x4 =>
       C f5 (fn x5 => C f6 (fn x6 => 
       C f7 (fn x7 => C f8 (fn x8 => 
       C f9 (fn x9 => fn s => SOME (cons (x1, x2, x3, x4, x5, x6, x7, x8), s))))))))))
    end
end

