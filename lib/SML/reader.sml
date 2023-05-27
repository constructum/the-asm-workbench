(* ************************************************************************** *)

signature Read_sig =
sig
  type POSITION = int * int * int

  type ('a, 'strm) reader = 'strm -> ('a * 'strm)
  (* similar to StringCvt.reader, but using an exception instead of NONE for failure *)

  exception ReadError of POSITION * string * string   (* pos, expected, found *)

  val errorMsg : exn -> string

  val bool : ('strm -> POSITION) ->
             (char, 'strm) StringCvt.reader -> (bool, 'strm) reader
                                                             
  val int  : ('strm -> POSITION) ->
             (char, 'strm) StringCvt.reader -> (int, 'strm) reader

  val real : ('strm -> POSITION) ->
             (char, 'strm) StringCvt.reader -> (real, 'strm) reader

  val string : ('strm -> POSITION) ->
               (char, 'strm) StringCvt.reader -> (string, 'strm) reader

  val empty : (char,'strm) StringCvt.reader -> (unit, 'strm) reader

  val ! : char -> 
          ('strm -> POSITION) ->
          (char, 'strm) StringCvt.reader -> (unit, 'strm) reader

  val $ : string -> 
          ('strm -> POSITION) ->
          (char, 'strm) StringCvt.reader -> (unit, 'strm) reader

  (*  infixr 7 -<  *)
  val -< : ( (('strm -> POSITION) -> (char, 'strm) StringCvt.reader -> (unit, 'strm) reader) * 
             (('strm -> POSITION) -> (char, 'strm) StringCvt.reader -> ('a, 'strm) reader) ) ->
           ('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a, 'strm) reader

  (*  infix 6 ++  *)
  val ++ : ( (('strm -> POSITION) -> (char, 'strm) StringCvt.reader -> ('a, 'strm) reader) * 
             (('strm -> POSITION) -> (char, 'strm) StringCvt.reader -> ('b, 'strm) reader) ) ->
           ('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a * 'b, 'strm) reader

  (*  infix 5 >-  *)
  val >- : ( (('strm -> POSITION) -> (char, 'strm) StringCvt.reader -> ('a, 'strm) reader) * 
             (('strm -> POSITION) -> (char, 'strm) StringCvt.reader -> (unit, 'strm) reader) ) ->
           ('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a, 'strm) reader

  (*  infix 4 or  *)
  val or : ( (('strm -> POSITION) -> (char, 'strm) StringCvt.reader -> ('a, 'strm) reader) * 
             (('strm -> POSITION) -> (char, 'strm) StringCvt.reader -> ('a, 'strm) reader) ) ->
           ('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a, 'strm) reader
                                                                               
  val single : (('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a,'strm) reader) ->
               ('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a, 'strm) reader
                       
  val pair : ( (('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a1,'strm) reader) *
               (('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a2,'strm) reader) ) ->
             ('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a1 * 'a2, 'strm) reader
                       
  val triple : ( (('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a1,'strm) reader) *
                 (('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a2,'strm) reader) *
                 (('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a3,'strm) reader) ) ->
               ('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a1 * 'a2 * 'a3, 'strm) reader
                       
  val tuple4 : ( (('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a1,'strm) reader) *
                 (('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a2,'strm) reader) *
                 (('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a3,'strm) reader) *
                 (('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a4,'strm) reader) ) ->
               ('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a1 * 'a2 * 'a3 * 'a4, 'strm) reader
                       
  val tuple5 : ( (('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a1,'strm) reader) *
                 (('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a2,'strm) reader) *
                 (('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a3,'strm) reader) *
                 (('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a4,'strm) reader) *
                 (('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a5,'strm) reader) ) ->
               ('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5, 'strm) reader
                       
  val list : (('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a,'strm) reader) ->
             ('strm -> POSITION) -> (char,'strm) StringCvt.reader -> ('a list, 'strm) reader
end

(* ************************************************************************** *)

structure Read :Read_sig  =
struct
  type POSITION = int * int * int

  type ('a, 'strm) reader = 'strm -> ('a * 'strm)

  exception ReadError of POSITION * string * string   (* pos, expected, found *)

  fun errorMsg (ReadError ((ptr, line, col), expected, found)) =
        let open Write
        in Output.toString
             ( tuple5_ ("at position $1 [line $2, column $3]: $4 expected" ^ (if found <> "" then ", $5 found" else "$5"))
                       (int, int, int, literal, literal) )
             (ptr, line, col, expected, found)
        end
    | errorMsg _ = raise Match

  val eos = "<end-of-stream>"

  (* ------------------------------------------------------------------------ *)

  fun bool (getPos :'strm -> POSITION) (basic_reader :(char, 'strm) StringCvt.reader) :(bool, 'strm) reader =
    fn istr => 
      let val istr' = StringCvt.skipWS basic_reader istr
          val pos   = getPos istr'
      in (valOf (Bool.scan basic_reader istr))
         handle Option => raise ReadError (pos, "bool constant (\"true\" or \"false\")", "")
      end
                                                       
  fun int (getPos :'strm -> POSITION) (basic_reader :(char, 'strm) StringCvt.reader) :(int, 'strm) reader =
    fn istr => 
      let val istr' = StringCvt.skipWS basic_reader istr
          val pos   = getPos istr'
      in (valOf (Int.scan StringCvt.DEC basic_reader istr))
        handle Option => raise ReadError (pos, "int constant", "")
      end
                                                       
  fun real (getPos :'strm -> POSITION) (basic_reader :(char, 'strm) StringCvt.reader) :(real, 'strm) reader =
    fn istr => 
      let val istr' = StringCvt.skipWS basic_reader istr
          val pos   = getPos istr'
      in (valOf (Real.scan basic_reader istr))
         handle Option => raise ReadError (pos, "real constant", "")
      end
                                                       
  fun ! (c0 :char) getPos basic_reader istr =    (* match character, do not skip over WS *)
    let val pos = getPos istr
    in case basic_reader istr of
          SOME (c, istr') =>
            if c = c0 then ((), istr')
            else raise ReadError (pos, "\""^(Char.toString c0)^"\"", "\""^(Char.toString c)^"\"")
        | NONE =>
            raise ReadError (pos, "\""^(Char.toString c0)^"\"", eos)
    end
        
  fun $ (s :string) getPos basic_reader istr =    (* match string, skip over WS first *)
    let val len   = size s
        val istr' = StringCvt.skipWS basic_reader istr
        val pos   = getPos istr'
        fun F (i, pastChars, istr) =
          if i = len
          then ((), istr)
          else case basic_reader istr of
                   SOME (c, istr') =>
                     if String.sub (s, i) = c
                     then F (i + 1, c :: pastChars, istr')
                     else raise ReadError (pos, "\""^s^"\"", "\""^(implode (rev (c :: pastChars)))^"\"")
                 | NONE =>
                     if pastChars <> []
                     then raise ReadError (pos, "\""^s^"\"", "\""^(implode (rev pastChars))^"\"")
                     else raise ReadError (pos, "\""^s^"\"", eos)
    in F (0, [], istr')
    end

  fun empty (basic_reader :(char, 'strm) StringCvt.reader) (istr :'strm) :(unit * 'strm) = ((), istr)

  infixr 7 -<
  infix  6 ++
  infix  5 >-

  fun op -< ( reader1 :('strm -> POSITION) -> (char, 'strm) StringCvt.reader -> (unit, 'strm) reader,
              reader2 :('strm -> POSITION) -> (char, 'strm) StringCvt.reader -> ('a, 'strm) reader )
            getPos (basic_reader :(char, 'strm) StringCvt.reader) istr =
    let val (_,  istr')  = reader1 getPos basic_reader istr
        val (x2, istr'') = reader2 getPos basic_reader istr'
    in (x2, istr'')                  (* throw away result of reader1 *)
    end
      
  fun op ++ ( reader1 :('strm -> POSITION) -> (char, 'strm) StringCvt.reader -> ('a, 'strm) reader,
              reader2 :('strm -> POSITION) -> (char, 'strm) StringCvt.reader -> ('b, 'strm) reader )
            getPos basic_reader istr :('a * 'b) * 'strm =
    let val (x1, istr')  = reader1 getPos basic_reader istr
        val (x2, istr'') = reader2 getPos basic_reader istr'
    in ((x1, x2), istr'')
    end

  fun op >- ( reader1 :('strm -> POSITION) -> (char, 'strm) StringCvt.reader -> ('a, 'strm) reader,
              reader2 :('strm -> POSITION) -> (char, 'strm) StringCvt.reader -> (unit, 'strm) reader )
            getPos basic_reader istr :'a * 'strm =
    let val (x1, istr')  = reader1 getPos basic_reader istr
        val (_,  istr'') = reader2 getPos basic_reader istr'
    in (x1, istr'')               (* throw away result of reader2 *)
    end

  infix 4 or
  fun op or ( reader1 :('strm -> POSITION) -> (char, 'strm) StringCvt.reader -> ('a, 'strm) reader,
              reader2 :('strm -> POSITION) -> (char, 'strm) StringCvt.reader -> ('a, 'strm) reader )
            getPos basic_reader istr :'a * 'strm =
    (reader1 getPos basic_reader istr)
    handle ReadError (pos, expected1, found) =>
           (reader2 getPos basic_reader istr)
           handle ReadError (pos, expected2, found) => raise ReadError (pos, expected1 ^ " or " ^ expected2, found)

  fun string (getPos :'strm -> POSITION) basic_reader istr =
  (* C-style string literals *)
    let val istr' = StringCvt.skipWS basic_reader istr
        val pos   = getPos istr'
        fun stringContent chars (getPos :'strm -> POSITION) basic_reader istr0 =
          let val stringContent = fn (chars, istr) => stringContent chars getPos basic_reader istr
              fun isOctal c =
                Char.isDigit c andalso c <> #"8" andalso c <> #"9"
              fun convert base octDigits = (* digits in the list are reversed *)
                let fun digitVal c =   (* precondition: c in [0-9A-Fa-f] *)
                      if Char.isDigit c then ord c - 48
                      else if Char.isUpper c then ord c - 55
                      else if Char.isLower c then ord c - 87
                      else 0
                    fun f [] = 0
                      | f (x :: xs) = digitVal x + base * f xs
                in f octDigits
                end
              fun escape_char_numeric (checkDigitFct, base, baseInLetters) (c0, (istr0, istr2)) =
                let fun F (octChars, istr) =
                      case basic_reader istr of
                          NONE => (octChars, istr)
                        | SOME (c, istr') => if checkDigitFct c then F (c :: octChars, istr') else (octChars, istr)
                    val (digitSeq, new_istr) = F ([c0], istr2)
                    val value = convert base digitSeq
                in if value < 256
                   then (chr value, new_istr)
                   else raise ReadError ( getPos istr0, baseInLetters ^ " escape character in range 0..255",
                                          implode ( if base = 16 then #"\\":: #"x"::(rev digitSeq)
                                                    else #"\\"::(rev digitSeq) ) )
                end
              val escape_char_octal = escape_char_numeric (isOctal, 8, "octal")
              val escape_char_hex = escape_char_numeric (Char.isHexDigit, 16, "hexadecimal")
              fun escape_char (istr0, istr1) =
                case basic_reader istr1 of
                   SOME (#"a", istr2) => SOME (#"\007", istr2)
                 | SOME (#"b", istr2) => SOME (#"\008", istr2)
                 | SOME (#"f", istr2) => SOME (#"\012", istr2)
                 | SOME (#"n", istr2) => SOME (#"\010", istr2)
                 | SOME (#"r", istr2) => SOME (#"\013", istr2)
                 | SOME (#"t", istr2) => SOME (#"\009", istr2)
                 | SOME (#"v", istr2) => SOME (#"\011", istr2)
                 | SOME (#"\\", istr2) => SOME (#"\092", istr2)
                 | SOME (#"'", istr2) => SOME (#"\039", istr2)
                 | SOME (#"\"", istr2) => SOME (#"\034", istr2)
                 | SOME (#"?", istr2) => SOME (#"\063", istr2)
                 | SOME (#"x", istr2) =>
                   ( case basic_reader istr2 of
                       SOME (c, istr3) =>     
                         if Char.isHexDigit c
                         then SOME (escape_char_hex (c, (istr0, istr3)))
                         else raise ReadError (getPos istr0, "\\x followed by hexadecimal digits", "\\x" ^ Char.toString c)
                     | NONE => raise ReadError (getPos istr0, "\\x followed by hexadecimal digits", "\\x followed by " ^ eos) )
                 | SOME (c, istr2) =>
                     if isOctal c 
                     then SOME (escape_char_octal (c, (istr0, istr2)))
                     else raise ReadError (getPos istr0, "valid escape character", "\\" ^ Char.toString c)
                 | NONE => NONE
          in case basic_reader istr0 of
                SOME (#"\"", istr1) => (implode (rev chars), istr0)
              | SOME (#"\\", istr1) =>
                ( case escape_char (istr0, istr1) of
                     SOME (c, istr2) => stringContent (c :: chars, istr2)
                   | NONE => raise ReadError (getPos istr1, "end of escape character in string", eos) )
              | SOME (#"'", istr1) => raise ReadError (getPos istr0, "valid string character", "'")
              | SOME (c, istr1) => stringContent (c :: chars, istr1)
              | NONE => raise ReadError (getPos istr0, "end of string", eos)
          end
    in ($ "\"" -< (stringContent []) >- $ "\"") getPos basic_reader istr'
    end
      
  fun applyToValue f reader getPos basic_reader istr =
    let val res = reader getPos basic_reader istr
    in (fn (value, istr') => (f value, istr')) res
    end

(*      
  fun chooseTags2 ((tag1 :string, reader1 :(char, 'strm) StringCvt.reader -> ('a, 'strm) StringCvt.reader, f1), (tag2 :string, reader2: (char, 'strm) StringCvt.reader -> ('b, 'strm) StringCvt.reader, f2)) (basic_reader :(char, 'strm) StringCvt.reader) (istr :'strm) =
    let val _ = StringCvt.skipWS basic_reader istr
    in case $ tag1 basic_reader istr of
          SOME (_, istr') => applyToValue f1 reader1 basic_reader (StringCvt.skipWS basic_reader istr')
        | NONE =>
          case $ tag2 basic_reader istr of
             SOME (_, istr') => applyToValue f2 reader2 basic_reader (StringCvt.skipWS basic_reader istr')
           | NONE => NONE
    end

  fun chooseTags3 ( (tag1 :string, reader1 :(char, 'strm) StringCvt.reader -> ('a, 'strm) StringCvt.reader, f1),
                    (tag2 :string, reader2: (char, 'strm) StringCvt.reader -> ('b, 'strm) StringCvt.reader, f2),
                    (tag3 :string, reader3: (char, 'strm) StringCvt.reader -> ('b, 'strm) StringCvt.reader, f3) )
                  (basic_reader :(char, 'strm) StringCvt.reader) (istr :'strm) =
    let val _ = StringCvt.skipWS basic_reader istr
    in case chooseTags2 ((tag1, reader2, f1), (tag2, reader2, f2)) basic_reader istr of
          SOME (result, istr') => result
        | NONE =>
          case $ tag3 basic_reader istr of
             SOME (_, istr') => applyToValue f3 reader3 basic_reader (StringCvt.skipWS basic_reader istr')
           | NONE => NONE
    end
      
(*
       else raise ReadError (NONE, Output.toString (Write.single_ "$1 expected" (Write.list_ "$$" "$1 or $2" Write.string)) [ tag1, tag2 ])
*)

  fun option value_reader basic_reader istr =
    chooseTags2 ( ( "SOME",  ($"(" -< value_reader) >- $")", fn value => SOME value),
                  ( "NONE", emptyReader,                     fn () => NONE) ) basic_reader istr
*)

  fun single reader = ( $"(" -< reader >- $")" )

  fun list (elem_reader :('strm -> POSITION) -> (char, 'strm) StringCvt.reader -> ('a, 'strm) reader)
           getPos (basic_reader :(char, 'strm) StringCvt.reader) istr0 =
    let fun read_further_elems (elems, istr) =
          let val (_, istr') = $"]" getPos basic_reader istr
          in (rev elems, istr') end
          handle ReadError (pos, expected1, found) =>
                 let val (_, istr') = ($"," getPos basic_reader istr)
                                         handle ReadError (pos, expected2, found) => raise ReadError (pos, expected1 ^ " or " ^ expected2, found)
                     val (elem, istr'') = elem_reader getPos basic_reader istr'
                 in (read_further_elems (elem :: elems, istr'')) end
        val (_, istr1) = $"[" getPos basic_reader istr0
    in let val (_, istr2) = $"]" getPos basic_reader istr1
       in ([], istr2) end
       handle ReadError (pos, expected1, found) =>
              let val (elem, istr2) = (elem_reader getPos basic_reader istr1)
                                      handle ReadError (pos, expected2, found) => raise ReadError (pos, expected1 ^ " or " ^ expected2, found)
              in read_further_elems ([elem], istr2) end
    end
                        
  fun pair (reader1, reader2) =
    ( $"(" -< reader1 ) ++ ( $"," -< reader2 >- $")" )

  fun triple (reader1, reader2, reader3) =
    applyToValue Misc.flatten3l ( $"(" -< reader1 ++ $"," -< reader2 ++ $"," -< reader3 >- $")" )
      
  fun tuple4 (reader1, reader2, reader3, reader4) =
    applyToValue Misc.flatten4l ( ( $"(" -< reader1 ) ++ ( $"," -< reader2 ) ++ ( $"," -< reader3 ) ++ ( $"," -< reader4 ) >- $")" )

  fun tuple5 (reader1, reader2, reader3, reader4, reader5) =
    applyToValue Misc.flatten5l ( ( $"(" -< reader1 ) ++ ( $"," -< reader2 ) ++ ( $"," -< reader3 ) ++ ( $"," -< reader4 ) ++ ( $"," -< reader5 ) >- $")" )

end (* struct *)

(* ************************************************************************** *)

structure StringInputStream =
struct
  type STREAM   = int * int * int
  type POSITION = int * int * int    (* in this case STREAM and POSITION are the same *)

  fun getPos (istr as (ptr :int, line :int, col :int)) = istr

  fun read reader (s :string) =
    let val n = size s
	fun getc (i, line, col) =
          if i < n
          then case String.sub(s, i) of
                  c as #"\n" => SOME (c, (i + 1, line + 1, 1))
                | c =>  SOME (c, (i + 1, line, col + 1))
          else NONE
    in (fn (res, _) => res) (reader getPos getc (0, 1, 1))
    end
end

(* ************************************************************************** *)

structure FileInputStream =     (* including for example TextIO.stdIn *)
struct
  type POSITION = int * int * int
  type STREAM   = TextIO.StreamIO.instream * POSITION

  fun getPos ((_, pos) :STREAM) :POSITION = pos

  fun newStream f = (TextIO.getInstream f, (0, 1, 1))

  fun getc (istr, (ptr, line, col)) =
          case TextIO.StreamIO.input1 istr of
             SOME (c as #"\n", istr') => SOME (c, (istr', (ptr + 1, line + 1, 1)))
           | SOME (c, istr') =>  SOME (c, (istr', (ptr + 1, line, col + 1)))
           | NONE => NONE

  fun read reader istr = reader getPos getc istr
                                
  (*
  Usage example:                                

  val f = TextIO.openIn "input";
  val istr = FileInputStream.newStream f;
  val (s1, istr') = FileInputStream.read Read.string istr;
  val (s2, istr'') = FileInputStream.read Read.string istr';
  val _ = TextIO.closeIn f
  *)
end

(* ************************************************************************** *)
  
structure Input =
struct
  fun fromString' reader s =
    StringInputStream.read reader s

  fun fromOpenFile' reader file =
    FileInputStream.read reader file

  fun fromFile' reader filename =
      let val file = TextIO.openIn filename
    in FileInputStream.read reader file;
       TextIO.closeIn file
    end

  fun catchReadError f reader istr =
    (f reader istr)
    handle (ex as Read.ReadError _) => (print (Read.errorMsg ex^"\n"); raise ex)

  fun fromString   reader s        = catchReadError fromString' reader s
  fun fromOpenFile reader file     = catchReadError fromOpenFile' reader file
  fun fromFile     reader filename = catchReadError fromFile' reader filename
end

(* ************************************************************************** *)
