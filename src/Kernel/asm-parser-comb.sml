structure ASM_ParserCombinators =
struct
  open ASM_Lexer ASM_AST
  exception FAIL of POSITION
  infix until until'

  local 
    fun C (savedPos :POSITION option) read F =
      fn (s as (numTok :int, currPos, sstr)) =>
        let val firstPos = case savedPos of NONE => currPos | SOME p => fst p
        in case read s of
	     NONE => raise (FAIL (Pos ({ first = firstPos, last = currPos })))
	   | SOME (i1, s') => F i1 s'
        end
  in
    val C' = C

    fun try_else (savedPos :POSITION option) read F_success F_fail =
      fn (s as (_, currPos, _)) =>
	let val firstPos = case savedPos of NONE => currPos | SOME p => fst p
	in ( case read s of
  	       NONE => F_fail (Pos ({ first = firstPos, last = currPos }), s)
	     | SOME (i1, s') => F_success i1 s' )
           handle FAIL _ => F_fail (Pos ({ first = firstPos, last = currPos }), s)
	end

    fun !! tok0 readFct (s as (_, firstPos, _)) =
      case readFct s of
	SOME ((pos :POSITION, tok :TOKEN), s' as (_, lastPos, _)) =>
          if eqToken (tok, tok0)
	  then SOME ((pos, ()), s')
	  else raise (FAIL (Pos ({ first = fst pos, last = lastPos })))
      | NONE => raise (FAIL (Pos ({ first = firstPos, last = firstPos })))
    fun $ s = !! (Keyw_ s)

    fun R1 f1 cons readTok (s0 as (_, pos0, _) :int * (int * int) * substring)=
      let val f1 = f1 readTok
      in ( C NONE f1 (fn (pos, x1) => fn s => SOME (cons pos x1, s)) s0 )
      end
    fun R2 f1 f2 cons readTok =
      let val (f1, f2) = (f1 readTok, f2 readTok)
      in C NONE f1 (fn (pos1, x1) =>
         C (SOME pos1) f2 (fn (pos2, x2) => fn s => SOME (cons (pos1 until pos2) (x1, x2), s)))
      end
    fun R3 f1 f2 f3 cons readTok =
      let val (f1, f2, f3) = (f1 readTok, f2 readTok, f3 readTok)
      in C NONE f1 (fn (pos1, x1) => C (SOME pos1) f2 (fn (pos2, x2) =>
	 C (SOME pos2) f3 (fn (pos3, x3) => fn s => SOME (cons (pos1 until pos3) (x1, x2, x3), s))))
      end
    fun R4 f1 f2 f3 f4 cons readTok =
      let val (f1, f2, f3, f4) = (f1 readTok, f2 readTok, f3 readTok, f4 readTok)
      in C NONE f1 (fn (pos1, x1) => C (SOME pos1) f2 (fn (pos2, x2) =>
	 C (SOME pos2) f3 (fn (pos3, x3) =>
	 C (SOME pos3) f4 (fn (pos4, x4) => fn s => SOME (cons (pos1 until pos4) (x1, x2, x3, x4), s)))))
      end
    fun R5 f1 f2 f3 f4 f5 cons readTok =
      let val (f1, f2, f3, f4, f5) = (f1 readTok, f2 readTok, f3 readTok, f4 readTok, f5 readTok)
      in C NONE f1 (fn (pos1, x1) => C (SOME pos1) f2 (fn (pos2, x2) =>
	 C (SOME pos2) f3 (fn (pos3, x3) => C (SOME pos3) f4 (fn (pos4, x4) =>
	 C (SOME pos4) f5 (fn (pos5, x5) => fn s => SOME (cons (pos1 until pos5) (x1, x2, x3, x4, x5), s))))))
      end
    fun R6 f1 f2 f3 f4 f5 f6 cons readTok =
      let val (f1, f2, f3, f4, f5, f6) = (f1 readTok, f2 readTok, f3 readTok, f4 readTok, f5 readTok, f6 readTok)
      in C NONE f1 (fn (pos1, x1) => C (SOME pos1) f2 (fn (pos2, x2) =>
	 C (SOME pos2) f3 (fn (pos3, x3) => C (SOME pos3) f4 (fn (pos4, x4) =>
	 C (SOME pos4) f5 (fn (pos5, x5) => 
	 C (SOME pos5) f6 (fn (pos6, x6) => fn s => SOME (cons (pos1 until pos6) (x1, x2, x3, x4, x5, x6), s)))))))
      end
    fun R7 f1 f2 f3 f4 f5 f6 f7 cons readTok =
      let val (f1, f2, f3, f4, f5, f6, f7) =
	    ( f1 readTok, f2 readTok, f3 readTok, f4 readTok, f5 readTok, f6 readTok, f7 readTok )
      in C NONE f1 (fn (pos1, x1) => C (SOME pos1) f2 (fn (pos2, x2) =>
	 C (SOME pos2) f3 (fn (pos3, x3) => C (SOME pos3) f4 (fn (pos4, x4) =>
	 C (SOME pos4) f5 (fn (pos5, x5) => C (SOME pos5) f6 (fn (pos6, x6) => 
	 C (SOME pos6) f7 (fn (pos7, x7) => fn s => SOME (cons (pos1 until pos7) (x1,x2,x3,x4,x5,x6,x7), s))))))))
      end
    fun R8 f1 f2 f3 f4 f5 f6 f7 f8 cons readTok =
      let val (f1, f2, f3, f4, f5, f6, f7, f8) =
	    ( f1 readTok, f2 readTok, f3 readTok, f4 readTok, f5 readTok, f6 readTok, f7 readTok, f8 readTok )
      in C NONE f1 (fn (pos1, x1) => C (SOME pos1) f2 (fn (pos2, x2) =>
	 C (SOME pos2) f3 (fn (pos3, x3) => C (SOME pos3) f4 (fn (pos4, x4) =>
	 C (SOME pos4) f5 (fn (pos5, x5) => C (SOME pos5) f6 (fn (pos6, x6) => 
	 C (SOME pos6) f7 (fn (pos7, x7) => 
	 C (SOME pos7) f8 (fn (pos8, x8) => fn s => SOME (cons (pos1 until pos8) (x1,x2,x3,x4,x5,x6,x7,x8), s)))))))))
      end
    fun R9 f1 f2 f3 f4 f5 f6 f7 f8 f9 cons readTok =
      let val (f1, f2, f3, f4, f5, f6, f7, f8, f9) =
	    ( f1 readTok, f2 readTok, f3 readTok, f4 readTok, f5 readTok, f6 readTok, f7 readTok, f8 readTok, f9 readTok )
      in C NONE f1 (fn (pos1, x1) => C (SOME pos1) f2 (fn (pos2, x2) =>
	 C (SOME pos2) f3 (fn (pos3, x3) => C (SOME pos3) f4 (fn (pos4, x4) =>
	 C (SOME pos4) f5 (fn (pos5, x5) => C (SOME pos5) f6 (fn (pos6, x6) => 
	 C (SOME pos6) f7 (fn (pos7, x7) => C (SOME pos7) f8 (fn (pos8, x8) =>
	 C (SOME pos8) f9 (fn (pos9, x9) => fn s =>
	      SOME (cons (pos1 until pos9) (x1,x2,x3,x4,x5,x6,x7,x8,x9), s))))))))))
      end

    fun sepSeq (separatorPred :TOKEN -> bool)
          (elem :(POSITION * TOKEN, 'b) tracking_reader -> (POSITION * 'c, 'b) tracking_reader)
	  (tokenizer :(POSITION * TOKEN, 'b) tracking_reader) :(POSITION * 'c list, 'b) tracking_reader =
      fn (s0 as (_, currPos, sstr)) =>
	let (*val firstPos = case savedPos of NONE => currPos | SOME p => fst p *)
	    val firstPos = currPos
	in case (elem tokenizer s0) of
	     SOME ((pos1 as Pos p1, e1), s1) =>
	       let val result1 = SOME ((Pos p1, [e1]), s1)
	       in ( case (tokenizer s1) of
		      SOME ((_, tok), s2) =>
		      ( if (separatorPred tok)
			then ( case ((sepSeq separatorPred elem) tokenizer s2) of
				 SOME ((pos3 as Pos p3, es), s3) => SOME ((pos1 until pos3, e1 :: es), s3)
			       | _ => result1 )
			else result1 )
		    | _ => result1 ) handle FAIL _ => result1
	       end
	   | _ => raise (FAIL (Pos ({ first = firstPos, last = currPos })))
	end

    (* --- the following is too inefficient:
       fun seq elem tokenizer =
	  (  R2   elem (seq elem)     (fn pos => fn (e,es) => (pos, e :: es))
	  || R1   elem		     (fn pos => fn e => (pos, [e]))  )  tokenizer
       ----------------------------------------------------------------------------------- *)

    fun commaSeq elem tokenizer = sepSeq (fn Keyw_ "," => true | _ => false) elem tokenizer
    fun semicolonSeq elem tokenizer = sepSeq (fn Keyw_ ";" => true | _ => false) elem tokenizer

    fun seq (elem :('a, 'b) tracking_reader -> (POSITION * 'c, 'b) tracking_reader)
	    (tokenizer :('a, 'b) tracking_reader)  :(POSITION * 'c list, 'b) tracking_reader =
      fn (s as (_, currPos, sstr)) =>
	let (*val firstPos = case savedPos of NONE => currPos | SOME p => fst p *)
	    val firstPos = currPos
	in case (elem tokenizer s) of
	     NONE => raise (FAIL (Pos ({ first = firstPos, last = currPos })))
	   | SOME ((pos1 as Pos p1, e1), s') =>
	     ( ( case ((seq elem) tokenizer s') of
		   SOME ((pos2 as Pos p2, es), s'') => SOME ((pos1 until pos2, e1 :: es), s'')
		 | NONE => SOME ((Pos p1, [e1]), s') ) handle FAIL _ => SOME ((Pos p1, [e1]), s') )
	end

    infixr ||
    fun ( (parseFct1 :('src, 'stream) tracking_reader -> ('dest, 'stream) tracking_reader)
	|| (parseFct2 :('src, 'stream) tracking_reader -> ('dest, 'stream) tracking_reader) ) tokenizer s =
      let val result1 = (parseFct1 tokenizer s) handle FAIL _ => parseFct2 tokenizer s
      in case result1 of
	   SOME _ => result1
	 | NONE   => parseFct2 tokenizer s
      end
  end

(*
  fun selectThis tokenizer continue (s :int * (int * int) * substring) =
    case tokenizer s of
      SOME ((pos as _, token :TOKEN), s') => continue pos token tokenizer s
    | _ => NONE
*)

  fun selectThis tokenizer continue (s :int * (int * int) * substring) =
    case tokenizer s of
      SOME ((pos as _, x), s') => continue pos x tokenizer s
    | _ => NONE

  fun selectNext tokenizer continue s =
    case tokenizer s of
      SOME ((pos as _, token :TOKEN), s') => continue pos token tokenizer s'
    | _ => NONE

  fun chooseAndProceed tokenizer continue terminate s =
    case tokenizer s of
      SOME ((pos, token :TOKEN), s') => continue pos token s'
    | NONE => terminate s

  fun beforePos (Pos { first = (l1,c1),... }) = Pos {first = (l1, c1-1), last = (l1,c1-1)}

  fun optSeq seq tokenizer =
    selectThis tokenizer ( fn pos0 => fn _ =>
      (  R1 seq              (fn pos => fn es => (pos, es))
      || R0 (* empty *)      (beforePos pos0, [])  ) )

  fun opt elem tokenizer =
    selectThis tokenizer ( fn pos0 => fn _ =>
      (  R1 elem             (fn pos => fn e => (pos, SOME e))
      || R0 (* empty *)      (beforePos pos0, NONE)  ) )

  fun opt' elem tokenizer =
    selectThis tokenizer ( fn pos0 => fn _ =>
      (  R1 elem             (fn pos => fn e => (pos, [e]))
      || R0 (* empty *)      (beforePos pos0, [])  ) )

  fun commaSeq0 elem tokenizer = optSeq (commaSeq elem) tokenizer 
  fun seq0 elem tokenizer      = optSeq (seq elem) tokenizer 
end


