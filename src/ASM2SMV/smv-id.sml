signature SMV_ID_sig =
sig
  type VALUE = ASM_Value.VALUE
  type LOCATION = IL.LOCATION

  val value2id :VALUE -> string
  val id2value :string -> VALUE

  val location2id :LOCATION -> string
  val id2location :string -> LOCATION
end


structure SMV_ID :SMV_ID_sig =
struct
  open IL

  exception Float and String and Imported
  exception BadId of string

  (* !!!!!! check that no id's in the ASM spec contain '__' or end with '_' !!!!! *)

  local open ASM_Value
  in
    fun make (header :string, x_list :VALUE list) =
      ( header ^ "____" ^ (List_.output "__" (map value x_list)) ^ "____" )

    and make2 (header :string, x_y_list :(VALUE * VALUE) list) =
      let fun value_pair (x, y) = (value x) ^ "__" ^ (value y)
      in ( header ^ "____" ^ (List_.output "__" (map value_pair x_y_list)) ^ "____" )
      end

    and value (x :VALUE) :string =
      case x of
        UNDEF => "undef"
        | BOOL true  => "TRUE"
        | BOOL false => "FALSE"
        | INT i      => let val i = IntInf.toInt i
	                in if i >= 0 then Int.toString i else ("-" ^ (Int.toString (~i)))
			end
        | FLOAT _    => raise Float
        | STRING _   => raise String
        | TUPLE x_list      => make ("tuple", x_list)
        | CELL ("::", _)    => make ("list", ASM_Value.ASM2ML.list x)
        | CELL ("nil", _)   => make ("list", ASM_Value.ASM2ML.list x)
        | CELL (C, TUPLE [])      => ("_" ^ C)
        | CELL (C, TUPLE x_list)  => make ("_" ^ C, x_list)
        | CELL (C, x)       => make ("_" ^ C, [x])
        | x as FSET_E       => make ("set", ASM_Value.Set.listItems x)
        | x as FSET_T _     => make ("set", ASM_Value.Set.listItems x)
        | x as FMAP_E       => make2 ("map", ASM_Value.Map.listItemsi x)
        | x as FMAP_T _     => make2 ("map", ASM_Value.Map.listItemsi x)

    val value2id = value

    fun location2id (loc as (s, value)) =
      case value of
        TUPLE []      => "_" ^ s
      | TUPLE x_list  => make ("__" ^ s, x_list)      (* n-ary fct name, n > 1 *)
      | x             => make ("_" ^ s, [ x ])


    local
      val s_ref = ref ""
      val L	= ref ([] :string list)

      fun lookchar () = case (!L) of c :: _ => c | [] => ""
      fun getchar ()  = case (!L) of c :: L' => (L := L'; c) | [] => ""
      fun getchars n  = if n > 0 then (getchar (); getchars (n-1)) else ()
      fun is_digit c  = (c >= "0" andalso c <= "9")
      fun is_alpha c  = (c >= "a" andalso c <= "z") orelse (c >= "A" andalso c <= "Z")
      fun digit_val c = (ord c - ord #"0")

      fun accept s' =
        let val L_s' = map String.str (explode s')
        in if List.take (!L, size s') = L_s'
           then getchars (size s')
           else raise (BadId (!s_ref))
        end

      fun read_ident () =
        let val c = lookchar ()
        in if is_alpha c
           then let fun read_ident_rest () =
                      case (!L) of
                        "_" :: "_" :: _ => ""
                      | c :: _ => 
                         if (is_digit c) orelse (is_alpha c) orelse (c = "_")
                         then (getchar (); c ^ (read_ident_rest ()))
                         else raise (BadId (!s_ref))
                      | [] => ""
                in (getchar (); c ^ (read_ident_rest ()))
                end
           else raise (BadId (!s_ref)) 
        end

      fun read_int () =
	let fun read_int_seq () =
	      let val c = lookchar ()
	      in if is_digit c
		 then (getchar (); c ^ (read_int_seq ()))
		 else ""
	      end                          
	in valOf (StringCvt.scanString (Int.scan StringCvt.DEC) (read_int_seq ()))
	end

      fun F_list_with (G :unit -> 'a) () =
	let fun read_nonempty_list () =
	      let val x = G ()
	      in case (!L) of
		   "_" :: "_" :: "_" :: "_" :: _ => [x]
		 | "_" :: "_" :: _ => (accept "__"; x :: (read_nonempty_list ()))
		 | _ => raise (BadId (!s_ref))
	      end
	    fun read_list () =
	      if lookchar () = "_"
	      then []
	      else read_nonempty_list ()
	in (fn (_, x_list, _) => x_list) (accept "____", read_list (), accept "____")
	end

      fun nullary_function f =
        ASM_Type.isUnitType (ASM_Type.domain (valOf (ASM_Signature.typeOf (ASM_Signature.find (!ASM_Top.sign) f))))
    in
      fun id2value' (s :string) :VALUE =
        let fun F_pair () =
              (fn (x1, _, x2) => (x1, x2)) (F (), accept "__", F ())
            and F_list () = F_list_with F ()
            and F_pair_list () = F_list_with F_pair ()
            and F () =
              case (!L) of
                "T" :: "R" :: "U" :: "E" :: _ => (accept "TRUE"; BOOL true)
              | "F" :: "A" :: "L" :: "S" :: "E" :: _ => (accept "FALSE"; BOOL false)
              | "u" :: "n" :: "d" :: "e" :: "f" :: _ => (accept "undef"; UNDEF)
              | "t" :: "u" :: "p" :: "l" :: "e" :: _ => (accept "tuple"; TUPLE (F_list ()))
              | "l" :: "i" :: "s" :: "t" :: _ => (accept "list"; ASM_Value.ML2ASM.list (F_list ()))
              | "s" :: "e" :: "t" :: _ => (accept "set"; ASM_Value.Set.fromList (F_list ()))
              | "m" :: "a" :: "p" :: _ => (accept "map"; ASM_Value.Map.fromList (F_pair_list ()))
              | "_" :: _ =>
              ( ( let val constructor = (getchar (); read_ident ())
                  in if nullary_function constructor
                     then CELL (constructor, TUPLE [])
                     else CELL (constructor, case F_list () of [x] => x | xs => TUPLE xs)
                  end ) handle _ => raise (BadId s) )
              | "-" :: c :: _ =>
                  if is_digit c
                  then (accept "-"; INT (IntInf.fromInt (~(read_int ()))))
                  else raise (BadId s)
              | c :: _ =>
                  if is_digit c
                  then INT (IntInf.fromInt (read_int ()))
                  else raise (BadId s)
              | _ => raise (BadId s)
        in F ()
        end

      fun id2value s =
        let val _ = (s_ref := s; L := map String.str (explode s))
	in id2value' s
	end

      fun id2location' (s :string) :LOCATION = 
        let fun F_list () = F_list_with (fn () => id2value' s) ()

	    and F () =
              case (!L) of
	        "_" :: "_" :: _ =>
		( ( let val function_name = (getchar (); getchar (); read_ident ())
		    in (function_name, TUPLE (F_list ()))
		    end ) handle _ => raise (BadId s) )
	      | "_" :: _ =>
		( ( let val function_name = (getchar (); read_ident ())
		    in if nullary_function function_name
		       then (function_name, TUPLE [])
		       else (function_name, hd (F_list ()))
		    end ) handle _ => raise (BadId s) )
              | _ => raise (BadId s)
        in F ()
	end 

     fun id2location s =
       let val _ = (s_ref := s; L := map String.str (explode s))
       in id2location' s
       end
    end
  end      
end
