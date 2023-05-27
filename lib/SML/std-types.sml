(*
##
## "std-types.sml", G. Del Castillo, Jan 00-Feb 00
##
##
##
*)


structure Bool_ =
struct
  fun compare (x :bool, y :bool) = 
    if x = y then EQUAL
    else if x = false then LESS
    else GREATER

  val read  = Read.bool
  val write = Write.bool
end


structure Int_ =
struct
  val read  = Read.int
  val write = Write.int
end


structure Real_ =
struct
  val read  = Read.real
  val write = Write.real
end


structure String_ =
struct
  fun replace (str :string) (subs_list :string list) :string =
    let fun rrepl ([] :char list) = []
	|   rrepl (e::l) =
	      if e <> #"$"
	      then e :: (rrepl l)
	      else case l of
		      nil => [e]
		   | e::l => if e < #"0" orelse e > #"9"
			     then e :: (rrepl l)
			     else (explode (List.nth (subs_list, ord e - ord #"0" - 1))) @ (rrepl l)
    in implode (rrepl (explode str))
    end

  fun trim_front_or_end (str: string, n: int) = 
    let val templist = explode str
	val revlist  = rev templist
	fun F [] = []
	  | F (x::xs) = if (x = #"\n" orelse x = #"\t" orelse x = #" ") then F xs else x::xs
    in case n of
	0  => implode (rev (F revlist)) (* cut at end *)
      | 1  => implode (F templist)      (* cut at front *)
      | _  => trim_front_or_end (trim_front_or_end (str, 0), 1)    (* cut at both ends *)
    end

  fun trim_end s   = trim_front_or_end (s, 0)
  fun trim_front s = trim_front_or_end (s, 1)
  fun trim s       = trim_front_or_end (s, 2)

  (* !!! 'quote' to be replaced by appropriate call to lexical analyser ?  *)
  fun quote s =
    let fun Q [] = []
          | Q (#"\"" :: xs) = #"\\" :: #"\"" :: (Q xs)
          | Q (#"\n" :: xs) = #"\\" :: #"n" :: (Q xs)
          | Q (#"\t" :: xs) = #"\\" :: #"t" :: (Q xs)
          | Q (#"\\" :: xs) = #"\\" :: #"\\" :: (Q xs)
          | Q (x :: xs)    = x :: (Q xs)
    in implode (Q (explode s))
    end
      
  fun unquote s =
    let fun U [] = []
          | U (#"\\" :: #"\\" :: xs) = #"\\" :: (U xs)
          | U (#"\\" :: #"n" :: xs) = #"\n" :: (U xs)
          | U (#"\\" :: #"t" :: xs) = #"\t" :: (U xs)
          | U (#"\\" :: #"\"" :: xs) = #"\"" :: (U xs)
	  | U (x :: xs) = x :: (U xs)
    in implode (U (explode s))
    end
      (*
  val read  = Read.string
  val write = Write.string
*)
end


structure Option_ =
struct
  fun toList (SOME x) = [x]
    | toList NONE = []
(*
  val read  = Read.option
  val write = Write.option
*)
end


structure List_ =
struct
  open List Misc
	  
  fun foldll f b L = foldl (fn (a, b) => f (b, a)) b L

  fun indexOf (x0, L) =
    let fun F i (x0, []) = NONE
	  | F i (x0, x :: xs) = if x = x0 then SOME i else F (i+1) (x0, xs)
    in F 0 (x0, L)
    end
      
  fun compare _ ([], []) = EQUAL
    | compare _ (_, [])  = GREATER
    | compare _ ([], _)  = LESS
    | compare cmpfun (e1::l1, e2::l2) =
	case cmpfun (e1, e2) of
	  LESS    => LESS
	| GREATER => GREATER
	| EQUAL   => compare cmpfun (l1, l2)

  fun output sep L =
    let fun out [] = ""
	  | out [x] = x
	  | out (hd::tl) = hd ^ sep ^ out tl
    in out L
    end

  fun fixpoint (f, e) n =
    let val fpl = fixpoint (f, e)
    in if n <= 0 then [] else e :: (map f (fpl (n-1)))
    end
(*
  val read  = Read.list
  val write = Write.list
*)
  fun remove_at (L, i) = (List.take (L, i) @ List.drop (L, i + 1))
  fun insert_at (x, L, i) = (List.take (L, i) @ [ x ] @ List.drop (L, i))
  fun replace_at (x, L, i) = (List.take (L, i) @ [ x ] @ List.drop (L, i + 1))
end


