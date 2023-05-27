(*
##
## The ASM Workbench, 1996-2000
##
## Generic Pretty Printer Module
## Adapted from: L.C. Paulson, "ML for the working programmer", p.355
##
*)


signature PRETTY =
  sig
    type t
    val blo : int * t list -> t
    val str : string -> t
    val brk : int -> t
    val line_brk : t
    val pr  : TextIO.outstream -> int -> t -> unit
    val toString : int -> t -> string
  end


structure Pretty :PRETTY =
struct
  datatype t = Block of t list * int * int
	    | String of string
	    | Break of int
	    | Line_Break

  fun breakdist (Block(_,_,len)::es, after) = len + breakdist (es, after)
    | breakdist (String s   :: es, after)   = size s + breakdist (es, after)
    | breakdist (Break _    :: es, after)   = 0
    | breakdist (Line_Break :: es, after)   = 0
    | breakdist ([], after)                 = after

  fun pr os margin e =
  let 
      fun nspace n = 
	  case n of 
	    0    => ""
	  | n    => " " ^ nspace (n - 1)

      val space = ref margin

      fun blanks n  = (TextIO.output (os, (nspace n));
		       space := !space - n)

      fun newline () = (TextIO.output (os, "\n"); 
		       space := margin)      

      fun printing ([], _, _)                  = ()
	| printing (e::es, blockspace, after)  = 
	  (case e of 
		Block (bes, indent, len)   => 
		      printing (bes, !space-indent, breakdist(es, after))
	      | String s                   => 
		      (TextIO.output (os, s); 
		       space := !space - size s)
	      | Break len                  =>
		      if len + breakdist (es, after) <= !space
		      then blanks len
		      else (newline(); blanks(margin-blockspace))
	      | Line_Break                 =>
		      (newline(); blanks(margin-blockspace));
	   printing (es, blockspace, after))
   in 
     printing ([e], margin, 0)
   end


  fun toString margin e =
  let 
      val space  = ref margin
      val result = ref ([] :char list)

      fun nspace n = if n <= 0 then ()
	             else (result := #" " :: (!result); nspace (n - 1))

      fun output s =
        let fun F [] = ()
              | F (c :: cs) = (result := c :: (!result); F cs)
        in F (explode s)
        end

      fun blanks n   = (nspace n; space := !space - n)
      fun newline () = (result := #"\n" :: (!result); space := margin)

      fun printing ([], _, _)                  = ()
	| printing (e::es, blockspace, after)  = 
	  (case e of 
		Block (bes, indent, len) => 
		      printing (bes, !space-indent, breakdist(es, after))
	      | String s => 
		      (output s; 
		       space := !space - size s)
	      | Break len =>
		      if len + breakdist (es, after) <= !space
		      then blanks len
		      else (newline(); blanks(margin-blockspace))
	      | Line_Break                 =>
		      (newline(); blanks(margin-blockspace));
	   printing (es, blockspace, after))
   in printing ([e], margin, 0);
      implode (rev (!result))
   end



  fun length (Block(_, _, len)) = len
   | length (String s)         = size s
   | length (Break len)        = len
   | length (Line_Break)       = 0

  val str      = String
  and brk      = Break
  and line_brk = Line_Break

  fun blo (indent, es) =
   let fun sum ([],    k) = k
	 | sum (e::es, k) = sum (es, length e + k)
   in
     Block (es, indent, sum(es, 0))
   end
end
