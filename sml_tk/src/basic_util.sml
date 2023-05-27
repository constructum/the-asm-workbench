(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/basic_util.sml,v $

   Some utility functions needed for sml_tk. 

   Originally, this was based on the gofer prelude, but most of the
   functions there are in the new standard basis library.
 
   $Date: 2001/03/30 13:39:01 $
   $Revision: 3.0 $
   Author: bu/cxl (Last modification by $Author: 2cxl $)

   (C) 1998, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)


structure BasicUtil : BASIC_UTIL = 

struct
    

(******************************************************************************
 *
 * Part 1: General functions.
 *
 * Mainly these are fst and snd (why are they not in the basis anyway?), and
 * some functionals to schoenfinkel and unschoenfinkel functions, and twiddle
 * their arguments.
 *)

    fun fst (a, _)    = a
    fun snd (_, b)    = b
    fun pair (f, g) z = (f z, g z) 

    fun a /= b = not(a = b)
    fun eq a b = a = b

    fun upto(i, j) = if (i:int) <= j then i::(upto(i+1, j)) else []

    fun inc x = (x := (!x + 1); !x)

    fun curry   f x y = f (x, y)
    fun uncurry f (x, y) = f x y 
    fun twist  (f:'a*'b-> 'c) = fn (y, x)=> f(x, y)

    val K0 = fn _ => ()

(******************************************************************************
 *
 * Part 2: List utility functions
 *
 * Most of these are needed because of the Gopheresque programming style 
 * in parts of sml_tk. 
 *)
       
    structure ListUtil =

    struct
	fun getx p [] ex      = raise ex
	  | getx p (x::xs) ex = if p x then x else getx p xs ex
	    
	fun updateVal p y = map (fn x=> if p x then y else x)
	    	   
	    
	fun dropWhile p []              = []
	  | dropWhile p (xs as(x::xs2)) = if p x then dropWhile p xs2
					  else xs

	(* Note this is not the same as List.partition, which runs through
	 * the whole of the list-- span stops as soon as p x is false.
	 *)
	fun span p []      = ([], [])
	  | span p (x::xs) = if p x then let val (ys, zs)= span p xs
	                                 in  (x::ys, zs) 
				         end
			     else ([], x::xs)

	fun break p = span (not o p)
	    
	fun sort (less: 'a*'a -> bool) =
	    let fun insert (x, []) = [x]
		  | insert (x, y::ys) =
		    if less(y, x) then y :: insert (x, ys) else x::y::ys
		fun sort1 [] = []
		  | sort1 (x::xs) = insert (x, sort1 xs)
	    in  sort1  end

	fun prefix [] ys = true
	  | prefix (x::xs) [] = false
	  | prefix (x::xs) (y::ys) = (x=y andalso prefix xs ys)

	fun concatWith s []     = []
	  | concatWith s [t]    = t
	  | concatWith s (t::l) = t@s@(concatWith s l)
	    
	
    end

(******************************************************************************
 *
 * Part 3: String utility functions.
 *
 * The "isBollocks" functions are needed here because SML/NJ 0.93 doesn't like
 * the literal character syntax. The other ones are here because they're dead
 * handy.
 *)
         
    structure StringUtil =

    struct

	fun isDot        c = #"." = c
	fun isComma      c = #"," = c
	fun isLinefeed   c = #"\n" = c
	fun isOpenParen  c = #"(" = c
	fun isCloseParen c = #")" = c
	
	fun concatWith s []     = ""
	  | concatWith s [t]    = t
	  | concatWith s (t::l) = t^s^(concatWith s l)
		       
       val words    = String.tokens Char.isSpace

       (* a utility function which splits up a string at the first dot
	* from the left, returning the two substrings dropping the dot-- e.g.
	*   breakAtDot("12.345 bollocks) = ("12", "345 bollocks")
	* (Needed quite often because dots are a bit special in Tcl.) 
	*)
       local 
	   open Substring 
       in
	   fun breakAtDot s = 
	       let val (hd, tl) = splitl (not o isDot) (full s)
	       in  (string hd, string (triml 1 tl))
	       end
       end

       (* convert string to int, but return 0 if conversion fails *)
       fun toInt s = 
	   Option.getOpt(Int.fromString s, 0)
	   handle Overflow => 
	       ((*TextIO.output(TextIO.stdErr,
			      "WARNING: caught int conversion overflow\n");*)
		0)
       (* convert int to string as readable by Tcl-- need - instead of ~ *)
       fun fromInt s = 
	   if s < 0 then ("-"^(Int.toString (Int.abs s))) 
	   else Int.toString s	  

       fun all p str = Substring.foldl (fn (c, r)=> (p c) andalso r)
	                                true (Substring.full str) 

       (* Adaptstring converts double quotes and other special characters 
	* into properly escaped sequences, to insure the string is to
	* Tcl's liking.
	*)
       fun adaptString s = 
	   let fun escape c = 
		   if Char.contains "\"\\$[]{}" c
		       then "\\" ^(str c)
		   else if c = #"\n" then "\\n" 
			else str c
	   in String.translate escape s
	   end

    end
       
(******************************************************************************
 *
 * Part 4: File utility functions.
 *
 * Now that the basis library offers a standardized interface to the OS and
 * the file system, we can put these here.
 *)
         

    structure FileUtil = 

    struct

	val execute = Unix.streamsOf o Unix.execute
	
	val exec    = SysDep.exec

        local
            open Posix
        in
            fun whoAmI () =
                 ProcEnv.getlogin()
                 (* this doesn't seem to work all the time, e.g. if running
                  * inside an emacs *)
                 handle OS.SysErr _ =>
                    (* do it the hard way :-} *)
                    SysDB.Passwd.name (SysDB.getpwuid (ProcEnv.getuid()))
                    handle OS.SysErr _ => "???"
        end


	fun whatTimeIsIt() =
	    let val dt= Date.fromTimeLocal (Time.now ())
	    in  (Date.toString dt)^(Date.fmt " %Z" dt)
	    end handle OS.SysErr _ => ""
		

    end
	 
end








