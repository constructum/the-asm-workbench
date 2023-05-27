(* ***********************************************************************

   Project: sml/Tk: an Tk Toolkit for sml
   Author: Stefan Westmeier, University of Bremen
  $Date: 2001/03/30 13:39:14 $
  $Revision: 3.0 $
   Purpose of this file: Mark Module

   *********************************************************************** *)

structure Mark : MARK = 
struct

local open BasicTypes BasicUtil in


exception MARK of string


fun show (Mark(n,m))     = (Int.toString n) ^"."^(Int.toString m)
   |show (MarkToEnd n)   = (Int.toString n) ^".end"
   |show MarkEnd         = "end";


fun showL ml = StringUtil.concatWith " " (map (fn (m1,m2) => (show m1)^" "^(show m2)) ml)



fun read m =
    let
	val (x,y)   = StringUtil.breakAtDot m
    in
	if ((size y) = 0) andalso ( x= "end") then
	    MarkEnd
	else if (not ((size x) = 0)) andalso (y = "end") then
	    MarkToEnd (StringUtil.toInt x)
	else
	    Mark(StringUtil.toInt x, StringUtil.toInt y)
    end

fun readL ml =
    let 
	val mls = StringUtil.words ml

	fun dezip []        = []
	  | dezip (x::y::l) = (x,y)::(dezip l)
	  | dezip _         = raise MARK "MARK.readL: odd number of marks"

    in
	dezip (map read mls)
    end

end 

end
