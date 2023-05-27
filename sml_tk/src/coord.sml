(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/coord.sml,v $
 
   Coordinates for sml_tk
  
   $Date: 2001/03/30 13:39:08 $
   $Revision: 3.0 $
   Author: stefan (Last modification by $Author: 2cxl $)
 
   (C) 1996, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)


structure Coord : COORD = 
struct

local open (*SmlTk*) BasicTypes BasicUtil in


exception COORD of string

fun show col = 
    let
	fun showInt i =
	    if ( i >= 0 ) then (Int.toString i)
	                  else ("-" ^ (Int.toString (i * ~1)))
	val sl = map (fn ((x,y):Coord) => (showInt x) ^ " " ^ (showInt y)) col
    in
	StringUtil.concatWith " " sl
    end

fun read str =
    let
	val _   = Debug.print 5 ("Coord.read: \""^str^"\"")
	val cos = StringUtil.words str
	fun dezip []        = []
	  | dezip (x::y::l) = 
	    let
		val x' = StringUtil.toInt x
		val y' = StringUtil.toInt y
	    in
		(x',y')::(dezip l)
	    end
	  | dezip _         = raise COORD "Coord.read: odd number of coordinates"
    in
	(dezip cos) handle Overflow => raise COORD "Coord.read: number conversion error"
    end

fun add (x1:int, y1:int) (x2, y2) = (x1 + x2, y1 + y2)

(* old: fun sub (x1, y1) (x2, y2) = (max(0,x1- x2),max(0, y1-y2)) *)

fun sub (x1:int, y1:int) (x2, y2) = (x1- x2, y1-y2)

(* scalar multiplication *)
fun smult (x1:int, y1:int) x      = (x*x1, y1*x)

(* rectangles *)
  type Rect = Coord * Coord
 
  fun between x (y:int) z = (x <= y) andalso (y<= z)
 
  fun inside  (u: int, v: int) ((x1: int, y1: int), (x2, y2)) = 
      (between x1 u x2) andalso (between y1 v y2)

  (* intersection of rectangles 
   * r1 intersects with r2 if any of the four corners of r2 is inside
   * r2, or the other way around.
   * Probably can be done shorter.
   *)
  fun intersect r1 r2 =
      let fun inter r1 ((x1, y1), (x2, y2)) =
	  List.exists (fn p=> inside p r1) 
	                              [(x1, y1), (x1, y2), (x2, y1), (x2, y2)]
      in  (inter r1 r2) orelse (inter r2 r1)
      end
      
 
  fun moveRect (p1, p2) p3 = (add p1 p3, add p2 p3)


  fun showRect ((x1:int,y1:int), (x2:int, y2:int)) = 
	"["^(Int.toString x1)^","^(Int.toString y1)^";"^
	    (Int.toString x2)^","^(Int.toString y2)^"] "


end 

end
