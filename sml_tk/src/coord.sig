(* ***********************************************************************

   Project: sml/Tk: an Tk Toolkit for sml
   Author: Stefan Westmeier, University of Bremen
  $Date: 2001/03/30 13:39:08 $
  $Revision: 3.0 $
   Purpose of this file: Coordinate Module

   *********************************************************************** *)



signature COORD =

	sig

	exception COORD of string

	val show : BasicTypes.Coord list -> string
	val read : string -> BasicTypes.Coord list

	val add : BasicTypes.Coord -> BasicTypes.Coord -> BasicTypes.Coord
	val sub : BasicTypes.Coord-> BasicTypes.Coord -> BasicTypes.Coord
	val smult : BasicTypes.Coord-> int-> BasicTypes.Coord

        type Rect

	val inside   : BasicTypes.Coord-> Rect-> bool
	val intersect : Rect-> Rect-> bool
	val moveRect : Rect-> BasicTypes.Coord-> Rect
        val showRect : Rect-> string

      end
