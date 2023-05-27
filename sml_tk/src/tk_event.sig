(* ***********************************************************************

   Project: sml/Tk: an Tk Toolkit for sml
   Author: Stefan Westmeier, University of Bremen
  $Date: 2001/03/30 13:39:20 $
  $Revision: 3.0 $
   Purpose of this file: Functions related to TkEvents

   *********************************************************************** *)

signature TK_EVENT =
    sig
	val selButton   : BasicTypes.TkEvent -> int
	val selState    : BasicTypes.TkEvent -> string
	val selXPos     : BasicTypes.TkEvent -> int
	val selYPos     : BasicTypes.TkEvent -> int
	val selXRootPos : BasicTypes.TkEvent -> int
	val selYRootPos : BasicTypes.TkEvent -> int

	val show    : unit   -> string
	val unparse : string -> BasicTypes.TkEvent
    end
