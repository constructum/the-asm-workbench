(* ***********************************************************************

   Project: sml/Tk: an Tk Toolkit for sml
   Author: Stefan Westmeier, University of Bremen
  $Date: 2001/03/30 13:39:25 $
  $Revision: 3.0 $
   Purpose of this file: Abstract data Type Window

   *********************************************************************** *)

signature WINDOW =
    sig
	val check        : BasicTypes.Window -> bool

	val checkWinId   : BasicTypes.WinId  -> bool
	val checkTitle   : BasicTypes.Title  -> bool

(*
	val appendGUI    : Window -> unit
	val addGUI       : Window -> unit
	val deleteGUI    : WinId -> unit	
	val deleteAllGUI : unit
*)

	val changeTitle  : BasicTypes.WinId -> BasicTypes.Title -> unit
	val openW        : BasicTypes.Window -> unit
	val close        : BasicTypes.WinId -> unit

        val deleteGUI    : BasicTypes.WinId -> unit
        val deleteAllGUI : unit

	val selectBindKeyPath : BasicTypes.WinId -> string -> BasicTypes.Action

    end
