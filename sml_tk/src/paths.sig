(* ***********************************************************************

   Project: sml/Tk: an Tk Toolkit for sml
   Author: Stefan Westmeier, University of Bremen
  $Date: 2001/03/30 13:39:15 $
  $Revision: 3.0 $
   Purpose of this file: Functions related to Path-Management

   *********************************************************************** *)

signature PATHS =
    sig
	(* PathAss = (WidId * (WinId * WidPath )) *)
	(*                    {    IntPath     }  *)

	(* Selektoren *)
	val fstWidPath  : BasicTypes.WidPath -> 
                          (BasicTypes.WidId * BasicTypes.WidPath)
	val lastWidPath : BasicTypes.WidPath -> 
	                  (BasicTypes.WidPath * BasicTypes.WidId)

	(* Konstruktoren und Destruktoren *)
	val addWidget   : BasicTypes.WidId -> BasicTypes.WinId -> 
                          BasicTypes.WidPath -> BasicTypes.PathAss list -> 
                          BasicTypes.PathAss list

	val deleteWidget     : BasicTypes.WidId   -> BasicTypes.PathAss list -> 
                               BasicTypes.PathAss list
	val deleteWidgetPath : BasicTypes.IntPath -> BasicTypes.PathAss list -> 
	                       BasicTypes.PathAss list

	val deleteWindow : BasicTypes.WinId -> BasicTypes.PathAss list -> 
                           BasicTypes.PathAss list

	val newWidgetId : unit -> BasicTypes.WidId


	val getTclPathGUI            : BasicTypes.IntPath -> BasicTypes.TclPath
	val getIntPathGUI            : BasicTypes.WidId -> BasicTypes.IntPath
	val getWidPathGUI            : BasicTypes.WidId -> BasicTypes.WidPath
	val getIntPathFromTclPathGUI : BasicTypes.TclPath -> 
	                               (BasicTypes.WinId * BasicTypes.WidId)

	val occursWidgetGUI : BasicTypes.WidId -> bool
	val occursWindowGUI : BasicTypes.WinId -> bool

    end
