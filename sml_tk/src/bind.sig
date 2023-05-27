(* ***********************************************************************

   Project: sml/Tk: an Tk Toolkit for sml
   Author: Stefan Westmeier, University of Bremen
  $Date: 2001/03/30 13:39:01 $
  $Revision: 3.0 $
   Purpose of this file: Functions related to "Tk-Bindings"

   *********************************************************************** *)

signature BIND =
    sig
	val selEvent  : BasicTypes.Binding -> BasicTypes.Event
	val selAction : BasicTypes.Binding -> BasicTypes.Action

	val getActionByName : string-> BasicTypes.Binding list -> 
                              BasicTypes.Action

	val noDblP    : BasicTypes.Binding list -> bool

	val add       : BasicTypes.Binding list -> BasicTypes.Binding list -> 
	                BasicTypes.Binding list
	val delete    : BasicTypes.Binding list -> BasicTypes.Binding list -> 
	                BasicTypes.Event list

        val packWindow   : BasicTypes.WinId -> BasicTypes.Binding list ->
                           string list

        val unpackWindow : BasicTypes.TclPath -> BasicTypes.Event list ->
                           string list

	val packWidget   : BasicTypes.TclPath -> BasicTypes.IntPath -> 
	                   BasicTypes.Binding list -> string list
	val packCanvas   : BasicTypes.TclPath -> BasicTypes.IntPath -> 
	                   BasicTypes.CItemId -> BasicTypes.Binding list -> 
			   string list
	val packTag      : BasicTypes.TclPath -> BasicTypes.IntPath -> 
	                   BasicTypes.AnnId   -> BasicTypes.Binding list -> 
			   string list

 	val unpackWidget : BasicTypes.TclPath -> BasicTypes.WidgetType -> 
	                   BasicTypes.Event list -> string list
(* 	val unpackCanvas : TclPath -> CItemId    -> Key list -> string  *)
    end
