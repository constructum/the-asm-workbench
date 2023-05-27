(* ***********************************************************************

   Project: sml/Tk: an Tk Toolkit for sml
   Author: Stefan Westmeier, University of Bremen
  $Date: 2001/03/30 13:39:03 $
  $Revision: 3.0 $
   Purpose of this file: Functions related to Canvas Items in Widget Tree

   *********************************************************************** *)

signature C_ITEM_TREE =
    sig
	exception CITEM_TREE of string

	val get : BasicTypes.WidId -> BasicTypes.CItemId -> 
	          BasicTypes.CItem
	val upd : BasicTypes.WidId -> BasicTypes.CItemId -> 
	          BasicTypes.CItem -> unit

	(*           inWid               toAdd                                           *)
	val add    : BasicTypes.WidId -> BasicTypes.CItem -> unit
	(*           inWid               toAdd               after                       *)
(*	val insert : BasicTypes.WidId -> BasicTypes.CItem -> BasicTypes.CItemId -> unit  *)
	(*           inWid               toDel                                           *)
	val delete : BasicTypes.WidId -> BasicTypes.CItemId  -> unit


	val getConfigure  : BasicTypes.WidId -> BasicTypes.CItemId -> 
	                    BasicTypes.Configure list
(*	val setConfigure  : WidId -> CItemId -> Configure list -> unit      *)
	val addConfigure  : BasicTypes.WidId -> BasicTypes.CItemId -> 
	                    BasicTypes.Configure list -> unit

	val printCanvas   : BasicTypes.CItemId -> BasicTypes.Configure list ->
                            unit

	val getBinding    : BasicTypes.WidId -> BasicTypes.CItemId -> 
	                    BasicTypes.Binding list
(*	val setBinding    : WidId -> CItemId -> Binding list -> unit        *)
	val addBinding    : BasicTypes.WidId -> BasicTypes.CItemId -> 
	                    BasicTypes.Binding list -> unit

(*	val getTags : WidId -> CItemId -> CItemId list                      *)


	val getCoords  : BasicTypes.WidId -> BasicTypes.CItemId -> 
	                 BasicTypes.Coord list 
	val setCoords  : BasicTypes.WidId -> BasicTypes.CItemId -> 
	                 BasicTypes.Coord list -> unit

	val getWidth   : BasicTypes.WidId -> BasicTypes.CItemId -> int
	val getHeight  : BasicTypes.WidId -> BasicTypes.CItemId -> int

	val getIconWidth  : BasicTypes.IconKind -> int
	val getIconHeight : BasicTypes.IconKind -> int

	(*                                                   distance      *)
	val move : BasicTypes.WidId -> BasicTypes.CItemId -> BasicTypes.Coord -> unit

    end
