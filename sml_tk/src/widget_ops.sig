(* ***********************************************************************

   Project: sml/Tk: an Tk Toolkit for sml
   Author: Stefan Westmeier, University of Bremen
   Date: $Date: 2001/03/30 13:39:22 $
   Revision: $Revision: 3.0 $
   Purpose of this file: Operations on Widgets Contents

   *********************************************************************** *)

signature WIDGET_OPS =
    sig
	val readTextWidState    : BasicTypes.WidId -> bool
	val setTextWidReadOnly  : BasicTypes.WidId -> bool -> unit

	val clearAnnoText       : BasicTypes.WidId -> unit
	val replaceAnnoText     : BasicTypes.WidId -> 
	                          BasicTypes.AnnoText -> unit 
	val deleteAnnoText      : BasicTypes.WidId -> 
	                          BasicTypes.Mark* BasicTypes.Mark -> unit
	val insertAnnoText      : BasicTypes.WidId -> 
	                          BasicTypes.AnnoText -> BasicTypes.Mark -> 
				                                        unit
	val insertAnnoTextEnd   : BasicTypes.WidId -> 
	                          BasicTypes.AnnoText -> unit 
	                          (* use discouraged-- very inefficient! *)


	val selectText          : BasicTypes.WidId -> 
	                          BasicTypes.Mark * BasicTypes.Mark -> 
				  string
	val selectTextAll       : BasicTypes.WidId -> string

	val selectSelRange      : BasicTypes.WidId -> 
	                          (BasicTypes.Mark * BasicTypes.Mark) list 
	val selectSelWindow     : unit  -> 
	                          (BasicTypes.WinId * BasicTypes.WidId) Option.option
	val selectCursor        : BasicTypes.WidId -> BasicTypes.Mark

	val selectVarValue      : string -> string
	val setVarValue         : string -> string        -> unit

	val createAndPopUpMenu  : BasicTypes.Widget -> (int Option.option) -> 
	                          BasicTypes.Coord -> unit 

        val setScaleValue       : BasicTypes.WidId -> real -> unit
    end
