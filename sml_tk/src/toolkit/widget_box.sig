(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/widget_box.sig,v $

   Widget boxes signature file

   $Date: 2001/03/30 13:39:57 $
   $Revision: 3.0 $
   Author: ludi (Last modification by $Author: 2cxl $)
 
   (C) 1999, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

signature WIDGET_BOX =
    sig
	exception WidgetBox

	type WBoxItemId

	val widgetBox            : {widId      : SmlTk.WidId,
				    scrolltype : SmlTk.ScrollType,
				    widgets    : SmlTk.Widget list,
				    packings   : SmlTk.Pack list,
				    configs    : SmlTk.Configure list,
				    bindings   : SmlTk.Binding list} ->
	                           SmlTk.Widget
	                           (* Widget box "constructor" *)

	val insertWidgetBoxAt    : SmlTk.WidId * int -> SmlTk.Widget ->
	                           WBoxItemId
	                           (* inserts entries at a specific line *)

	val insertWidgetBoxAtEnd : SmlTk.WidId -> SmlTk.Widget -> WBoxItemId
	                           (* inserts entries at the end *)

	val delWidgetBox         : SmlTk.WidId -> WBoxItemId -> unit
	                           (* delete entry *)

	val clearWidgetBox       : SmlTk.WidId -> unit
                                   (* deletes all entries *)

	val replaceWidgetBox     : SmlTk.WidId* SmlTk.Widget list-> 
	                                                       WBoxItemId list
	                           (* replaces contents with new widgets *)
    end
