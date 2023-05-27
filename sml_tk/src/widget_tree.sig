(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/widget_tree.sig,v $
 
   Functions related to Path-Management.
  
   $Date: 2001/03/30 13:39:23 $
   $Revision: 3.0 $
   Author: Stefan Westmeier (Last modification by $Author: 2cxl $)

   (C) 1996, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

signature WIDGET_TREE =
    sig
	(* ***************************************************************** *)
	(* CHECKING the INTEGRITY of WIDGETS                                 *)
	(* ***************************************************************** *)

	val checkWidId              : string -> bool

	val checkOneWidgetConfigure : BasicTypes.WidgetType ->
	                              BasicTypes.Configure -> bool
	val checkWidgetConfigure    : BasicTypes.WidgetType ->
	                              BasicTypes.Configure list -> bool
	val checkOneWidgetBinding   : BasicTypes.WidgetType ->
	                              BasicTypes.Event -> bool
	val checkWidgetBinding      : BasicTypes.WidgetType ->
	                              BasicTypes.Binding list -> bool
	val checkOneMConfigure      : BasicTypes.MItemType ->
	                              BasicTypes.Configure -> bool
	val checkMItem              : BasicTypes.MItem -> bool
	val checkOneCConfigure      : BasicTypes.CItemType ->
	                              BasicTypes.Configure -> bool
	val checkCItem              : BasicTypes.CItem -> bool
	val checkWidget             : BasicTypes.Widget -> unit


	(* ***************************************************************** *)
	(* SELECTING WIDGETS from the internal GUI state 		     *)
	(* ***************************************************************** *)

	val getWidgetGUI     : BasicTypes.WidId -> BasicTypes.Widget
	val getWidgetGUIPath : BasicTypes.IntPath -> BasicTypes.Widget


	(* ***************************************************************** *)
	(* ADDING WIDGETS to the internal GUI state	 		     *)
	(* ***************************************************************** *)

	val addWidgetPathAssGUI  : BasicTypes.WinId -> BasicTypes.WidPath -> 
	                           BasicTypes.Widget -> unit
	val addWidgetsPathAssGUI : BasicTypes.WinId -> BasicTypes.WidPath -> 
	                           BasicTypes.Widget list -> unit

	val addWidgetGUI  : BasicTypes.WinId -> BasicTypes.WidPath -> 
	                    BasicTypes.Widget -> unit
	val addWidgetsGUI : BasicTypes.WinId -> BasicTypes.WidPath -> 
	                    BasicTypes.Widget list -> unit


	(* ***************************************************************** *)
	(* DELETING WIDGETS from the internal GUI state 	             *)
	(* ***************************************************************** *)

	val deleteWidgetGUI     : BasicTypes.WidId -> unit
	val deleteWidgetGUIPath : BasicTypes.IntPath -> unit


	(* ***************************************************************** *)
	(* UPDATING WIDGETS in the internal GUI state		             *)
	(* ***************************************************************** *)

	val updWidgetGUI     : BasicTypes.Widget -> unit
	val updWidgetGUIPath : BasicTypes.IntPath -> BasicTypes.Widget -> unit


	(* ***************************************************************** *)
	(* ADDING WIDGETS to the "real" GUI                                  *)
	(* ***************************************************************** *)

	val packWid0 : bool -> string -> BasicTypes.TclPath ->
	               BasicTypes.IntPath -> BasicTypes.WidId ->
		       BasicTypes.Pack list -> BasicTypes.Configure list ->
		       string -> BasicTypes.Binding list -> bool -> string

	val packWid : bool -> string -> BasicTypes.TclPath ->
	              BasicTypes.IntPath -> BasicTypes.WidId ->
		      BasicTypes.Pack list -> BasicTypes.Configure list ->
		      BasicTypes.Binding list -> bool -> string

	val packTextWid : bool -> BasicTypes.TclPath -> BasicTypes.IntPath -> 
	                  BasicTypes.WidId -> BasicTypes.ScrollType -> 
			  string -> BasicTypes.Annotation list -> 
			  BasicTypes.Pack list -> BasicTypes.Configure list -> 
			  BasicTypes.Binding list -> bool -> string

	val packListbox : bool -> BasicTypes.TclPath -> BasicTypes.IntPath -> 
	                  BasicTypes.WidId -> BasicTypes.ScrollType -> 
			  BasicTypes.Pack list -> BasicTypes.Configure list -> 
			  BasicTypes.Binding list -> bool -> string

	val packCanvas : bool -> BasicTypes.TclPath -> BasicTypes.IntPath -> 
	                 BasicTypes.WidId -> BasicTypes.ScrollType ->
			 BasicTypes.CItem list -> BasicTypes.Pack list -> 
			 BasicTypes.Configure list ->
                         BasicTypes.Binding list -> bool -> string

	val packMenu : bool -> BasicTypes.TclPath -> BasicTypes.IntPath -> 
	               BasicTypes.WidId -> BasicTypes.MItem list -> 
		       BasicTypes.Pack list -> BasicTypes.Configure list -> 
		       BasicTypes.Binding list -> bool -> string

	val packWidget  : bool -> BasicTypes.TclPath -> BasicTypes.IntPath -> 
	                  bool option -> BasicTypes.Widget -> string
	val packWidgets : bool -> BasicTypes.TclPath -> BasicTypes.IntPath ->
	                  bool option -> BasicTypes.Widget list -> string

	val packMenuItem  : BasicTypes.TclPath -> BasicTypes.IntPath -> 
	                    BasicTypes.WidId -> BasicTypes.MItem -> int list ->
			    string
	val packMenuItems : BasicTypes.TclPath -> BasicTypes.IntPath -> 
	                    BasicTypes.WidId -> BasicTypes.MItem list -> 
			    int list -> string


	(* ***************************************************************** *)
	(* UPDATING WIDGETS in the "real" GUI			             *)
	(* ***************************************************************** *)
(*
	val updConfigurePack  : WidId -> Configure list -> unit
	val updBindingPack    : WidId -> Binding list   -> unit

	val updWidgetPackPath : IntPath -> unit
	val updWidgetPack     : Widget  -> unit
*)

	(* ***************************************************************** *)
	(* EXPORTED FUNCTIONS				        	     *)
	(* ***************************************************************** *)

	val selectWidget     : BasicTypes.WidId -> BasicTypes.Widget
	val selectWidgetPath : BasicTypes.IntPath -> BasicTypes.Widget

	val addWidget        : BasicTypes.WinId -> BasicTypes.WidId -> 
	                       BasicTypes.Widget -> unit
	val deleteWidget     : BasicTypes.WidId -> unit
(*
	val updateWidget     : BasicTypes.Widget -> unit
*)

	(* ***************************************************************** *)
	(* IMPLEMENTATION: WIDGET CONTENTS 			             *)
	(* ***************************************************************** *)

	val select              : BasicTypes.WidId   ->
	                          BasicTypes.Configure list
	val selectCommand       : BasicTypes.WidId   -> BasicTypes.SimpleAction
	val selectCommandPath   : BasicTypes.IntPath -> BasicTypes.SimpleAction
	val selectSCommandPath  : BasicTypes.IntPath -> BasicTypes.ScaleAction
	val selectMCommandMPath : BasicTypes.IntPath -> int list ->
	                          BasicTypes.SimpleAction
	val selectMCommand      : BasicTypes.WidId   -> int list ->
	                          BasicTypes.SimpleAction
	val selectMCommandPath  : BasicTypes.IntPath -> int list ->
	                          BasicTypes.SimpleAction
	val selectBindings      : BasicTypes.WidId   -> BasicTypes.Binding list
	val selectBindKey       : BasicTypes.WidId   -> string ->
	                          BasicTypes.Action
	val selectBindKeyPath   : BasicTypes.IntPath -> string ->
	                          BasicTypes.Action
	val selectWidth         : BasicTypes.WidId -> int
	val selectHeight        : BasicTypes.WidId -> int
	val selectRelief        : BasicTypes.WidId -> BasicTypes.RelKind


	val configure        : BasicTypes.WidId -> BasicTypes.Configure list ->
	                       unit
	val newconfigure     : BasicTypes.WidId -> BasicTypes.Configure list ->
	                       unit
	val configureCommand : BasicTypes.WidId -> BasicTypes.SimpleAction ->
	                       unit
	val addBindings      : BasicTypes.WidId -> BasicTypes.Binding list ->
	                       unit
	val newBindings      : BasicTypes.WidId -> BasicTypes.Binding list ->
	                       unit
	val configureWidth   : BasicTypes.WidId -> int -> unit
	val configureRelief  : BasicTypes.WidId -> BasicTypes.RelKind -> unit
	val configureText    : BasicTypes.WidId -> string -> unit

	val insertText       : BasicTypes.WidId -> string -> BasicTypes.Mark ->
	                       unit
	val insertTextEnd    : BasicTypes.WidId -> string -> unit
	val deleteText       : BasicTypes.WidId -> 
	                       BasicTypes.Mark * BasicTypes.Mark -> unit
	val clearText        : BasicTypes.WidId -> unit


	val focus   : BasicTypes.WinId -> unit
	val deFocus : BasicTypes.WinId -> unit

	val grab    : BasicTypes.WinId -> unit
	val deGrab  : BasicTypes.WinId -> unit

	val popUpMenu : BasicTypes.WidId -> (int Option.option) -> 
	                BasicTypes.Coord -> unit
    end
