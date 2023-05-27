(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/tests+examples/table_ex.sml,v $

   SmlTk-Tables example

   $Date: 2001/03/30 13:40:05 $
   $Revision: 3.0 $
   Author: ludi (Last modification by $Author: 2cxl $)

   (C) 1999, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)

structure TableEx :
    sig
	val go : unit -> unit
    end =
    struct
	open SmlTk

	fun blue_text s =
	    AnnoText {annotations = [TATag {annId   = newAnnotationId(),
					    marks   = [(Mark(1, 0), MarkEnd)],
					    configs = [Foreground Blue,
						       Underline],
					    bindings = []}],
		      len         = NONE,
		      str         = s}

	val tab =
	    Table.table Table.std_conf
	      [[blue_text "Widgets", blue_text "Configs", blue_text "Comment"],
	       [mkAT "Button", mkAT "Foreground, Background, ...",
		mkAT "Simple button Widget"],
	       [mkAT "Label", mkAT "Foreground, Background, ...",
		mkAT "Simple text label"],
	       [mkAT "TextWid", mkAT "Foreground, Background,\nText, ...",
		mkAT "Text entry widget"],
	       [mkAT "Entry", mkAT "Foreground, Background, ...",
		mkAT "Line entry widget"],
	       [mkAT "Frame", mkAT "Foreground, Background, ...",
		mkAT "Container"],
	       [mkAT "...", mkAT "...", mkAT "..."]]

	val quitbutton = Button {widId    = newWidgetId(),
				 packings = [],
				 configs  = [Text "Quit",
					     Command(fn _ => exitTcl())],
				 bindings = []}

	fun go() = startTcl [mkWindow {winId    = newWinId(),
				       widgets  = Pack [tab, quitbutton],
				       config   = [WinTitle "Table example"],
				       bindings = [],
				       init     = noAction}]
    end
