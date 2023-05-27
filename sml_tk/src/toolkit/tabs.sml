(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/tabs.sml,v $

   SmlTk-Tabs

   $Date: 2001/03/30 13:39:52 $
   $Revision: 3.0 $
   Author: ludi (Last modification by $Author: 2cxl $)

   (C) 2000, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)

structure Tabs : TABSSIG =
    struct
	exception Error of string

	open SmlTk

	fun tabs {pages : {title    : string,
			   widgets  : SmlTk.Widgets,
			   show     : SmlTk.SimpleAction,
			   hide     : SmlTk.SimpleAction,
			   shortcut : int option} list,
		  configure as {width, spare, height, font,
				labelheight}} =
	    let
		val canvasId = newWidgetId()
		val cwId = newCItemId()

		val SELECTED_CARD = ref 0

		val lwidth = (width - spare) div (length pages)

		fun init_citemIds n =
		    if n = 0 then []
		    else [newCItemId(), newCItemId(), newCItemId(),
			  newCItemId(), newCItemId(),
			  newCItemId()] :: init_citemIds(n - 1)

		val citemIds = init_citemIds(length pages)

		fun init_frames n =
		    if  n = 0 then []
		    else init_frames(n - 1) @
			 [Frame {widId    = newWidgetId(),
				 widgets  = #widgets(List.nth(pages, n - 1)),
				 packings = [],
				 configs  = [],
				 bindings = []}]

		val frames = init_frames(length pages)

		fun page n = CWidget {citemId  = cwId,
				      coord    = (3, labelheight + 1),
				      widgets  = Pack [List.nth(frames, n)],
				      configs  = [Anchor NorthWest,
						  Width width, Height height],
				      bindings = []}

		fun id lab idn = List.nth(List.nth(citemIds, lab), idn)

		fun field_name n =
		    [Text(#title(List.nth(pages, n)))] @
		    (if isSome(#shortcut(List.nth(pages, n))) then
			 [MUnderline(valOf(#shortcut(List.nth(pages, n))))]
		     else [])

		fun selected_page n _ =
		    (delete_label(!SELECTED_CARD);
		     add_inactive_label(!SELECTED_CARD);
		     delete_label n;
		     add_active_label n;
		     #hide(List.nth(pages, !SELECTED_CARD))();
		     delCItem canvasId cwId;
		     addCItem canvasId (page n);
		     #show(List.nth(pages, n))();
		     SELECTED_CARD := n)

		and button_pressed ev =
		    let
			val (x, y) = (selXPos ev, selYPos ev)
			val n = x div lwidth
		    in
			if (y < labelheight andalso n < length pages andalso
			    not(n = !SELECTED_CARD)) then
			    selected_page n (TkEvent(0, "", 0, 0, 0, 0))
			else ()
		    end

		and delete_label n =
		    app (fn id => delCItem canvasId id handle _ => ())
		        (List.nth(citemIds, n))

		and active_label n =
		    [CWidget {citemId  = id n 0,
			      coord    = (n * lwidth + 10, labelheight div 2),
			      widgets  =
			        Pack [Label {widId    = newWidgetId(),
					     packings = [],
					     configs  = field_name n @
					                [Font font],
					     bindings = []}],
			      configs  = [Anchor West],
			      bindings = []},
		     CLine {citemId  = id n 1,
			    coords   = [(n * lwidth + 1, labelheight),
					(n * lwidth + 1, 1),
					((n + 1) * lwidth , 1)],
			    configs  = [FillColor White],
			    bindings = []},
		     CLine {citemId  = id n 2,
			    coords   = [(n * lwidth + 2, labelheight),
					(n * lwidth + 2, 2),
					((n + 1) * lwidth - 1, 2)],
			    configs  = [FillColor White],
			    bindings = []},
		     CLine {citemId  = id n 3,
			    coords   = [((n + 1) * lwidth, 1),
					((n + 1) * lwidth, labelheight)],
			    configs  = [],
			    bindings = []},
		     CLine {citemId  = id n 4,
			    coords   = [((n + 1) * lwidth - 1, 2),
					((n + 1) * lwidth - 1,
					 labelheight)],
			    configs  = [],
			    bindings = []},
		     CLine {citemId  = id n 5,
			    coords   = [(n * lwidth + 6, labelheight - 5),
					((n + 1) * lwidth - 5,
					 labelheight - 5),
					((n + 1) * lwidth - 5, 6),
					(n * lwidth + 6, 6),
					(n * lwidth + 6, labelheight - 5)],
			    configs  = [FillColor Grey],
			    bindings = []}]
			    
		and add_active_label n =
		    app (addCItem canvasId) (active_label n)

		and inactive_label n =
		    [CWidget {citemId  = id n 0,
			      coord    = (n * lwidth + 10,
					  labelheight div 2 + 2),
			      widgets  =
			        Pack [Label {widId    = newWidgetId(),
					     packings = [],
					     configs  = field_name n @
					                [Font font],
					     bindings =
					       [BindEv(ButtonPress(SOME 1),
						       selected_page n)]}],
			      configs  = [Anchor West],
			      bindings = []},
		     CLine {citemId  = id n 1,
			    coords   = [(n * lwidth + 1, labelheight),
					(n * lwidth + 1, 3),
					((n + 1) * lwidth - 2, 3)],
			    configs  = [FillColor White],
			    bindings = []},
		     CLine {citemId  = id n 2,
			    coords   = [(n * lwidth + 2, labelheight - 1),
					(n * lwidth + 2, 4),
					((n + 1) * lwidth - 3, 4)],
			    configs  = [FillColor White],
			    bindings = []},
		     CLine {citemId  = id n 3,
			    coords   = [((n + 1) * lwidth - 2, 3),
					((n + 1) * lwidth - 2,
					 labelheight)],
			    configs  = [],
			    bindings = []},
		     CLine {citemId  = id n 4,
			    coords   = [((n + 1) * lwidth - 3, 4),
					((n + 1) * lwidth - 3,
					 labelheight)],
			    configs  = [],
			    bindings = []},
		     CLine {citemId  = id n 5,
			    coords   = [((n + 1) * lwidth + 1,
					 labelheight - 1),
					(n * lwidth + 1, labelheight - 1),
					(n * lwidth + 1, labelheight),
					((n + 1) * lwidth + 1, labelheight)],
			    configs  = [FillColor White],
			    bindings = []}]

		and add_inactive_label n =
		    app (addCItem canvasId) (inactive_label n)

		fun init_labels 0 = active_label 0
		  | init_labels n =
		    inactive_label n @ init_labels(n - 1)

		val init =
		    [CLine {citemId  = newCItemId(),
			    coords   = [(2, labelheight),
					(2, height + labelheight + 2)],
			    configs  = [FillColor White],
			    bindings = []},
		     CLine {citemId  = newCItemId(),
			    coords   = [(1, labelheight),
					(1, height + labelheight + 3)],
			    configs  = [FillColor White],
			    bindings = []},
		     CLine {citemId  = newCItemId(),
			    coords   = [(2, height + labelheight + 2),
			                (width + 3, height + labelheight + 2),
					(width + 3, labelheight + 1)],
			    configs  = [],
			    bindings = []},
		     CLine {citemId  = newCItemId(),
			    coords   = [(1, height + labelheight + 3),
					(width + 4, height + labelheight + 3),
					(width + 4, labelheight)],
			    configs  = [],
			    bindings = []},
		     CLine {citemId  = newCItemId(),
			    coords   = [(width + 4, labelheight - 1),
					(lwidth * length pages,
					 labelheight - 1)],
			    configs  = [FillColor White],
			    bindings = []},
		     CLine {citemId  = newCItemId(),
			    coords   = [(width + 3, labelheight),
					(lwidth * length pages, labelheight)],
			    configs  = [FillColor White],
			    bindings = []}] @ init_labels(length pages - 1) @
		    [page 0]

		fun check_shortcuts() =
		    let
			fun sc_equal (p1 :  {title    : string,
					     widgets  : SmlTk.Widgets,
					     show     : SmlTk.SimpleAction,
					     hide     : SmlTk.SimpleAction,
					     shortcut : int option})
			             (p2 :  {title    : string,
					     widgets  : SmlTk.Widgets,
					     show     : SmlTk.SimpleAction,
					     hide     : SmlTk.SimpleAction,
					     shortcut : int option}) =
			    if (isSome(#shortcut p1) andalso
				isSome(#shortcut p2)) then
				if (Char.toUpper
				      (String.sub(#title p1,
						  valOf(#shortcut p1))) =
				    Char.toUpper
				      (String.sub(#title p2,
						  valOf(#shortcut p2)))) then
				    true
				else false
			    else false

			fun no_doubles ((p :  {title    : string,
					       widgets  : SmlTk.Widgets,
					       show     : SmlTk.SimpleAction,
					       hide     : SmlTk.SimpleAction,
					       shortcut : int option}) :: ps) =
			    not(List.exists (sc_equal p) ps) andalso
			    no_doubles ps
			  | no_doubles _         = true
		    in
			if no_doubles pages then ()
			else
			    (print
		   "Error: Two shortcuts with the same character, aborting...";
			      raise
				  Error
				    "Two shortcuts with the same character")
		    end

		fun shortcuts ((p : {title    : string,
				     widgets  : SmlTk.Widgets,
				     show     : SmlTk.SimpleAction,
				     hide     : SmlTk.SimpleAction,
				     shortcut : int option}) :: ps) n =
		    (if isSome(#shortcut p) then
			 [BindEv
			    (Meta(KeyPress(Char.toString
					     (Char.toUpper
					        (String.sub
						   (#title p,
						      valOf(#shortcut p)))))),
			     selected_page n),
			  BindEv
			    (Meta(KeyPress(Char.toString
					     (Char.toLower
					        (String.sub
						   (#title p,
						    valOf(#shortcut p)))))),
			     selected_page n)]
		     else []) @ shortcuts ps (n + 1)
		  | shortcuts _ _         = []
	    in
		check_shortcuts();
		SELECTED_CARD := 0;
		(Canvas {widId      = canvasId,
			 scrolltype = NoneScb,
			 citems     = init,
			 packings   = [],
			 configs    = [Borderwidth 0,
				       Width(width + 6),
				       Height(height + labelheight + 4)],
			 bindings   = [BindEv(ButtonPress(SOME 1),
					      button_pressed)]},
		 shortcuts pages 0)
	    end

	val std_conf = {width       = 450,
			spare       = 50,
			height      = 500,
			font        = SansSerif [Bold],
			labelheight = 34}
    end
