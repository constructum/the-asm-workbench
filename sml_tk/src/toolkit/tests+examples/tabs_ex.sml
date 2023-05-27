(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/tests+examples/tabs_ex.sml,v $

   SmlTk-Tabs example

   $Date: 2001/03/30 13:40:05 $
   $Revision: 3.0 $
   Author: ludi (Last modification by $Author: 2cxl $)

   (C) 2000, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)

structure TabsEx :
    sig
	val go : unit -> SmlTk.Widget
    end =
    struct
	open SmlTk

	val labId  = newWidgetId()
	val showId = newWidgetId()

	fun drop_newlines s =
	    String.implode(List.take(String.explode s, size s - 2))

	val TXT         = ref "Welcome"
	val FONT        = ref SansSerif
	val FONTSIZE    = ref NormalSize
	val BOLD        = ref false
	val ITALIC      = ref false
	val TXTCOL      = ref Blue
	val BGCOL       = ref Green
	val WIDTH       = ref 15
	val HEIGHT      = ref 2
	val RELIEF      = ref Raised
	val BORDERWIDTH = ref 2

	fun font_name f =
	    case f of
		SansSerif _  => "SansSerif"
	      | Typewriter _ => "Typewriter"
	      | Normalfont _ => "Normalfont"

	fun fontsize_name s =
	    case s of
		Tiny       => "Tiny"
	      | Small      => "Small"
	      | NormalSize => "NormalSize"
	      | Large      => "Large"
	      | Huge       => "Huge"

	fun rel_name r =
	    case r of
		Flat   => "Flat"
	      | Groove => "Groove"
	      | Ridge  => "Ridge"
	      | Raised => "Raised"
	      | Sunken => "Sunken"

	fun col_name c =
	    case c of
		Black  => "Black"
	      | White  => "White"
	      | Grey   => "Grey"
	      | Blue   => "Blue"
	      | Green  => "Green"
	      | Red    => "Red"
	      | Brown  => "Brown"
	      | Yellow => "Yellow"

	fun show_code() =
	    let
		val txtconf =
		    "Font(" ^ font_name((!FONT) []) ^ " [" ^ 
		    fontsize_name(!FONTSIZE) ^ 
		    (if !BOLD then ", Bold" else "") ^
		    (if !ITALIC then ", Italic" else "") ^ "]),\n"

		val txt = "Label {widId    = newWidgetId(),\n" ^
		          "       packings = [],\n" ^
			  "       configs  = [Text \"" ^ !TXT ^ "\",\n" ^
			  "                   " ^ txtconf ^
			  "                   Foreground " ^
			  col_name(!TXTCOL) ^ ",\n" ^
			  "                   Background " ^
			  col_name(!BGCOL) ^ ",\n" ^
			  "                   Width " ^
			  Int.toString(!WIDTH) ^ ",\n" ^
			  "                   Height " ^
			  Int.toString(!HEIGHT) ^ ",\n" ^
			  "                   Relief " ^
			  rel_name(!RELIEF) ^ ",\n" ^
			  "                   Borderwidth " ^
			  Int.toString(!BORDERWIDTH) ^ "],\n" ^
			  "       bindings = []}"
	    in
		(addConf showId [Active true];
		 clearText showId;
		 insertTextEnd showId txt;
		 addConf showId [Active false])
	    end

	fun color_chooser id act pack cols col =
	    Menubutton {widId    = id,
			mitems   =
			  [MCommand [Text "Black",
				     Command(act Black)],
			   MCommand [Text "White", Foreground White,
				     Command(act White)],
			   MCommand [Text "Grey", Foreground Grey,
				     Command(act Grey)],
			   MCommand [Text "Blue", Foreground Blue,
				     Command(act Blue)],
			   MCommand [Text "Green", Foreground Green,
				     Command(act Green)],
			   MCommand [Text "Red", Foreground Red,
				     Command(act Red)],
			   MCommand [Text "Brown", Foreground Brown,
				     Command(act Brown)],
			   MCommand [Text "Yellow", Foreground Yellow,
				     Command(act Yellow)]],
			packings = pack,
			configs  = [Width 20, Relief Raised, Tearoff false,
				    Text cols, Foreground col],
			bindings = []}

	fun fconf() =
	    [!FONTSIZE] @ (if !BOLD then [Bold] else []) @
	    (if !ITALIC then [Italic] else [])

	fun font() = (!FONT)(fconf())

	val page1 =
	    let
		val id      = newWidgetId()
		val fontId  = newWidgetId()
		val fsizeId = newWidgetId()

		fun ch_font f _ = (FONT := f;
				   addConf fontId [Text(font_name(f []))];
				   addConf labId [Font(font())];
				   show_code())

		fun ch_bold _ = (BOLD := not(!BOLD);
				 addConf labId [Font(font())];
				 show_code())

		fun ch_ital _ = (ITALIC := not(!ITALIC);
				 addConf labId [Font(font())];
				 show_code())

		fun ch_fsize s _ = (FONTSIZE := s;
				    addConf fsizeId [Text(fontsize_name s)];
				    addConf labId [Font(font())];
				    show_code())
	    in
		{title    = "Text Settings",
		 widgets  =
		   Pack
		     [Label {widId    = newWidgetId(),
			     packings = [PadY 8],
			     configs  = [Text "Enter text:"],
			     bindings = []},
		      TextWid {widId      = id,
			       annotext   = mkAT "Welcome",
			       scrolltype = RightScb,
			       packings   = [PadX 10],
			       configs    = [Width 60, Height 10,
					     Background White],
			       bindings   =
			         [BindEv(KeyPress "",
					 fn _ =>
					   (addConf labId
					            [Text
						       (drop_newlines
							  (readTextAll id))];
						    TXT :=
						      (drop_newlines
						         (readTextAll id));
						    show_code()))]},
		      Frame {widId    = newWidgetId(),
			     widgets  =
			       Grid
			         [Label {widId    = newWidgetId(),
					 packings = [Row 1, Column 1],
					 configs  = [Text "Font:"],
					 bindings = []},
				  Menubutton
				    {widId    = fontId,
				     mitems   =
				       [MCommand [Text "Normalfont",
						  Font(Normalfont []),
						  Command(ch_font Normalfont)],
					MCommand [Text "Typewriter",
						  Font(Typewriter []),
						  Command(ch_font Typewriter)],
					MCommand [Text "Sans Serif",
						  Font(SansSerif []),
						  Command(ch_font SansSerif)]],
				     packings = [Row 1, Column 2, PadY 5,
						 Sticky NSEW],
				     configs  = [Width 20, Relief Raised,
						 Text "SansSerif",
						 Font(SansSerif []),
						 Tearoff false],
				     bindings = []},
				  Label {widId    = newWidgetId(),
					 packings = [Row 2, Column 1],
					 configs  = [Text "Font size:"],
					 bindings = []},
				  Menubutton
				    {widId    = fsizeId,
				     mitems   =
				       [MCommand [Text "Tiny",
						  Font(SansSerif [Tiny]),
						  Command(ch_fsize Tiny)],
					MCommand [Text "Small",
						  Font(SansSerif [Small]),
						  Command(ch_fsize Small)],
					MCommand [Text "Normal",
						  Font(SansSerif
						         [NormalSize]),
						  Command(ch_fsize
							    NormalSize)],
					MCommand [Text "Large",
						  Font(SansSerif [Large]),
						  Command(ch_fsize Large)],
					MCommand [Text "Huge",
						  Font(SansSerif [Huge]),
						  Command(ch_fsize Huge)]],
				     packings = [Row 2, Column 2, PadY 5,
						 Sticky NSEW],
				     configs  = [Width 20, Relief Raised,
						 Text "Normal", Tearoff false,
						 Font(SansSerif [NormalSize])],
				     bindings = []},
				  Checkbutton {widId = newWidgetId(),
					       packings = [Row 3, Column 1],
					       configs  =
					         [Width 20, Text "Italic",
						  Font(SansSerif [Italic]),
						  Command ch_ital],
					       bindings = []},
				  Checkbutton {widId    = newWidgetId(),
					       packings = [Row 3, Column 2],
					       configs  =
					         [Width 20, Text "Bold",
						  Font(SansSerif [Bold]),
						  Command ch_bold],
					       bindings = []}],
			    packings = [],
			    configs  = [],
			    bindings = []}],
		 show     = fn() => (clearText id;
				     insertTextEnd id (!TXT);
				     addConf fsizeId
				             [Text(fontsize_name(!FONTSIZE))];
				     addConf fontId
				             [Text(font_name((!FONT) []))]),
		 hide     = noAction,
		 shortcut = SOME 0}
	    end

	val page2 =
	    let
		val id   = newWidgetId()
		val c1Id = newWidgetId()
		val c2Id = newWidgetId()

		fun foreground c _ = (TXTCOL := c;
				      addConf c1Id [Text(col_name c),
						     Foreground c];
				      addConf labId [Foreground c];
				      show_code())

		fun background c _ = (BGCOL := c;
				      addConf c2Id [Text(col_name c),
						    Foreground c];
				      addConf labId [Background c];
				      show_code())
	    in
	        {title    = "Color Settings",
		 widgets  =
		   Pack
		     [Frame {widId   = newWidgetId(),
			     widgets =
			       Grid
			           [Label {widId    = newWidgetId(),
					   packings = [Row 2, Column 1],
					   configs  =
					     [Text "Text color:"],
					   bindings = []},
				    color_chooser c1Id foreground
				                  [Row 2, Column 2, PadY 5]
						  "Blue" Blue,
				    Label {widId    = newWidgetId(),
					   packings = [Row 3, Column 1,
						       PadX 15],
					   configs  =
					     [Text "Background color:"],
					   bindings = []},
				    color_chooser c2Id background
				                  [Row 3, Column 2, PadY 5]
						  "Green" Green],
			     packings = [PadY 50],
			     configs  = [],
			     bindings = []}],
		 show     = fn() => (addConf c1Id [Text(col_name(!TXTCOL)),
						   Foreground(!TXTCOL)];
				     addConf c2Id [Text(col_name(!BGCOL)),
						   Foreground(!BGCOL)]),
		 hide     = noAction,
		 shortcut = SOME 0}
	    end

	fun page3() =
	    let
		val n_chooser1 =
		    NumericChooser.numeric_chooser
		      {initial_value      = 15,
		       min                = SOME 0,
		       max                = NONE,
		       increment          = 1,
		       width              = 3,
		       orientation        = Horizontal,
		       selection_notifier = fn i => (addConf labId [Width i];
						     WIDTH := i;
						     show_code())}

		val n_chooser2 =
		    NumericChooser.numeric_chooser
		      {initial_value      = 2,
		       min                = SOME 0,
		       max                = NONE,
		       increment          = 1,
		       width              = 2,
		       orientation        = Vertical,
		       selection_notifier = fn i => (addConf labId [Height i];
						     HEIGHT := i;
						     show_code())}
	    in
		{title    = "Dimensions",
		 widgets  =
		   Pack [Frame {widId   = newWidgetId(),
				widgets =
				  Grid [Frame
					  {widId    = newWidgetId(),
					   widgets  =
					     Pack
					       [Label
						  {widId    = newWidgetId(),
						   packings = [Side Left],
						   configs  =
						     [Text "Width:", Width 10],
						   bindings = []},
						#chooser n_chooser1],
					   packings = [Row 1, Column 2,
						       PadY 3, PadX 30],
					   configs  = [],
					   bindings = []},
					Frame
					  {widId    = newWidgetId(),
					   widgets  =
					     Pack
					       [Label
						  {widId    = newWidgetId(),
						   packings = [Side Left],
						   configs  = [Text "Height:",
							       Width 10],
						   bindings = []},
						#chooser n_chooser2],
					   packings = [Row 1, Column 4,
						       PadY 3, PadX 30],
					   configs  = [],
					   bindings = []}],
				packings = [PadY 50],
				configs  = [],
				bindings = []}],
		   show     = fn() => ((#set_value n_chooser1) (!WIDTH);
				       (#set_value n_chooser2) (!HEIGHT)),
		   hide     = noAction,
		   shortcut = SOME 0}
	    end

	fun page4 () =
	    let
		val id1 = newWidgetId()
		val id2 = newWidgetId()

		fun rel relk _ = (addConf labId [Relief relk];
				  RELIEF := relk;
				  show_code())
		val n_chooser =
		    NumericChooser.numeric_chooser
		      {initial_value = 2,
		       min           = SOME 0,
		       max           = NONE,
		       increment     = 1,
		       width         = 3,
		       orientation   = Horizontal,
		       selection_notifier = fn i => (addConf labId
						             [Borderwidth i];
						     BORDERWIDTH := i;
						     show_code())}

		fun rel_val() =
		    case !RELIEF of
			Flat   => "0"
		      | Groove => "1"
		      | Raised => "2"
		      | Ridge  => "3"
		      | Sunken => "4"
	    in
		{title    = "Relief",
		 widgets  =
		   Pack
		     [Frame {widId    = newWidgetId(),
			     widgets  =
			       Pack
			         [Frame
				    {widId    = newWidgetId(),
				     widgets  =
				       Grid
				         [Label {widId    = newWidgetId(),
						 packings = [Row 1, Column 1,
							     PadX 10, PadY 5],
						 configs  = [Text "Relief: ",
							     Width 15],
						 bindings = []},
					  Radiobutton
					    {widId    = newWidgetId(),
					     packings = [Row 1, Column 2,
							 Sticky W],
					     configs  =
					       [Text "Flat",
						Variable "relief", Value "0",
						Command(rel Flat),
						Font(SansSerif [])],
					     bindings = []},
					  Radiobutton
					    {widId    = newWidgetId(),
					     packings = [Row 1, Column 3,
							 Sticky W],
					     configs  =
					       [Text "Groove",
						Variable "relief", Value "1",
						Command(rel Groove),
						Font(SansSerif [])],
					     bindings = []},
					  Radiobutton
					    {widId    = newWidgetId(),
					     packings = [Row 2, Column 2,
							 Sticky W],
					     configs  =
					       [Text "Raised",
						Variable "relief", Value "2",
						Command(rel Raised),
						Font(SansSerif [])],
					     bindings = []},
					  Radiobutton
					    {widId    = newWidgetId(),
					     packings = [Row 2, Column 3,
							 Sticky W],
					     configs  =
					       [Text "Ridge",
						Variable "relief", Value "3",
						Command(rel Ridge),
						Font(SansSerif [])],
					     bindings = []},
					  Radiobutton
					    {widId    = newWidgetId(),
					     packings = [Row 3, Column 2,
							 Sticky W],
					     configs  =
					       [Text "Sunken",
						Variable "relief", Value "4",
						Command(rel Sunken),
						Font(SansSerif [])],
					     bindings = []}],
				     packings = [PadY 30],
				     configs  = [],
				     bindings = []},
				  Frame
				    {widId    = newWidgetId(),
				     widgets  =
				       Pack
				         [Label {widId    = newWidgetId(),
						 packings = [PadX 10, PadY 20,
							     Side Left],
						 configs  =
						   [Text "Borderwidth:",
						    Width 15],
						 bindings = []},
					  Frame
					    {widId    = newWidgetId(),
					     widgets  =
					       Pack [#chooser n_chooser],
					     packings = [PadX 20, Side Left],
					     configs  = [],
					     bindings = []}],
				     packings = [Row 2, Column 2, Side Left],
				     configs  = [],
				     bindings = []}],
				packings = [],
				configs  = [],
				bindings = []}],
		 show     = fn() => (setVarValue "relief" (rel_val());
				     (#set_value n_chooser) (!BORDERWIDTH)),
		 hide     = fn() => BORDERWIDTH := (#read_value n_chooser) (),
		 shortcut = SOME 0}
	    end

	val page5 =
	    {title    = "Info",
	     widgets  = Pack [Label {widId    = newWidgetId(),
				     packings = [PadY 30],
				     configs  = [Text "SmlTk-Tabs example",
						 Foreground Red,
						 Font(SansSerif [Huge])],
				     bindings = []},
	                      Label {widId    = newWidgetId(),
				     packings = [],
				     configs  = [Text "(C) 2000, Bremen Institute for Safe Systems, Universitaet Bremen\nAdded to the SmlTk Toolkit in april 2000",
						 Font(SansSerif [Large])],
				     bindings = []}],
	     show     = fn() => (),
	     hide     = fn() => (),
	     shortcut = SOME 0}

	fun go() =
	    let
		val (tabs, shortcuts) =
		    Tabs.tabs
		      {pages     = [page1, page2, page3(), page4(), page5],
		       configure = {width       = 700,
				    spare       = 50,
				    height      = 300,
				    font        = SansSerif [Bold],
				    labelheight = 34}}
	    in
		(TXT         := "Welcome";
		 FONT        := SansSerif;
		 FONTSIZE    := NormalSize;
		 BOLD        := false;
		 ITALIC      := false;
		 TXTCOL      := Blue;
		 BGCOL       := Green;
		 WIDTH       := 15;
		 HEIGHT      := 2;
		 RELIEF      := Raised;
		 BORDERWIDTH := 2;
		 startTcl [mkWindow {winId    = newWinId(),
				     widgets  =
				       Pack [Frame {widId    = newWidgetId(),
						    widgets  = Pack [tabs],
						    packings =
						      [PadX 10, PadY 10],
						    configs  = [],
						    bindings = []},
					     Frame
					       {widId   = newWidgetId(),
						widgets =
						  Pack
						    [Button
						       {widId    =
							  newWidgetId(),
							packings =
							  [Side Right],
							configs  =
							  [Text "Ok", Width 15,
							   Command
							     (fn _ =>
							        exitTcl())],
							bindings = []}],
						packings = [PadY 5, Fill X,
							    Expand true],
						configs  = [],
						bindings = []}],
				     config   = [WinTitle "Tabs example"],
				     bindings = shortcuts,
				     init     = noAction},
		           mkWindow {winId    = newWinId(),
				     widgets  =
				       Pack [Label {widId    = labId,
						    packings =
						      [PadX 10, PadY 10],
						    configs  =
						      [Text "Welcome",
						       Font
						         (SansSerif
							    [NormalSize]),
						       Foreground Blue,
						       Background Green,
						       Borderwidth 2,
						       Relief Raised,
						       Height 2,
						       Width 15],
						    bindings = []}],
				     config   = [WinTitle "Constructed label"],
				     bindings =
				       [BindEv(Destroy, fn _ => exitTcl())],
				     init     = noAction},
			   mkWindow {winId    = newWinId(),
				     widgets  =
				       Pack [TextWid
					       {widId      = showId,
						scrolltype = RightScb,
						annotext   = mtAT,
						packings   =
						  [PadX 10, PadY 10],
						configs    = [Width 80,
							      Height 15,
							      Active false],
						bindings   = []}],
				     config   = [WinTitle "Label code"],
				     bindings =
				       [BindEv(Destroy, fn _ => exitTcl())],
				     init     = show_code}];
		 Label {widId    = newWidgetId(),
			packings = [],
			configs  = [Text(!TXT), Font(font()),
				    Foreground(!TXTCOL),
				    Background(!BGCOL), Width(!WIDTH),
				    Height(!HEIGHT), Relief(!RELIEF),
				    Borderwidth(!BORDERWIDTH)],
			bindings = []})
	    end
    end
