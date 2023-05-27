(* ***************************************************************************

   $Source: /repository/sml/sml_tk/src/tests+examples/scale_ex.sml,v $

   ScaleWid example

   $Date: 2001/03/30 13:39:34 $
   $Revision: 3.0 $
   Author: ludi (Last modification by $Author: 2cxl $)

   (C) 1999, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)


structure ScaleEx : sig
                      val go : unit -> unit
                    end =
struct
    open SmlTk

    val mainId = newWinId()

    val id1      = newWidgetId()
    val id2      = newWidgetId()
    val sc1Id    = newWidgetId()
    val sc2Id    = newWidgetId()
    val actId    = newWidgetId()
    val canvasId = newWidgetId()

    val imId = newCItemId()

    val ICON = ref (CTag {citemId = newCItemId(),
			  citemIds = []})
    val ACTIVE = ref true

    fun cvalue b r =
	let
	    fun showReal r =
		if r < 0.0 then
		    "-" ^ Real.toString(Real.abs r)
		else
		    Real.toString r
	in
	    if b then
		addConf id1 [Text(showReal r)]
	    else
		addConf id2 [Text(showReal r)]
	end

    fun value b _ =
	if b then
	    addConf id1 [Text(readVarValue "hscale")]
	else
	    addConf id2 [Text(readVarValue "vscale")]

    fun act _ =
	(if !ACTIVE then
	     (addConf actId [Text "Activate"];
	      addConf sc1Id [Active false];
	      addConf sc2Id [Active false])
	 else
	     (addConf actId [Text "Deactivate"];
	      addConf sc1Id [Active true];
	      addConf sc2Id [Active true]);
	 ACTIVE := not(!ACTIVE))

    val scales =
	Frame {widId   = newWidgetId(),
	       widgets = Grid [ScaleWid {widId    = sc1Id,
					 packings = [Column 1, Row 1],
					 configs  = [SCommand(cvalue false),
						     Variable "vscale",
						     SliderLength 15,
						     Length 180,
						     SLabel "VScale",
						     From (~1.0),
						     To 1.0, Digits 3,
						     Resolution 0.2,
						     BigIncrement 0.5],
					 bindings = []},
	                       ScaleWid {widId    = sc2Id,
					 packings = [Column 2, Row 2],
					 configs  = [SCommand(cvalue true),
						     Orient Horizontal,
						     Variable "hscale",
						     SliderLength 30,
						     Length 180,
						     SLabel "HScale"],
					 bindings = []},
			       Canvas {widId      = canvasId,
				       scrolltype = NoneScb,
				       citems     = [],
				       packings   = [Column 2, Row 1],
				       configs    = [Relief Raised, Width 275,
						     Height 235,
						     Background White],
				       bindings   = []}],
	       packings = [PadX 10, PadY 10],
	       configs  = [],
	       bindings = []}

    fun move _ =
	(value true ();
	 value false ();
	 delCItem canvasId imId;
	 let
	     val x = 2 * valOf(Int.fromString(readVarValue "hscale")) + 5
	     val y =
		 Real.round(valOf(Real.fromString(readVarValue "vscale"))
			    * 100.0 + 105.0)
	 in
	     ICON := updItemCoords (!ICON) [(x, y)]
	 end;
	 addCItem canvasId (!ICON))

    val displ =
	Frame {widId    = newWidgetId(),
	       widgets  = Grid [Label {widId    = newWidgetId(),
				       packings = [Column 1, Row 1, PadY 10],
				       configs  = [Text "HScale:",
						   Background Blue,
						   Foreground White],
				       bindings = []},
	                        Label {widId    = id1,
				       packings = [Column 2, Row 1, PadX 10],
				       configs  = [Background White, Width 10],
				       bindings = []},
				Label {widId    = newWidgetId(),
				       packings = [Column 1, Row 2],
				       configs  = [Text "VScale:",
						   Background Blue,
						   Foreground White],
				       bindings = []},
				Label {widId    = id2,
				       packings = [Column 2, Row 2],
				       configs  = [Background White, Width 10],
				       bindings = []},
				Button {widId    = newWidgetId(),
					packings = [Column 3, Row 1],
					configs  = [Text "Move", Width 15,
						    Command move],
					bindings = []},
				Button {widId    = actId,
					packings = [Column 3, Row 2],
					configs  = [Text "Deactivate",
						    Width 15,
						    Command act],
					bindings = []},
				Button {widId    = newWidgetId(),
					packings = [Column 3, Row 3, PadY 10],
					configs  =
					  [Text "Quit", Width 15,
					   Command(fn _ => exitTcl())],
					bindings = []}],
	       packings = [PadX 10, PadY 10],
	       configs  = [],
	       bindings = []}

    fun init _ =
	(ICON := CIcon {citemId  = imId,
			coord    = (170, 110),
			iconkind =
			  FileImage(OS.Path.concat(getLibPath(),
						   "images/smltk.gif"),
				    newImageId()),
			configs  = [Anchor NorthWest],
			bindings = []};
	setScale sc2Id 80.0;
	addCItem canvasId (!ICON);
	value true ();
	value false ())

    val main =
	mkWindow {winId    = mainId,
		  widgets  = Pack [scales, displ],
		  config   = [WinTitle "ScaleWid example"],
		  bindings = [],
		  init     = init}

    fun go() = startTcl [main]
end
