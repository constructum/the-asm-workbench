(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/widget_tree.sml,v $
 
   Functions related to Path-Management (and widgets). 
  
   $Date: 2001/03/30 13:39:23 $
   $Revision: 3.0 $
   Author: Burkhart Wolff (Last modification by $Author: 2cxl $)

   (C) 1996, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

structure WidgetTree : WIDGET_TREE = 
struct

local open BasicTypes GuiState BasicUtil in


(* *********************************************************************** *)
(* CHECKING the INTEGRITY of WIDGETS                                       *)
(* *********************************************************************** *)

(* yet implememed checks: widId of widgets /
                          configs of widgets, mitems and citems
   other checks may be added *)

fun checkWidget w =
    let
	val t = selWidgetWidgetType w
    in
	if checkWidId(selWidgetWidId w) then ()
	else (print("WidId " ^ selWidgetWidId w ^ " is not O.K.!");
	      raise WIDGET("WidId " ^ selWidgetWidId w ^ " is not O.K.!"));
	if checkWidgetConfigure t (selWidgetConfigure w) then ()
	else (print("Configures of Widget " ^ selWidgetWidId w ^
		    " are not O.K.!");
	      raise WIDGET("Configures of Widget " ^ selWidgetWidId w ^
			   " are not O.K.!"));
	if checkWidgetBinding t (selWidgetBinding w) then ()(* NOT YET IMPL. *)
	else (print("Bindings of Widget " ^ selWidgetWidId w ^
		    " are not O.K.!");
	      raise WIDGET("Bindings of Widget " ^ selWidgetWidId w ^
			   " are not O.K.!"));
	case w of
	    Menubutton {mitems,...} =>
		if List.all checkMItem mitems then ()
		else (print("MItems of Menubutton " ^ selWidgetWidId w ^
			    " are not O.K.!");
		      raise WIDGET("MItems of Menubutton " ^ selWidgetWidId w ^
				   " are not O.K.!"))
	  | Popup {mitems,...}      =>
		if List.all checkMItem mitems then ()
		else (print("MItems of Popup " ^ selWidgetWidId w ^
			    " are not O.K.!");
		      raise WIDGET("MItems of Popup " ^ selWidgetWidId w ^
				   " are not O.K.!"))
	  | Canvas {citems,...}     =>
		if List.all checkCItem citems then ()
		else (print("CItems of Canvas " ^ selWidgetWidId w ^
			    " are not O.K.!");
		      raise WIDGET("CItems of Canvas " ^ selWidgetWidId w ^
				   " are not O.K.!"))
	  | _                       => ()
    end

(* check on the widget-id. Currently only widget-ids that begin with    *)
(* lowercase, and further consist of alphanumerical characters allowed. *)
(* Tcl allows a wider range of strings.                                 *)

and checkWidId s =
    if size s = 0 then
	false
    else
	Char.isLower(String.sub(s, 0))
	andalso StringUtil.all Char.isAlphaNum s

and checkOneMConfigure MChe c =
    (case c of
	 Accelerator _ => true
       | Background _  => true
       | Foreground _  => true
       | Command _     => true
       | Text _        => true
       | Font _        => true
       | Variable _    => true
       | Value _       => true
       | MUnderline _  => true
       | _             =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for MCheckbutton!\n");
	      false))
  | checkOneMConfigure MRad c =
    (case c of
	 Accelerator _ => true
       | Background _  => true
       | Foreground _  => true
       | Command _     => true
       | Text _        => true
       | Font _        => true
       | Variable _    => true
       | Value _       => true
       | MUnderline _  => true
       | _             =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for MRadiobutton!\n");
	      false))
  | checkOneMConfigure MCo c  =
    (case c of
	 Accelerator _ => true
       | Background _  => true
       | Foreground _  => true
       | Command _     => true
       | Text _        => true
       | Font _        => true
       | MUnderline _  => true
       | _             =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for MCommand!\n");
	      false))
  | checkOneMConfigure MCas c =
    (case c of
	 Command _     => true
       | Background _  => true
       | Foreground _  => true
       | Text _        => true
       | Font _        => true
       | MUnderline _  => true
       | Tearoff _     => true
       | _             =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for MCascade!\n");
	      false))

and checkMItem MSeparator          = true
  | checkMItem (MCascade (ms, cs)) =
    Config.noDblP cs andalso List.all (checkOneMConfigure MCas) cs
    andalso List.all checkMItem ms
  | checkMItem mit                 =
    let
	val cs = selMItemConfigure mit
    in
	Config.noDblP cs andalso
	List.all (checkOneMConfigure (selMItemMItemType mit)) cs
    end

and checkOneCConfigure CTRectangle c =
    (case c of
	 FillColor _    => true
       | Outline _      => true
       | OutlineWidth _ => true
       | Width _        => true
       | _              =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for CRectangle!\n");
	      false))
  | checkOneCConfigure CTOval c      =
    (case c of
	 FillColor _    => true
       | Outline _      => true
       | OutlineWidth _ => true
       | Width _        => true
       | _              =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for COval!\n");
	      false))
  | checkOneCConfigure CTLine c      =
    (case c of
	 Arrow _        => true
       | Capstyle _     => true
       | FillColor _    => true
       | Joinstyle _    => true
       | Smooth _       => true
       | Width _        => true
       | _              =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for CLine!\n");
	      false))
  | checkOneCConfigure CTPoly c      =
    (case c of
	 FillColor _    => true
       | Outline _      => true
       | OutlineWidth _ => true
       | Smooth _       => true
       | Width _        => true
       | _              =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for CPoly!\n");
	      false))
  | checkOneCConfigure CTText c      =
    (case c of
	 Anchor _       => true
       | FillColor _    => true
       | Font _         => true
       | Justify _      => true
       | Text _         => true
       | Width _        => true
       | _              =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for CText!\n");
	      false))
  | checkOneCConfigure CTWidget c    =
    (case c of
	 Anchor _    => true
       | Height _    => true
       | Width _     => true
       | _           =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for CWidget!\n");
	      false))

and checkOneCIconConfigure true c  =
    (case c of
	 Anchor _     => true
       | Background _ => true
       | Foreground _ => true
       | _            =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for CIcon with NoIcon, TkBitmap or " ^
		    "FileBitmap!\n");
	      false))
  | checkOneCIconConfigure false c =
    (case c of
	 Anchor _     => true
       | _            =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for CIcon with FileImage!\n");
	      false))

and checkCItem (CTag _)                        = true
  | checkCItem (CIcon {iconkind, configs,...}) =
    (if Config.noDblP configs then true
     else (print "Double configure option in Widget definition!\n";
	   false)
     andalso
     case iconkind of
	 FileImage _ => List.all (checkOneCIconConfigure false) configs
       | _           => List.all (checkOneCIconConfigure true) configs)
  | checkCItem cit                             =
    let
	val cs = CItem.selItemConfigure cit
    in
	if Config.noDblP cs then true
	else (print "Double configure option in Widget definition!";
	      false)
	andalso
	List.all (checkOneCConfigure (CItem.selItemType cit)) cs
    end

and checkOneWidgetConfigure Fra c    =
    (case c of
	 Background _      => true
       | Borderwidth _     => true
       | ColorMap _        => true
       | Cursor _          => true
       | Height _          => true
       | Relief _          => true
       | Width _           => true
       | _                 =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for Frame!\n");
	      false))
  | checkOneWidgetConfigure Mes c    =
    (case c of
	 Anchor _          => true
       | Background _      => true
       | Borderwidth _     => true
       | Cursor _          => true
       | Font _            => true
       | Foreground _      => true
       | Justify _         => true
       | Relief _          => true
       | Text _            => true
       | Width _           => true
       | InnerPadX _       => true
       | InnerPadY _       => true
       | _                 =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for Message!\n");
	      false))
  | checkOneWidgetConfigure Lab c    =
    (case c of
	 Anchor _          => true
       | Background _      => true
       | Icon _            => true
       | Borderwidth _     => true
       | Cursor _          => true
       | Font _            => true
       | Foreground _      => true
       | Height _          => true
       | Justify _         => true
       | Relief _          => true
       | Text _            => true
       | Underline         => true
       | MUnderline _      => true
       | Width _           => true
       | InnerPadX _       => true
       | InnerPadY _       => true
       | _                 =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for Label!\n");
	      false))
  | checkOneWidgetConfigure Lis c    =
    (case c of
	 Background _      => true
       | Borderwidth _     => true
       | Cursor _          => true
       | Font _            => true
       | Foreground _      => true
       | Height _          => true
       | Relief _          => true
       | Width _           => true
       | _                 =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for Listbox!\n");
	      false))
  | checkOneWidgetConfigure But c    =
    (case c of
	 Anchor _          => true
       | Background _      => true
       | Borderwidth _     => true
       | Command _         => true
       | Cursor _          => true
       | Font _            => true
       | Foreground _      => true
       | Height _          => true
       | Icon _            => true
       | Justify _         => true
       | Relief _          => true
       | Text _            => true
       | Width _           => true
       | InnerPadX _       => true
       | InnerPadY _       => true
       | Active _          => true
       | _                 =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for Button!\n");
	      false))
  | checkOneWidgetConfigure Rad c    =
    (case c of
	 Anchor _          => true
       | Background _      => true
       | Borderwidth _     => true
       | Command _         => true
       | Cursor _          => true
       | Font _            => true
       | Foreground _      => true
       | Height _          => true
       | Icon _            => true
       | Justify _         => true
       | Relief _          => true
       | Text _            => true
       | Variable _        => true
       | Value _           => true
       | Width _           => true
       | InnerPadX _       => true
       | InnerPadY _       => true
       | Active _          => true
       | _                 =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for Radiobutton!\n");
	      false))
  | checkOneWidgetConfigure Che c    =
    (case c of
	 Anchor _          => true
       | Background _      => true
       | Borderwidth _     => true
       | Command _         => true
       | Cursor _          => true
       | Font _            => true
       | Foreground _      => true
       | Height _          => true
       | Icon _            => true
       | Justify _         => true
       | Relief _          => true
       | Text _            => true
       | Variable _        => true
       | Width _           => true
       | InnerPadX _       => true
       | InnerPadY _       => true
       | Active _          => true
       | _                 =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for Checkbutton!\n");
	      false))
  | checkOneWidgetConfigure Menbut c =
    (case c of
	 Anchor _          => true
       | Background _      => true
       | Borderwidth _     => true
       | Command _         => true
       | Cursor _          => true
       | Font _            => true
       | Foreground _      => true
       | Height _          => true
       | Icon _            => true
       | Justify _         => true
       | Relief _          => true
       | Text _            => true
       | Width _           => true
       | InnerPadX _       => true
       | InnerPadY _       => true
       | Active _          => true
       | Tearoff _         => true
       | MUnderline _      => true
       | _                 =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for Menubutton!\n");
	      false))
  | checkOneWidgetConfigure Sca c    =
    (case c of
	 Background _      => true
       | BigIncrement _    => true
       | Borderwidth _     => true
       | SCommand _        => true
       | Cursor _          => true
       | Digits _          => true
       | From _            => true
       | Font _            => true
       | Foreground _      => true
       | SLabel _          => true
       | Length _          => true
       | Orient _          => true
       | Relief _          => true
       | Resolution _      => true
       | ShowValue _       => true
       | SliderLength _    => true
       | SliderRelief _    => true
       | Active _          => true
       | TickInterval _    => true
       | To _              => true
       | Variable _        => true
       | Width _           => true
       | RepeatDelay _     => true
       | RepeatInterval _  => true
       | ThroughColor _    => true
       | _                 =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for Scale!\n");
	      false))
  | checkOneWidgetConfigure Ent c    =
    (case c of
	 Background _      => true
       | Borderwidth _     => true
       | Cursor _          => true
       | Font _            => true
       | Text _            => true
       | Foreground _      => true
       | Justify _         => true
       | Relief _          => true
       | Width _           => true
       | Active _          => true
       | Show _            => true
       | _                 =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for Entry!\n");
	      false))
  | checkOneWidgetConfigure Can c    =
    (case c of
	 Background _      => true
       | Borderwidth _     => true
       | Cursor _          => true
       | Height _          => true
       | Relief _          => true
       | ScrollRegion _    => true
       | Width _           => true
       | _                 =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for Canvas!\n");
	      false))
  | checkOneWidgetConfigure Tex c    =
    (case c of
	 Background _      => true
       | Borderwidth  _    => true
       | Cursor _          => true
       | Font _            => true
       | Foreground _      => true
       | Height _          => true
       | Relief _          => true
       | Active _          => true
       | Width _           => true
       | Wrap _            => true
       | InnerPadX _       => true
       | InnerPadY _       => true
       | _                 =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for TextWid!\n");
	      false))
  | checkOneWidgetConfigure Pop c    =
    (case c of
	 Background _      => true
       | Borderwidth _     => true
       | Cursor _          => true
       | Font _            => true
       | Foreground _      => true
       | Tearoff _         => true
       | _                 =>
	     (print("Wrong configure option:\n" ^ Config.confName c ^
		    " not allowed for Popup!\n");
	      false))

and checkWidgetConfigure wt cs  =
    (if Config.noDblP cs then true
     else (print "Double configure option in Widget definition!";
	   false))
    andalso
    List.all (checkOneWidgetConfigure wt) cs

and checkOneWidgetBinding _  _ = true                 (* NOT YET IMPLEMENTED *)

and checkWidgetBinding wt bs =
    Bind.noDblP bs andalso
    List.all ((checkOneWidgetBinding wt) o Bind.selEvent) bs



(* *********************************************************************** *)
(* SELECTING WIDGETS from the internal GUI state 			   *)
(* *********************************************************************** *)

(* on the toplevel, widgets must be Frames *)

(* getWidgetGUIPath is a variant that has the internal path as argument
   is needed for use with the event loop *)

fun getWidgetGUIPath (win, p) =
    let 
	(* val selWid         : Widget -> string -> Widget *)
	fun selWid w "" = w
	  | selWid (w as Listbox _) p = 
	    if p=".box" then w 
	    else raise WIDGET "Error occurred in function selWid 1"
	  | selWid (w as Canvas _) p = 
	    if (p = ".cnv") then
		w
	    else if (ListUtil.prefix (rev (explode".cnv.cfr")) (rev (explode p))) then
		raise WIDGET ("WidgetTree.getWidgetGUIPath: \"cfr\" should not appear")
	    else 
		let
		    val _ = Debug.print 2 ("selWid(Canv) "^(selWidgetWidId w)^" "^p)
		    val (wid,np)     = Paths.fstWidPath p   (* strip ".cnv" *)
		    val (wid',np')   = Paths.fstWidPath np  (* strip ".cfr" *)
		    val (wid'',np'') = Paths.fstWidPath np'
		    val _ = Debug.print 2 ("selWid(Canv) "^wid''^" "^np'')
		in
		    selWids (CItem.getCanvasWidgets w) wid'' np''
		end
	  | selWid (w as TextWid _) p = 
            if p=".txt" then 
		w 
	    else if (ListUtil.prefix (rev (explode".cnv.tfr")) (rev (explode p))) then
		raise WIDGET ("WidgetTree.getWidgetGUIPath: \"tfr\" should not appear")
            else 
		let
		    val _ = Debug.print 2 ("selWid(Canv) "^(selWidgetWidId w)^" "^p)
		    val (wid,np)     = Paths.fstWidPath p   (* strip ".txt" *)
		    val (wid',np')   = Paths.fstWidPath np  (* strip ".tfr" *)
		    val (wid'',np'') = Paths.fstWidPath np'
		    val _ = Debug.print 2 ("selWid(Canv) "^wid''^" "^np'')
		in
		    selWids (Annotation.getTextWidWidgets w) wid'' np''
		end
	  | selWid (Frame{widgets,...}) p = 
		    let 
			val (wid, np) = Paths.fstWidPath p 
		    in 
			selWids (selRawWids widgets) wid np 
		    end
	  | selWid _ s = raise WIDGET ("Error occurred in function selWid 3 " ^ s)

	(* val selWids        : Widget list -> WidId -> WidPath -> Widget *)
	and selWids wids w p = 
	    selWid (ListUtil.getx ((fn x=>w=x)o selWidgetWidId) wids 
		        (WIDGET ("selWids with widgetId \"" ^ w ^ "\""))) p

	val (w, np) = Paths.fstWidPath p        (* <-- w hier "" *)
    in  
	selWids (selWindowWidgets (getWindowGUI win)) w np
    end;

fun getWidgetGUI wId = getWidgetGUIPath (Paths.getIntPathGUI wId);


(* *********************************************************************** *)
(* ADDING WIDGETS to the internal GUI state	 			   *)
(* *********************************************************************** *)

(* val addWidgetPathAssGUI : WinId -> WidPath -> Widget -> unit *)
fun addWidgetPathAssGUI win p wid =
    if Paths.occursWidgetGUI (selWidgetWidId wid) then
	raise WIDGET("Two identical widget names not allowed: "^ 
		     (selWidgetWidId wid))
    else
	let 
	    val np = p ^ ("." ^ (selWidgetWidId wid))  
	    val ass = getPathAssGUI()
	    val nass = Paths.addWidget (selWidgetWidId wid) win np ass
	in
	    (updPathAssGUI nass;
	     case wid of
		 Frame{widId, widgets,...}   =>  addWidgetsPathAssGUI win np
                                                   (selRawWids widgets)
	       | Canvas _  =>
		     let
			 fun addOne (cit,ws) =
			     let
				 val np' = np ^ ".cnv." ^ (CItem.selItemId cit)
			     in
				 addWidgetsPathAssGUI win np' ws
			     end
			 val assl = CItem.getCanvasCItemWidgetAssList wid
		     in
			 app addOne assl
		     end
	       | TextWid _  =>
		     let
			 fun addOne (an,ws) =
			     let
				 val np' = np ^ ".txt." ^
				           (Annotation.selAnnotationId an)
			     in
				 addWidgetsPathAssGUI win np' ws
			     end
			 val assl = Annotation.getTextWidAnnotationWidgetAssList wid
		     in
			 app addOne assl
		     end
	       | _                     =>  ())
	end

(* val addWidgetsPathAssGUI : WinId -> WidPath -> Widget list -> unit *)
and addWidgetsPathAssGUI w p wids = app (addWidgetPathAssGUI w p) wids;



(* val addWidgetGUI  : WinId -> WidPath -> Widget      -> unit *)
fun addWidgetGUI win p wid = 
    let
	(* val addWids : Widget list -> Widget -> WidPath -> WidgetList *)
	fun addWids widgs widg "" = 
	    (Debug.print 2 ("addWids(final)");
	     widgs @ [widg])
	  | addWids widgs widg wp =
	    let
		val (wId,nwp)  = Paths.fstWidPath wp
		val nwidg      = ListUtil.getx ((fn x => x=wId) o selWidgetWidId) widgs
		                      (WIDGET ("addWids with widgetId \"" ^ wId ^ "\""))  
		val newwidg     = addWid nwidg widg nwp
	    in
		ListUtil.updateVal ((fn x => x=wId) o selWidgetWidId) newwidg widgs
	    end

	(* val addWid : Widget -> Widget -> WidPath -> Widget *)
	and addWid (Frame{widId,widgets,packings,configs,bindings}) widg wp =
	    Frame{widId=widId,
                  widgets=case widgets of Pack ws => Pack(addWids ws widg wp)
                                        | Grid ws => Grid(addWids ws widg wp),
                  packings=packings, configs=configs,bindings=bindings}
	  | addWid (w as (Canvas _)) widg wp =
	    (Debug.print 2 ("addWid(canv) "^" "^(selWidgetWidId w)^" "^
			 (selWidgetWidId widg)^" "^wp);
	    CItem.addCanvasWidget addWids w widg wp)
	  | addWid (w as (TextWid _)) widg wp =
	    (Debug.print 2 ("addWid(textw) "^" "^(selWidgetWidId w)^" "^
			 (selWidgetWidId widg)^" "^wp);
	    Annotation.addTextWidWidget addWids w widg wp)
	  | addWid _ _ _ =
	    raise WIDGET
		  "addWidgetGUI: attempt to add widget to non-container widget"
    in
	checkWidget wid;
	let 
	    val window   = getWindowGUI win
	    val newwids  = addWids (selWindowWidgets window) wid p
	    val newwindow = (win, selWindowConfigures window,
			     if isWindowGrid window then
				 Grid newwids
			     else
				 Pack newwids,
                             selWindowBindings window,
			     selWindowAction window)
	    val _ = Debug.print 2 ("addWidgetGUI: done")
	in
	    (addWidgetPathAssGUI win p wid;
	     updWindowGUI win newwindow)
	end
    end

and addWidgetsGUI w p wids = app (addWidgetGUI w p) wids


(* *********************************************************************** *)
(* DELETING WIDGETS from the internal GUI state 			   *)
(* *********************************************************************** *)

fun deleteWidgetGUI wId =
    let 
	(* val deleteWidgetPathAss : (Widget * PathAssList) -> PathAssList *)
	fun deleteWidgetPathAss ((widg as Frame{widId,widgets,...}),ass) =
	    let
		val nass = deleteWidgetsPathAss (selRawWids widgets,ass)
	    in
		Paths.deleteWidget widId nass
	    end
	  | deleteWidgetPathAss ((widg as Canvas{widId,...}),ass) =
	    let
		val widgs = CItem.getCanvasWidgets widg
		val nass = deleteWidgetsPathAss (widgs,ass)
	    in
		Paths.deleteWidget widId nass
	    end
	  | deleteWidgetPathAss ((widg as TextWid{widId,...}),ass) =
	    let
		val widgs = Annotation.getTextWidWidgets widg
		val nass = deleteWidgetsPathAss (widgs,ass)
	    in
		Paths.deleteWidget widId nass
	    end
	  | deleteWidgetPathAss (widg,ass) =
	    Paths.deleteWidget (selWidgetWidId widg) ass

	(* val deleteWidgetPathAss : (Widget list * PathAssList) -> PathAssList *)
	and deleteWidgetsPathAss (widgs,ass) =
	    foldr deleteWidgetPathAss ass widgs

	(* val delWid       : Widget -> WidId -> WidPath -> Widget *)
	fun delWid (Frame{widId,widgets,packings,configs,bindings}) w p = 
	    Frame{widId=widId,
                  widgets=case widgets of Pack ws => Pack(delWids ws w p)
                                        | Grid ws => Grid(delWids ws w p),
                  packings=packings,configs=configs,bindings=bindings}
	  | delWid (widg as (Canvas _)) w p =
	    CItem.deleteCanvasWidget delWids widg w p
	  | delWid (widg as (TextWid _)) w p =
	    Annotation.deleteTextWidWidget delWids widg w p
	  | delWid _                        _ _ = 
	    raise WIDGET "Error occurred in function delWid"

	(* val delWids      : Widget list -> WidId -> WidPath -> Widget list *)
	and delWids wids w "" = 
	    List.filter ((fn x=> not (w = x))o selWidgetWidId) wids
	  | delWids wids w p  =
	    let 
		val _ = Debug.print 2 ("delWids(Canv) "^w^" "^p)
		val wid = ListUtil.getx ((fn x=>w=x)o selWidgetWidId) wids 
		               (WIDGET ("delWids with widgetId \"" ^ w ^ "\""))
		val (nw, np) = Paths.fstWidPath p
		val newwid   = delWid wid nw np
	    in  
		 ListUtil.updateVal ((fn x=>w=x)o selWidgetWidId) newwid wids
	    end

	val _ = Debug.print 2 ("deleteWidgetGUI "^wId)
	val widg = getWidgetGUI wId;
	val (ip as (win, p)) = Paths.getIntPathGUI wId;

	val ass  = getPathAssGUI()
	val nass = deleteWidgetPathAss (widg,ass)

	val _ = Debug.print 2 ("deleteWidgetGUI(after nass) "^wId)
	val (nw, np) = Paths.fstWidPath p
	val window   = getWindowGUI win
        val newwids  = delWids (selWindowWidgets window) nw np
        val newwindow = (win, selWindowConfigures window,
                         if isWindowGrid window then
			     Grid newwids
			 else
			     Pack newwids,
                         selWindowBindings window,
			 selWindowAction window)
    in  
	updWindowGUI win newwindow;
	updPathAssGUI nass
    end

fun deleteWidgetGUIPath ip =
    deleteWidgetGUI(selWidgetWidId(getWidgetGUIPath ip))


(* *********************************************************************** *)
(* 3F. UPDATING WIDGETS in the internal GUI state			   *)
(* *********************************************************************** *)

(* updWidgetPath :: IntPath -> Widget s -> GUI s -> ((), GUI s) *)
fun updWidgetGUIPath (win, p) w =
    let 
	val _ = Debug.print 2 ("updWidgetGUIPath "^win^" "^p^" "^(selWidgetWidId w))
	(* val updWids : Widget list -> WidId -> WidPath -> Widget -> Widget list *)
	fun updWids wids w "" neww = 
	    ListUtil.updateVal ((fn x=>w=x)o selWidgetWidId) neww wids
	  | updWids wids w p  neww =
	    let 
		val _ = Debug.print 2 ("updWids "^w^" "^p);
		val wid      = ListUtil.getx ((fn x=>w=x)o selWidgetWidId) wids
		                    (WIDGET ("updWids with widgetId " ^ w))
		val (nw, np) = Paths.fstWidPath p
		val newwid   = updWid wid nw np neww
	    in  
		ListUtil.updateVal ((fn x=>w=x)o selWidgetWidId) newwid wids 
	    end

	(* val updWid : Widget -> WidId -> WidPath -> Widget -> Widget *)
	and updWid(Frame{widId, widgets, packings, configs, bindings}) w p neww = 
	    Frame{widId=widId,
                  widgets=case widgets of Pack ws => Pack(updWids ws w p neww)
                                        | Grid ws => Grid(updWids ws w p neww),
                  packings=packings,configs=configs, bindings=bindings}
	  | updWid (widg as (Canvas _)) w p neww =
	    (Debug.print 2 ("updWid(Canv) "^(selWidgetWidId widg)^" "^w^" "^p);
	    CItem.updCanvasWidget updWids widg w p neww)
	  | updWid (widg as (TextWid _)) w p neww =
	    (Debug.print 2 ("updWid(TextWid) "^(selWidgetWidId widg)^" "^w^" "^p);
	    Annotation.updTextWidWidget updWids widg w p neww)
	  | updWid _ _ _ _ = 
	    raise WIDGET  "Error occurred in function updWid";

	val (nw, np)  = Paths.fstWidPath p
        val window    = getWindowGUI win
        val newwids   = updWids (selWindowWidgets window) nw np w
        val newwindow = (win, selWindowConfigures window, 
			 if isWindowGrid window then
			     Grid newwids
			 else
			     Pack newwids,
                         selWindowBindings window,
			 selWindowAction window)
    in  
	updWindowGUI win newwindow 
    end

fun updWidgetGUI w =
    updWidgetGUIPath (Paths.getIntPathGUI (selWidgetWidId w)) w


(* *********************************************************************** *)
(* ADDING WIDGETS to the "real" GUI		 			   *)
(* *********************************************************************** *)
(* -- i.e. sending pack commands to Tcl/Tk *)

fun isGridPath (win, p) =
    (if p="" then
	 isWindowGrid(getWindowGUI win)
     else
	 case getWidgetGUIPath(win, p) of
	     Frame{widgets,...} => (case widgets of Grid _ => true
	                                          | _      => false)
	   | _                  => false)
    handle WIDGET _ => isGridPath (win, #1(Paths.lastWidPath p))

fun packWidgets doP tp ip gopt ws =
    concat (map (packWidget doP tp ip gopt) ws)

and packWidget doP tp (win, p) gopt w =
    let
	val wid  = selWidgetWidId w
	val nip  = (win, p ^ "." ^ wid)
	val ntp  = tp ^ "." ^ wid
	val grid =
	    if isSome gopt then
		valOf gopt
	    else
		isGridPath (win, p)
    in
	checkWidget w;
	case w of
	    Frame {widgets, packings, configs, bindings,...}             =>
		(packWid doP "frame" ntp nip wid packings configs bindings
		         grid ^
		 packWidgets true ntp nip (SOME (isGrid widgets))
		             (selRawWids widgets))
	  | Message {packings, configs, bindings,...}                    =>
		packWid doP "message" ntp nip wid packings configs bindings
		        grid
	  | Listbox {scrolltype, packings, configs, bindings,...}        =>
		packListbox doP ntp nip wid scrolltype packings configs
		            bindings grid
	  | Label {packings, configs, bindings,...}                      =>
		packWid doP "label" ntp nip wid packings configs bindings grid
	  | Button {packings, configs, bindings,...}                     =>
		packWid doP "button" ntp nip wid packings configs bindings grid
	  | Radiobutton {packings, configs, bindings,...}                =>
		packWid doP "radiobutton" ntp nip wid packings configs bindings
		        grid
	  | Checkbutton {packings, configs, bindings,...}                =>
		packWid doP "checkbutton" ntp nip wid packings configs bindings
		        grid
	  | Menubutton {mitems, packings, configs, bindings,...}         =>
		packMenu doP ntp nip wid mitems packings configs bindings grid
	  | TextWid {scrolltype, annotext, packings, configs,
		     bindings,...}                                       =>
	        packTextWid doP ntp nip wid scrolltype
		            (AnnotatedText.selText annotext)
			    (AnnotatedText.selAnno annotext) packings configs
			    bindings grid
	  | Canvas {scrolltype, citems, packings, configs, bindings,...} =>
		packCanvas doP ntp nip wid scrolltype citems packings configs
		           bindings grid
	  | Popup {mitems, configs,...}                                  =>
		packPopup doP ntp nip wid mitems configs
	  | Entry {packings, configs, bindings,...}                      =>
		packWid doP "entry" ntp nip wid packings configs bindings grid
	  | ScaleWid {packings, configs, bindings,...}                   =>
		packWid doP "scale" ntp nip wid packings configs bindings grid
    end

and packWid0 doP s tp ip w pack conf confstr binds grid =
    if doP then
	((if grid then
	      ("grid [" ^ s ^ " " ^ tp ^ " " ^ Config.pack ip conf ^
	       confstr ^ "] " ^ (Config.gridInfo pack) ^ "\n")
	  else ("pack [" ^ s ^ " " ^ tp ^ " " ^ Config.pack ip conf ^
		confstr ^ "] " ^ (Config.packInfo pack) ^ "\n")) ^
	  concat (Bind.packWidget tp ip binds))
    else
	(s ^ " " ^ tp ^ " " ^ Config.pack ip conf ^ confstr ^ "\n" ^
	 concat (Bind.packWidget tp ip binds))

and packWid doP s tp ip w pack conf binds grid =
    packWid0 doP s tp ip w pack conf "" binds grid

and packMenu doP tp (ip as (win, p)) w ms pack conf binds grid =
    let
	val mip   = (win, p ^ ".m")
	val mtp   = tp ^ ".m"
	val conf' = List.filter (not o (Config.confEq(Tearoff true))) conf
	val to    =
	    case List.find (Config.confEq (Tearoff true)) conf of
		NONE            => true
	      | SOME(Tearoff b) => b
    in
	((if doP then
	      ((if grid then
		    "grid [menubutton " ^ tp ^ " " ^
		    Config.pack ip conf' ^ " -menu " ^ mtp ^
		    "] " ^ Config.gridInfo pack ^ "\n"
		else
		    "pack [menubutton " ^ tp ^ " " ^ 
		    Config.pack ip conf' ^ " -menu " ^ mtp ^
		    "] " ^ Config.packInfo pack ^ "\n") ^
	       concat (Bind.packWidget tp ip binds))
	  else
	      ("menubutton " ^ tp ^ " " ^
	       Config.pack ip conf' ^ " -menu " ^ mtp ^ "\n" ^
	       concat (Bind.packWidget tp ip binds))) ^
	 "menu " ^ mtp ^ " -tearoff " ^ (Bool.toString to) ^ "\n" ^
	 packMenuItems mtp mip w ms [])
    end

and packPopup doP tp (ip as (win, p)) w ms conf =
    let
	val mip = (win, p ^ ".pop")
	val mtp = tp ^ ".pop"
    in
	"menu " ^ tp ^ Config.pack ip conf ^ "\n" ^ packMenuItems tp ip w ms []
    end

and packMenuItems tp ip wid mis m_item_path =
    let 
	fun pmi tp ip w []      n = ""
	  | pmi tp ip w (m::ms) n =
	    (packMenuItem tp ip w m (n :: m_item_path) ^
	     pmi tp ip w ms (n+1))
    in
	pmi tp ip wid mis 0
    end

and packMenuItem tp ip w (MSeparator) n                          =
    tp ^ " add separator" ^ "\n"
  | packMenuItem tp ip w (MCheckbutton (cs)) n                   =
    tp ^ " add checkbutton " ^ Config.packM ip (rev n) cs ^ "\n"
  | packMenuItem tp ip w (MRadiobutton (cs)) n                   =
    tp ^ " add radiobutton "^ Config.packM ip (rev n) cs ^ "\n"
  | packMenuItem tp (ip as (win, p)) w (MCascade (ms,cs)) (n::s) =
    let
	val ntp = tp ^ ".m" ^ Int.toString n
	val n2  = rev(n :: s)
	val cs' = List.filter (not o (Config.confEq (Tearoff true))) cs
	val to  =
	    case List.find (Config.confEq (Tearoff true)) cs of
		NONE            => true
	      | SOME(Tearoff b) => b
    in
	(tp ^ " add cascade "^ Config.packM ip n2 cs' ^ " -menu "^ntp ^ "\n" ^
	 "menu " ^ ntp ^ " -tearoff " ^ (Bool.toString to) ^ "\n" ^
	 packMenuItems ntp ip w ms (n :: s))
    end
  | packMenuItem tp ip w (MCommand cs) n                         =
    tp ^ " add command " ^ Config.packM ip (rev n) cs ^ "\n"

(* around Listboxes, there is always a Frame. This has the advantage, that *)
(* packing can treat "Listbox with scrollbar" as a unit. Commands address- *)
(* ing the "Listbox" have to take into account this change of paths... *)
and packListbox doP tp (ip as (win, pt)) wid NoneScb p c b grid =
    let
	val bip = (win, pt ^ ".box")
	val btp = tp ^ ".box"
    in
	(packWid doP "frame" tp ip wid p [] [] grid ^
	 packWid true "listbox" btp bip wid [Fill Both, Expand true] c b false)
    end
  | packListbox doP tp (ip as (win, pt)) wid scb (* C *) p c b grid =
    if single scb then
	let
	    val bip    = (win, pt ^ ".box")
	    val btp    = tp ^ ".box"
	    val scip   = (win, pt ^ ".scr")
	    val sctp   = tp ^ ".scr"
	    val si     = Side(scrollTypeToEdgeH scb)
	    val siquer = Side(scrollTypeToOppEdgeH scb)
	in
	    (packWid doP "frame" tp ip wid p [] [] grid ^
	     packWid true "listbox" btp bip wid
	             [siquer, Fill Both, Expand true] c b false ^
	     packWid true "scrollbar" sctp scip wid [si,Fill Y] [] []
	             false ^
	     btp ^ " configure -yscrollcommand \"" ^ sctp ^ " set \" " ^ "\n" ^
	     sctp ^ " configure -command \"" ^ btp ^ " yview\"" ^ "\n")
	end
    else
	let
	    val bip                           = (win, pt ^ ".box")
	    val btp                           = tp ^ ".box"
	    val vscip                         = (win, pt ^ ".hscr")
	    val hscip                         = (win, pt ^ ".vscr")
	    val vsctp                         = tp ^ ".hscr"
	    val hsctp                         = tp ^ ".vscr"
	    val (scbHpack, scbVpack, boxpack) = scrollTypeToGrid scb
	in
	    (packWid doP "frame" tp ip wid p [] [] grid ^
	     packWid true "scrollbar" hsctp hscip wid (scbHpack @ [Sticky EW])
	             [] [] true ^
	     packWid true "scrollbar" vsctp vscip wid (scbVpack @ [Sticky NS])
	             [] [] true ^
	     packWid true "listbox" btp bip wid (boxpack @ [Sticky NSEW]) c b
	             true ^
	     btp ^ " configure -xscrollcommand \"" ^ hsctp ^
	     " set \" " ^ "\n" ^
	     hsctp ^ " configure -command \"" ^ btp ^
	     " xview\"" ^ " -orient horizontal" ^ "\n" ^
	     btp ^ " configure -yscrollcommand \"" ^ vsctp ^
	     " set \" " ^ "\n" ^
	     vsctp ^ " configure -command \"" ^ btp ^
	     " yview\"" ^ "\n")
	end

(* around Canvases, there is always a Frame. This has the advantage, that *)
(* packing can treat "Canvas with scrollbar" as a unit. Commands address- *)
(* ing the "Canvas" have to take into account this change of paths... *)
and packCanvas doP tp (ip as (win, pt)) wid NoneScb ci p c b grid =
    let
	val cip = (win, pt ^ ".cnv")
	val ctp = tp ^ ".cnv"
    in
	(packWid doP "frame" tp ip wid p [] [] grid ^
	 packWid true "canvas" ctp cip wid [Fill Both, Expand true] c b false ^
	 concat (map (CItem.pack packWidget ctp cip) ci))
    end
  | packCanvas doP tp (ip as (win, pt)) wid scb ci p c b grid =
    if single scb then
	if orient scb then
	    let
		val cip    = (win, pt ^ ".cnv")
		val ctp    = tp ^ ".cnv"
		val vscip  = (win, pt ^ ".hscr")
		val vsctp  = tp ^ ".hscr"
		val vsi    = Side (scrollTypeToEdgeH scb)
		val siquer = Side (scrollTypeToOppEdgeH scb)
	    in
		(packWid doP "frame" tp ip wid p [] [] grid ^
		 packWid true "canvas" ctp cip wid
		         [siquer,Fill Both, Expand true] c b false ^
		 packWid true "scrollbar" vsctp vscip wid [vsi,Fill Y] [] []
		         false ^
		 ctp ^ " configure -yscrollcommand \"" ^ vsctp ^
		 " set \" " ^ "\n" ^
		 vsctp ^ " configure -command \"" ^ ctp ^ " yview\"" ^ "\n" ^
		 concat (map (CItem.pack packWidget ctp cip) ci))
	    end
	else
	    let
		val cip    = (win, pt ^ ".cnv")
		val ctp    = tp ^ ".cnv"
		val hscip  = (win, pt ^ ".vscr")
		val hsctp  = tp ^ ".vscr"
		val hsi    = Side (scrollTypeToEdgeV scb)
		val siquer = Side (scrollTypeToOppEdgeV scb)
	    in
		(packWid doP "frame" tp ip wid p [] [] grid ^
		 packWid true "canvas" ctp cip wid
		         [siquer, Fill Both, Expand true] c b false ^
		 packWid true "scrollbar" hsctp hscip wid [hsi,Fill X] [] []
		         false ^
		 ctp ^ " configure -xscrollcommand \"" ^ hsctp ^
		 " set \" " ^ "\n" ^
		 hsctp^" configure -command \""^ ctp ^
		 " xview\"" ^ " -orient horizontal" ^ "\n" ^
		 concat (map (CItem.pack packWidget ctp cip) ci))
              end
    else (* two scrollbars *)
	let
	    val cip   = (win, pt ^ ".cnv")
	    val ctp   = tp ^ ".cnv"
	    val vscip = (win, pt ^ ".hscr")
	    val hscip = (win, pt ^ ".vscr")
	    val vsctp = tp ^ ".hscr"
	    val hsctp = tp ^ ".vscr"
	    val (scbHpack, scbVpack, cnvpack) = scrollTypeToGrid scb
	in
	    (packWid doP "frame" tp ip wid p [] [] grid ^
	     packWid true "scrollbar" hsctp hscip wid (scbHpack @ [Sticky EW])
	             [] [] true ^
	     packWid true "scrollbar" vsctp vscip wid (scbVpack @ [Sticky NS])
	             [] [] true ^
	     packWid true "canvas" ctp cip wid
	             (cnvpack @ [Sticky NSEW]) c b true ^
	     ctp ^ " configure -xscrollcommand \"" ^ hsctp ^
	     " set \" " ^ "\n" ^
	     hsctp ^ " configure -command \"" ^ ctp ^
	     " xview\"" ^ " -orient horizontal" ^ "\n" ^
	     ctp ^ " configure -yscrollcommand \"" ^ vsctp ^
	     " set \" " ^ "\n" ^
	     vsctp ^ " configure -command \"" ^ ctp ^
	     " yview\"" ^ "\n" ^
	     concat (map (CItem.pack packWidget ctp cip) ci))
	end

(* At the moment only empty taglists ... *)
and packTextWid doP tp (ip as (win, pt)) wid NoneScb t ans p c b grid = 
    let
	val fdef = Font (Fonts.Normalfont [Fonts.NormalSize])

	val bip  = (win, pt ^ ".txt")
	val btp  = tp ^ ".txt"

	val nc   = List.filter (not o (Config.confEq (Active true))) c
	val sc   = List.filter (Config.confEq (Active true)) c

	val tt   = btp ^ " insert end \"" ^ StringUtil.adaptString t ^
	           "\"" ^ "\n"
	val stt  = btp ^ " configure " ^ (Config.pack bip sc) ^ "\n"

	val nc'  =
	    if (List.exists (Config.confEq fdef) nc) then nc else fdef::nc
    in
	(packWid doP "frame" tp ip wid p [] [] grid ^
	 packWid true "text" btp bip wid [Fill Both,Expand true] nc' b false ^
	 tt ^ stt ^
	 concat (map (Annotation.pack packWidget btp bip) ans))
    end
  | packTextWid doP tp (ip as (win, pt)) wid scb t ans p c b grid =
    if single scb then (* one scrollbar *)
	let
	    val fdef = Font (Fonts.Normalfont [Fonts.NormalSize])

	    val bip    = (win, pt ^ ".txt")
	    val btp    = tp ^ ".txt"
	    val scip   = (win, pt ^ ".scr")
	    val sctp   = tp ^ ".scr"
	    val si     = Side (scrollTypeToEdgeH scb)
	    val siquer = Side (scrollTypeToOppEdgeH scb)

	    val nc     = List.filter (not o (Config.confEq (Active true))) c
	    val sc     = List.filter (Config.confEq (Active true)) c

	    val tt     = btp ^ " insert end \"" ^ StringUtil.adaptString t ^
		         "\"" ^ "\n"
	    val stt    = btp ^ " configure " ^ Config.pack bip sc ^ "\n"

	    val nc'    =
		if (List.exists (Config.confEq fdef) nc) then nc else fdef::nc
	in
	    (packWid doP "frame" tp ip wid p [] [] grid ^
	     packWid true "text" btp bip wid [siquer,Fill Both,Expand true] nc'
	             b false ^
	     packWid true "scrollbar" sctp scip wid [si,Fill Y] [] [] false ^
	     btp ^ " configure -yscrollcommand \"" ^ sctp ^ " set \" " ^ "\n" ^
	     sctp^" configure -command \""^ btp ^ " yview\"" ^ "\n" ^
	     tt ^ stt ^
	     concat (map (Annotation.pack packWidget btp bip) ans))
	end
    else (* two scrollbars *)
	let
	    val fdef = Font (Fonts.Normalfont [Fonts.NormalSize])

	    val bip    = (win, pt ^ ".txt")
	    val btp    = tp ^ ".txt"

	    val vscip  = (win, pt ^ ".hscr")
	    val hscip  = (win, pt ^ ".vscr")
	    val vsctp  = tp ^ ".hscr"
	    val hsctp  = tp ^ ".vscr"

	    val (scbHpack, scbVpack, txtpack) = scrollTypeToGrid scb

	    val nc     = List.filter (not o (Config.confEq (Active true))) c
	    val sc     = List.filter (Config.confEq (Active true)) c

	    val tt     = btp ^ " insert end \"" ^ StringUtil.adaptString t ^
		         "\"" ^ "\n"
	    val stt    = btp ^ " configure " ^ Config.pack bip sc ^ "\n"

	    val nc'    =
		if (List.exists (Config.confEq fdef) nc) then nc else fdef::nc
	in
	    (packWid doP "frame" tp ip wid p [] [] grid ^
	     packWid true "scrollbar" hsctp hscip wid (scbHpack @ [Sticky EW])
	             [] [] true ^
	     packWid true "scrollbar" vsctp vscip wid (scbVpack @ [Sticky NS])
	             [] [] true ^
	     packWid true "text" btp bip wid (txtpack @ [Sticky NSEW]) nc'
	             b true ^
	     btp ^ " configure -xscrollcommand \"" ^ hsctp ^
	     " set \" " ^ "\n" ^
	     hsctp ^ " configure -command \"" ^ btp ^
	     " xview\"" ^ " -orient horizontal" ^ "\n" ^
	     btp ^ " configure -yscrollcommand \"" ^ vsctp ^
	     " set \" " ^ "\n" ^
	     vsctp ^ " configure -command \"" ^ btp ^
	     " yview\"" ^ "\n" ^
	     tt ^ stt ^
	     concat (map (Annotation.pack packWidget btp bip) ans))
	end


(* *********************************************************************** *)
(* UPDATING WIDGETS in the "real" GUI				   *)
(* *********************************************************************** *)

(* General Case 
--      the widget and its younger brothers must be destroyed 
--      and then newly packed. 
*)

(* val selWidgetsFrom : Widget -> WidId -> Widget list *)

(*

(* updPackWidgetPath :: IntPath -> GIO s () *)
fun updWidgetPackPath (win, p) =
    let
	fun selWidgetsFrom (Frame(_,ws,_,_,_))w =
	    dropWhile((fn x => w/=x)o selWidgetWidId) ws
	  | selWidgetsFrom _ _ =
	    raise WIDGET  "Error occurred in selWidgetsFrom"

	val (fp, w) = Paths.lastWidPath p
        val ftp = Paths.getTclPathGUI (win, fp)
    in
	if fp = "" then
	    let
		val wids = dropWhile ((fn x=>w/=x)o selWidgetWidId)
		                     (selWindowWidgets(getWindowGUI win))
	    in
		 packWidgets true ftp (win, fp) wids
	    end
        else
	    let
		val wids = selWidgetsFrom (getWidgetGUIPath (win, fp)) w
	    in
		packWidgets true ftp (win, fp) wids
	    end
    end

fun updWidgetPack w =
    updWidgetPackPath (Paths.getIntPathGUI (selWidgetWidId w))


(* Special Cases 
--       here we only have to send the appropriate Tcl/Tk scripts. 
*)

fun updConfigurePack wId cs =
    Com.putTclCmd (Config.pack (Paths.getIntPathGUI wId) cs)

fun updBindingPack w bs =  
    let 
	val ip = Paths.getIntPathGUI w
	val tp = Paths.getTclPathGUI ip                       
    in 
	BasicUtil.app Com.putTclCmd (Bind.packWidget tp ip bs) 
    end
*)

(* *********************************************************************** *)
(* 3H. EXPORTED FUNCTIONS						   *)
(* *********************************************************************** *)

val selectWidget = getWidgetGUI

val selectWidgetPath = getWidgetGUIPath

fun deleteWidget wid =
    (Debug.print 2 ("deleteWidget "^wid);
     Com.putTclCmd ("destroy " ^
		    (Paths.getTclPathGUI(Paths.getIntPathGUI wid)));
     deleteWidgetGUI wid)

fun addWidget winId widId widg = 
    let
	val widPath = Paths.getWidPathGUI widId
    in
	addWidgetGUI winId widPath widg;
	let
	    (* Kurzform: hoffentlich hab ich das mit den Pfanden 
	       alles richtig verstanden
               val nip      = (winId, widPath)
               val ntclp    = Paths.getTclPathGUI nip
	     *)
	    val wId      = selWidgetWidId widg;
	    val (win,wp) = Paths.getIntPathGUI wId;
	    val (nwp,l)  = Paths.lastWidPath wp;
	    val nip      = (win,nwp)
	    val ntclp    = Paths.getTclPathGUI nip;
	    val nwidg    = getWidgetGUI wId;
	in
	    Debug.print 2 ("addWidget: " ^ntclp^" ("^win^","^nwp^") "^wId);
	    Com.putTclCmd (packWidget true ntclp nip NONE nwidg)
	end
    end

(*
(* -- not yet implemented (sigh...) *)
fun updateWidget w =
    (checkWidget w;
     let
	 val ip = Paths.getIntPathGUI (selWidgetWidId w)
     in
	 updWidgetGUIPath ip w;
	 updWidgetPackPath ip
     end)

*)


(* *********************************************************************** *)
(*									   *)
(* IMPLEMENTATION: WIDGET CONTENTS		 			   *)
(*									   *)
(* *********************************************************************** *)

(* EXPORTED FUNCTIONS *)

val select = selWidgetConfigure o getWidgetGUI

val selectCommand = Config.selCommand o getWidgetGUI

val selectCommandPath = Config.selCommand o getWidgetGUIPath

val selectSCommandPath = Config.selSCommand o getWidgetGUIPath


(* this function gets the path of the Menubutton *)
fun selectMCommandPath ip n =
    let
	val w = getWidgetGUIPath ip
        fun sel_cascade ms [n]       = List.nth(ms,n)
	  | sel_cascade ms (n::m::s) =
	    case List.nth(ms,n) of
		MCascade (mms, _) => sel_cascade mms (m::s)
    in
	case w of Menubutton{mitems,...} =>
	              Config.selMCommand (sel_cascade mitems n)
                | Popup{mitems,...}      =>
		      Config.selMCommand (sel_cascade mitems n)
		| _                      => fn () => ()
    end

(* this function gets the menu path, i.e. a path with .m suffix *)
fun selectMCommandMPath (win, mp) n =
    let
	val (p, m) = Paths.lastWidPath mp
    in
	if ( m = "m" ) then
	    selectMCommandPath (win, p) n
	else
	    selectMCommandPath (win,mp) n
    end

fun selectMCommand wId n = selectMCommandPath (Paths.getIntPathGUI wId) n

val selectBindings = selWidgetBinding o getWidgetGUI

fun selectBindKey wId name    =
    Bind.getActionByName name (selWidgetBinding(getWidgetGUI wId))

fun selectBindKeyPath ip name =
    Bind.getActionByName name (selWidgetBinding(getWidgetGUIPath ip))

val selectWidth  = Config.selWidth o getWidgetGUI
    
val selectHeight = Config.selHeight o getWidgetGUI

val selectRelief = Config.selRelief o getWidgetGUI

fun configure w cs =
    let
	val ip  = Paths.getIntPathGUI w
	val wid = getWidgetGUIPath ip
	val tp  = Paths.getTclPathGUI ip
	val ntp =
	    case wid of
		TextWid _ => tp ^ ".txt"
	      | Canvas _  => tp ^ ".cnv"
	      | _         => tp
    in
	if checkWidgetConfigure (selWidgetWidgetType wid) cs then
	    let
		val oldcs  = selWidgetConfigure wid
		val newcs  = Config.add oldcs cs
		val newwid = updWidgetConfigure wid newcs
	    in
		(updWidgetGUIPath ip newwid;
		 Com.putTclCmd (ntp ^ " configure " ^ Config.pack ip cs))
	    end
	else
	    raise CONFIG "Trying to reconfigure with wrong type of configures"
    end

fun newconfigure w cs =
    let
	val ip = Paths.getIntPathGUI w
	val wid = getWidgetGUIPath ip
	val wt = selWidgetWidgetType wid
	val tp  = Paths.getTclPathGUI ip
	val ntp =
	    case wid of
		TextWid _ => tp ^ ".txt"
	      | Canvas _  => tp ^ ".cnv"
	      | _         => tp
    in
	if checkWidgetConfigure wt cs then
	    let
		val oldcs  = selWidgetConfigure wid
		val newcs  = Config.new wt oldcs cs
		val newwid = updWidgetConfigure wid newcs
	    in
		(updWidgetGUIPath ip newwid;
		 Com.putTclCmd (ntp ^ " configure " ^ Config.pack ip newcs))
	    end
	else
	    raise CONFIG "Trying to reconfigure with wrong type of configures"
    end

fun configureCommand w c = configure w [Command c]

fun configureWidth w n = configure w [Width n]

fun configureRelief w r = configure w [Relief r]

fun configureText w t = configure w [Text t]

fun addBindings w bs =
    let
	val ip  = Paths.getIntPathGUI w
	val wid = getWidgetGUIPath ip
	val tp  = Paths.getTclPathGUI ip
	val ntp =
	    case wid of
		TextWid _ => tp ^ ".txt"
	      | Canvas _  => tp ^ ".cnv"
	      | _         => tp
    in
	if checkWidgetBinding (selWidgetWidgetType wid) bs then
	    let
		val oldbs  = selWidgetBinding wid
		val newbs  = Bind.add oldbs bs
		val newwid = updWidgetBinding wid newbs
	    in
		(updWidgetGUIPath ip newwid;
		 Com.putTclCmd (concat(Bind.packWidget ntp ip bs)))
	    end
	else
	    raise CONFIG  "Trying to add wrong bindings"
    end

fun newBindings w bs =
    let
	val ip  = Paths.getIntPathGUI w
	val wid = getWidgetGUIPath ip
	val wt  = selWidgetWidgetType wid
	val tp  = Paths.getTclPathGUI ip
	val ntp =
	    case wid of
		TextWid _ => tp ^ ".txt"
	      | Canvas _  => tp ^ ".cnv"
	      | _         => tp
    in
	if checkWidgetBinding wt bs then
	    let
		val oldbs  = selWidgetBinding wid
		val oldks  = Bind.delete oldbs bs
		val newwid = updWidgetBinding wid bs
	    in
		(updWidgetGUIPath ip newwid;
		 Com.putTclCmd (concat (Bind.unpackWidget ntp wt oldks) ^
				concat (Bind.packWidget ntp ip bs)))
	    end
	else
	    raise CONFIG "Trying to newly set wrong bindings"
    end

fun insertText wid str m =
    let
	val tp = Paths.getWidPathGUI wid
	val ip = Paths.getIntPathGUI wid
        val w  = getWidgetGUIPath ip
	val (m1,_)= StringUtil.breakAtDot (Mark.show m)
    in
	case w of
	    TextWid _ =>
		Com.putTclCmd ((Paths.getTclPathGUI ip) ^
			       ".txt insert " ^ Mark.show m ^ " \"" ^
			       StringUtil.adaptString str ^ "\"")
	  | Listbox _ =>
		Com.putTclCmd ((Paths.getTclPathGUI ip) ^
			       ".box insert " ^ m1 ^
			       " \"" ^ StringUtil.adaptString str ^ "\" ")
	  | Entry _   =>
		Com.putTclCmd ((Paths.getTclPathGUI ip) ^ " insert " ^ m1 ^
			       " \"" ^ StringUtil.adaptString str ^ "\" ")
	  | _         =>
		raise WIDGET "text insertion in illegal window"
    end

fun insertTextEnd wid str = insertText wid str MarkEnd

fun deleteText wid (from,to) =
    let
	val tp      = Paths.getWidPathGUI wid
	val ip      = Paths.getIntPathGUI wid
        val w       = getWidgetGUIPath ip
	val (m1, _) = StringUtil.breakAtDot (Mark.show from)
	val (m2, _) = StringUtil.breakAtDot (Mark.show to)
    in
	case w of
	    TextWid _ =>
		Com.putTclCmd ((Paths.getTclPathGUI ip) ^ ".txt delete " ^
			       Mark.show from ^ " " ^ Mark.show to)
	  | Listbox _ =>
		Com.putTclCmd ((Paths.getTclPathGUI ip) ^ ".box delete " ^ m1 ^
			       " " ^ m2)
	  | Entry _   =>
		Com.putTclCmd ((Paths.getTclPathGUI ip) ^ " delete " ^ m1 ^
			       " " ^ m2)
	  | _         => raise WIDGET "text deletion in illegal window"
   end

fun clearText wid = deleteText wid (Mark(0, 0), MarkEnd)

fun focus win =
    if ( win = "main" orelse win = "." ) then
	Com.putTclCmd "focus ."
    else
	Com.putTclCmd ("focus ." ^ win)

fun deFocus _ =
    Com.putTclCmd "focus ."
(*  somewhat buggy:
    let
	val (win,p) = Paths.getIntPathGUI wid
    in
	if ( win = "main" ) then
	    Com.putTclCmd ("focus .")
	else
	    Com.putTclCmd ("focus ." ^ win)
    end
 *)

fun grab win =
    if win = "main" orelse win = "." then
	Com.putTclCmd "grab set ."
    else
	Com.putTclCmd ("grab set ." ^ win)

fun deGrab win =
    if win = "main" orelse win = "." then
	Com.putTclCmd "grab release ."
    else
	Com.putTclCmd ("grab release ." ^ win)

fun popUpMenu wid index co =
    let
	val tp  = Paths.getTclPathGUI(Paths.getIntPathGUI wid)
	val cot = Coord.show [co]

	fun popItUp (Menubutton _) (SOME i) =
	    Com.putTclCmd ("tk_popup " ^ tp ^ ".m " ^ cot ^ " " ^
			   Int.toString (i:int))
	  | popItUp (Menubutton _) NONE     =
	    Com.putTclCmd ("tk_popup " ^ tp ^ ".m " ^ cot)
	  | popItUp (Popup _ )     (SOME i) =
	    Com.putTclCmd ("tk_popup " ^ tp ^ " " ^ cot ^ " " ^
			   Int.toString (i:int))
	  | popItUp (Popup _ )     NONE     =
	    Com.putTclCmd ("tk_popup " ^ tp ^ " " ^ cot)
	  | popItUp _               _       =
	    raise WIDGET "WidgetTree.popUpMenu: tried to pop up non-MenuWidget"

	val widg = getWidgetGUI wid
    in
	popItUp widg index
    end

(* doesn't really work ---

fun createAndPopUpMenu widg index co =
    let
	val winid = Paths.newWidgetId()
	val frmid = Paths.newWidgetId()
	val frm   = Frame(frmId, [widg], [], [], [])
	val wid   = selWidgetWidId widg
    in
	Window.openW(winid, [], [frm], fn()=> ());
	popUpMenu wid frmid co
    end
 *)

end
end
