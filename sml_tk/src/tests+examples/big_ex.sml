(* ***********************************************************************

   Project: sml/Tk: an Tk Toolkit for sml
   Author: Stefan Westmeier, University of Bremen
           (ported to SmlTk30 by bu)
   Date: $Date: 2001/03/30 13:39:30 $
   Revision: $Revision: 3.0 $
   Purpose of this file: Test for Canvas and other new stuff ...

   *********************************************************************** *)

structure BigEx :
    sig val go : unit-> string end  =
struct 

open SmlTk SmlTk21

(*
structure daVinciSMLTK :
sig
    val startDaVinci : unit -> unit
    val stopDaVinci  : unit -> unit
end
=
struct
    val appId = "DAVINCI"
    val prog  = ("/usr/local/software/daVinci/daVinci",["-pipe"])
    val prot  = "/tmp/g2da.log"

    fun callBack s = (insertTextEnd "aText" (s ^ "\n"))
    val quitAction = fn () => Com.putLineApp appId "quit";

    fun stopDaVinci () = Com.removeApp(appId)

    fun startDaVinci () = 
	let
        (* These two functions are not delivered with sml_tk .
         * They are intended to convert a file test2.nf into a string
         * representation of a graph which daVinci can understand
         *)
	    val g = FdrNf.parse("/home/stefan/bkb/sml/fdrnf/fdr-examples/test2.nf")
	    val s = StringGraph.graph2daVinci(g,"0")
	in
	    (addApp(appId,prog,prot,callBack,quitAction);
	     Com.putLineApp appId ("new_term_placed(" ^ s ^ ")") )
	end

end;

val startDaVinci = daVinciSMLTK.startDaVinci;
val stopDaVinci = daVinciSMLTK.stopDaVinci;
*)

(* --- path to images ---- *)

fun getImgPath name = OS.Path.joinDirFile{dir= OS.Path.concat(getLibPath(),
							    "tests+examples"),
	  				  file= name}
 

(* ----------------------------- Id's ------------------------------------ *)

val mainWinId      = mkWinId "meister";
val aTextId        = mkWidgetId "atext"
val aLabelId       = mkWidgetId "alabel"
val hideButtonId   = mkWidgetId "hidebutton"
val hiddenButtonId = mkWidgetId "hiddenButton"
val hiderId        = mkWidgetId "hider"

val hiddenFrameId  = mkWidgetId "hiddenframe"
val hideFrameId    = mkWidgetId "hiddeframe"
val hider1Id       = mkWidgetId "hider1"

val bLabelId       = mkWidgetId "bLabel"
val daviId         = mkWidgetId "davi" 
val texterId       = mkWidgetId "texter"  
val daVinciHiderId = mkWidgetId "davincihider" 
val daVinciButtonStartId  = mkWidgetId "davincibuttonstart" 
val daVinciButtonStopId   = mkWidgetId "davincibuttonstop"
val EntryWinId     = mkWinId "entry"
val EntryId        = mkWidgetId "entry"
val CanvasfrId     = mkWidgetId "canvasfr" 
val mesCanFrId     = mkWidgetId "mescanFr"
val CanvasId       = mkWidgetId "canvas"

val cnvHiddenButtonId = mkWidgetId "cnvhiddenbutton"
val cnvHiderId = mkWidgetId "cnvhider"
val cnvHideButtonId = mkWidgetId "cnvhidebutton"
val cnvDeleterId = mkWidgetId "cnvdeleter"

val c1Id = mkWidgetId "c1"

val it0_cid = mkCItemId "it0"
val it1_cid = mkCItemId "it1"
val it2_cid = mkCItemId "it2"
val it3_cid = mkCItemId "it3"
val l1_2_cid = mkCItemId "l1-2"
val l2_3_cid = mkCItemId "l2-3"
val l3_1_cid = mkCItemId "l3-1"
val it4_cid = mkCItemId "it4"
val it4a_cid = mkCItemId "it4a"
val it5_cid = mkCItemId "it5"
val it6_cid = mkCItemId "it6"
val its_cid = mkCItemId "its"

(* ----------------------------- hide Simple Widget ---------------------- *)
(* *)
val startDaVinci = fn _ => (insertTextEnd aTextId "Start\n");
val stopDaVinci  = fn _ => (insertTextEnd aTextId "Stop\n");
(* *)
val doQuit       = fn () => closeWindow (mainWinId);

fun aLabel () = Label(aLabelId,[],[Text "My Example"],[]);

fun doHideButton () = (delWidget (hiddenButtonId);
		       addConf hideButtonId [Text "Add"];
		       addConf hideButtonId [Command doAddButton] )

and doAddButton () = (addWidget (mainWinId) hiderId (hiddenButton());
		      addConf hideButtonId [Text "Hide"];
		      addConf hideButtonId [Command doHideButton])

and hideButton hide = Button(hideButtonId,
			     [Side Left,Fill X,Expand true],
			     [Text "Hide",Command hide],[])

and hiddenButton () = Button(hiddenButtonId,
			     [Side Left,Fill X,Expand true],
			     [Text "ToHide",Command noAction],[]);

fun hider () = Frame(hiderId, [hideButton doHideButton, hiddenButton ()],
		      [Fill X],
		      [Relief Ridge,Borderwidth 2],[]);

(* ----------------------------- hide Recursive Widget ---------------------- *)

fun doHideFrame () = (delWidget (hiddenFrameId);
		      addConf hideFrameId [Text "Add"];
		      addConf hideFrameId [Command doAddFrame] )

and doAddFrame () = (addWidget (mainWinId) hider1Id (hiddenFrame());
		     addConf hideFrameId [Text "Hide"];
		     addConf hideFrameId [Command doHideFrame])

and hideFrame hide = Button(hideFrameId,
			    [Fill X,Expand true],
			    [Text "Hide",Command hide],[])

and hiddenButton1 x = Button(mkWidgetId("hiddenButton"^x),
			     [Side Left,Fill X,Expand true],
			     [Text ("ToHide"^x),Command noAction],[])

and hiddenFrame () = Frame(hiddenFrameId,
			   [hiddenButton1 "A",hiddenButton1 "B"],
			   [Fill X],[Relief Ridge,Borderwidth 2],[]);

fun hider1 () = Frame(hider1Id,
		      [hideFrame doHideFrame,
		       hiddenFrame ()],
		      [Fill X],
		      [Relief Ridge,Borderwidth 2],[]);

(* ----------------------------- daVinci Starter ------------------------------ *)


fun bLabel() = Label(bLabelId,[],[Text "Start daVinci"],[])

and doHideDaVinci () = (delWidget (daviId);delWidget (texterId))

and bButton () = Button(daVinciHiderId,[Fill X],
			[Text "Hide daVinci",Command doHideDaVinci],[])

and daVinciButtonStart() = Button(daVinciButtonStartId,[Fill X],
				  [Text "Start",Command startDaVinci],[])
and daVinciButtonStop() = Button(daVinciButtonStopId,[Fill X],
				 [Text "Stop",Command stopDaVinci],[])

and daVinciStarter () = Frame(daviId, [bLabel(),bButton(),
				       daVinciButtonStart(),
				       daVinciButtonStop()],
			      [Fill X],
			      [Relief Ridge,Borderwidth 2],[])

and aText () = TextWid(aTextId,NoneScb,mtAT,[Fill X],[Width 60,Height 10],[])

and texter() = Frame(texterId,[aText()],[Fill X],[Relief Ridge,Borderwidth 2],[]);

fun Entry () = Frame(newWidgetId (),
		     [SmlTk21.Entry(EntryId ,[Fill X],[],
			    [BindEv(KeyPress "Return",
					     (fn (_) => 
					      let
						  val t = readTextAll EntryId
					      in
						  ((* WAS : deFocus "Entry"; 
                                                      changed by bu *)
                                                   deFocus mainWinId;
						   addConf mesCanFrId 
						   [Text ("Entered: \""^t^"\"")])
					      end)
					     )]
			    )],
		     [Fill X],[Relief Ridge,Borderwidth 2],[]);

val count = ref (0:int);
val pos   = ref (0,0);

fun Canvasfr () = Frame(CanvasfrId,
			[Canvas(),mesCanFr()],
			[Fill X],
			[Relief Ridge,Borderwidth 2],[])

and mesCanFr () = Message (mesCanFrId,
			   [Side Top,Fill X,Expand true],
			   [Width 350,Text "some Text"],
			   [])

and CanvasItemsBindings wid cit = 			 
    [BindEv (Enter, enterIt wid cit),
     BindEv (Leave, leaveIt wid cit),
     BindEv (Motion, wrMotC wid cit),
     BindEv (Shift(ButtonPress (SOME 3)), greyIt wid cit),
     BindEv (Alt(ButtonPress (SOME 3)), displayWidth wid cit),
     BindEv (Ctrl(ButtonPress (SOME 3)), displayHeight wid cit),
     BindEv (ButtonPress (SOME 3), deleteIt wid cit),
     BindEv (ButtonPress (SOME 1), grabIt wid cit),
     BindEv (Double(ButtonPress (SOME 1)), greyIt wid cit),
     BindEv (ButtonRelease (SOME 1), dropIt wid cit),
     BindEv (ModButton(1, Motion), moveIt wid cit)]

and CanvasItems (wid:WidId) = 
    [
      COval(it1_cid,(50,50),(100,100),
	    [FillColor Red,OutlineWidth 3],
	    CanvasItemsBindings wid it1_cid),
      CRectangle(it2_cid,(200,200),(250,250),
		 [FillColor Red,Outline NoColor],
		 CanvasItemsBindings wid it2_cid),
      COval(it3_cid,(50,200),(100,250),
	    [FillColor NoColor,Outline Green,OutlineWidth 3],
	    CanvasItemsBindings wid it3_cid),
      CLine(l1_2_cid,[(75,75),(150,100),(200,150),(225,225)],
	    [FillColor Brown ,OutlineWidth 10,Smooth true],
	    CanvasItemsBindings wid l1_2_cid),
      CLine(l2_3_cid,[(225,225),(75,225)],
	    [FillColor White ,OutlineWidth 3],
	    CanvasItemsBindings wid l2_3_cid),
      CLine(l3_1_cid,[(75,225),(75,75)],
	    [FillColor Blue ,OutlineWidth 5],
	    CanvasItemsBindings wid l3_1_cid),
      CIcon(it4_cid,(300,250),
	    FileBitmap (getImgPath "myex.bmp"),
	    [Background Blue,Foreground Yellow,Anchor NorthWest],
	    CanvasItemsBindings wid it4_cid),
      CWidget(it5_cid,(250,100),newCItemFrameId(),
	      [Button(mkWidgetId"canvBut",[Fill X],
		      [Text "Add Subitem",
		       Command (fn () => addSubCanvas wid )],
		      [])],
	      [],
	      [Anchor NorthWest],
	      CanvasItemsBindings wid it5_cid),
      CWidget(it6_cid,(200,10),newCItemFrameId(),
	      subCanvas wid it6_cid,
	      [Background Green],
	      [Anchor NorthWest,Width 200,Height 180],
	      CanvasItemsBindings wid it6_cid),
      CTag(its_cid,[it1_cid,it2_cid,it3_cid,it4_cid,it5_cid])
      ]



and cnvDoHideButton (wid:WidId) (cid:CItemId)() = 
    (delWidget (cnvHiddenButtonId);
     addConf cnvHideButtonId [Text "Add"];
     addConf cnvHideButtonId [Command (cnvDoAddButton wid cid)] )

and cnvDoAddButton (wid:WidId) (cid:CItemId) () =
    let
	val cit = getCItem wid cid
    in
	(addWidget (mainWinId) cnvHiderId (cnvHiddenButton());
	 addConf cnvHideButtonId [Text "Hide"];
	 addConf cnvHideButtonId [Command (cnvDoHideButton wid cid)])
 
    end

and cnvHideButton hide = Button(cnvHideButtonId,
				[Side Left,Fill X,Expand true],
				[Text "Hide",Command hide],[])

and cnvHiddenButton () = Button(cnvHiddenButtonId,
				[Side Left,Fill X,Expand true],
				[Text "Delete",
				 Command (fn () => delCItem c1Id it1_cid)],
				[])

and testit () = (addConf cnvHiddenButtonId [Text "Deleted"])

and cnvHider (wid:WidId) (cid:CItemId) = 
    Frame(cnvHiderId,
	  [cnvHideButton (cnvDoHideButton wid cid),
	   cnvHiddenButton ()],
	  [Fill X,PadX 5,PadY 5],
	  [Relief Ridge,Borderwidth 2],[])

and cnvDeleter (wid:WidId) (cid:CItemId) =
    Button(cnvDeleterId,
	   [Fill X,Expand true,PadX 5,PadY 5],
	   [Text "Quit Subitem",
	    Command (fn () => delCItem wid cid)],
	   [])

and subCanvas (wid:WidId) (cid:CItemId) = 
    [cnvHider wid cid,
     cnvDeleter wid cid,
     SmlTk21.Canvas(c1Id,RightScb,
	          [ COval(it1_cid,(25,25),(75,75),
		    [FillColor Red,OutlineWidth 3],
		    []) ],
	          [PadX 5,PadY 5],
	          [Background Yellow,Borderwidth 2,Relief Ridge],
	          [])]

and addSubCanvas (wid:WidId) =
    let
	val cid = newCItemId()
	val cit = CWidget(cid,(200,10),newCItemFrameId(),
			  subCanvas wid cid,
			  [Background Green],
			  [Anchor NorthWest,Width 200,Height 180],
			  CanvasItemsBindings wid cid)
    in
	addCItem wid cit
    end
    

and Canvas ()   = let val c = CanvasId
                  in SmlTk21.Canvas(c,RightScb,CanvasItems c,
			          [Side Top,Fill X,Expand true],
			          [Height 300,Width 200,Background Yellow,
			          Borderwidth 2,Relief Ridge],
			          [BindEv(ButtonPress (SOME 2),addOneItem c)]) 
                  end

and addOneItem (wid:WidId) (e:TkEvent) =
    let
	val x    = selXPos e
	val y    = selYPos e
	val ncid = newCItemId ();
	val ncit = COval(ncid,(x-25,y-25),(x+25,y+25),
			 [FillColor Red,OutlineWidth 3],
			 CanvasItemsBindings wid ncid)
    in
	addCItem wid ncit
    end

and deleteIt (wid:WidId) (cid:CItemId) (_:TkEvent) =
    delCItem wid cid

and displayWidth (wid:WidId) (cid:CItemId) (_:TkEvent) =
    addConf mesCanFrId [Text ("Item \""^(CItemIdToString cid)^"\" has width: "^
				(Int.toString (readCItemWidth wid cid)))]

and displayHeight (wid:WidId) (cid:CItemId) (_:TkEvent) =
    addConf mesCanFrId [Text ("Item \""^(CItemIdToString cid)^"\" has height: "^
				(Int.toString (readCItemHeight wid cid)))]

and greyIt (wid:WidId) (cid:CItemId) (_:TkEvent) =
    (addCItemConf wid cid [FillColor Grey];
     addCItemBind wid cid [BindEv (Shift(ButtonPress (SOME 3)), blueIt wid cid)])

and blueIt (wid:WidId) (cid:CItemId) (_:TkEvent) =
    (addCItemConf wid cid [FillColor Blue];
     addCItemBind wid cid [BindEv (Shift(ButtonPress (SOME 3)), greyIt wid cid)])

and enterIt (wid:WidId) (cit:CItemId) (_:TkEvent) = 
    (addConf mesCanFrId [Text ("<Enter Canvas Item(" ^ 
                                (WidIdToString wid) ^ "," ^ 
                                (CItemIdToString cit) ^ ")>")];
     addConf wid [Cursor(XCursor("hand2",NONE))])

and grabIt (wid:WidId) (cid:CItemId) (TkEvent(_,_,x,y,_,_)) =
    (pos := (x,y);
     addConf wid [Cursor(XCursor("fleur",NONE))])

and moveIt (wid:WidId) (cid:CItemId) (TkEvent(_,_,x,y,_,_)) = 
    let
	val cit_col  = readCItemCoords wid cid
	val (xP,yP)  = !pos
	val _        = (pos:=(x,y))
	val delta    = (x-xP,y-yP)
	val cit_col' = map (addCoord (mkCoord delta)) cit_col
	val t = "<Drag Canvas Item(" ^ (Int.toString x) ^ "," ^ 
	        (Int.toString y) ^ "," ^ (WidIdToString wid) ^ "," ^ 
                (CItemIdToString cid) ^ ") > " ^ 
		(Int.toString (!count))
	val _ = BasicUtil.inc count
    in
(*	(addConf mesCanFrId [Text t]; 
 *)
        setCItemCoords wid cid cit_col' 
(*
         moveCItem wid cid delta
*)
    end

and dropIt (wid:WidId) (cid:CItemId) (TkEvent(_,_,x,y,_,_)) =
    addConf wid [Cursor(XCursor("hand2",NONE))]

and leaveIt (wid:WidId) (cit:CItemId) (_:TkEvent) = 
    (addConf mesCanFrId [Text ("<Leave Canvas Item(" ^ (WidIdToString wid) ^ "," ^ 
                                (CItemIdToString cit) ^ ")>")];
     addConf wid [Cursor(NoCursor)])

and wrMotC (wid:WidId) (cid:CItemId) (TkEvent(_,_,x,y,_,_)) = 
    let
	val t = "<Motion Canvas Item(" ^ (Int.toString x) ^ "," ^ 
	        (Int.toString y) ^ "," ^ (WidIdToString wid) ^ "," ^ 
                (CItemIdToString cid) ^ ") > " ^ 
		(Int.toString (!count))
	val _ = BasicUtil.inc count
    in 
	addConf mesCanFrId [Text t]
    end
and wrEnt (_:TkEvent) = addConf mesCanFrId [Text "<Enter>"]
and wrLea (_:TkEvent) = addConf mesCanFrId [Text "<Leave>"]
and wrMot (_:TkEvent) = 
    let
	val t = "<Motion> " ^ (Int.toString (!count))
	val _ = BasicUtil.inc count
    in 
	addConf mesCanFrId [Text t]
    end;

fun quitButton quit = Button(mkWidgetId "quitButton",
			     [Side Left,Fill X,Expand true],
			     [Text "Quit",Command quit,
			      Cursor(
				FileCursor(getImgPath "myex.cursor",
				  Blue,
                                  SOME(getImgPath"myex.cursor_mask",
                                  Yellow)))],[])

fun quitter () = Frame(mkWidgetId "quitter",
		       [quitButton doQuit],
		       [Fill X],
		       [Relief Ridge,Borderwidth 2],[]);

fun initwin _ = [mkWindow{winId   = mainWinId,
		          config  = [WinTitle "Hider Example",
		                     WinSizeFrom Program,
	                             WinGeometry(NONE,SOME(50,50))], 
		          widgets = Pack [aLabel(),hider(),hider1(),daVinciStarter(),
                                          texter(), Entry(),Canvasfr(),quitter()], 
			  bindings = [],
		          init= noAction}];

val go = fn () => startTclExn (initwin ())


end;
