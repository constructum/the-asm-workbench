(*
 *
 *  Project: sml/Tk: an Tk Toolkit for sml
 *  Author: Burkhart Wolff, University of Bremen
 * $Date: 2001/03/30 13:39:33 $
 * $Revision: 3.0 $
 *  Purpose of this file: PopUp example
 *
 *)

structure PopUpEx :>
    sig val go : unit -> string end =
struct

open SmlTk

val warn = mkWinId "warning"

(* Warning Window *)
fun warnwin msg yes no = 
   mkWindow{winId=warn, 
	    config=[WinTitle "Warning"], 
	    widgets=
               Pack [Label{widId=newWidgetId(),
			   packings=[Fill X, Expand true],
			   configs=[Text msg, Relief Flat, Width 25],
			   bindings=[]},
		     Frame{widId=newWidgetId(),
			   widgets=
                              Pack [Button{widId=newWidgetId(),
					   packings=[Side Left,  Fill X, Expand true],
					   configs=[Text "YES", Command yes],
					   bindings=[]},
				    Button{widId=newWidgetId(),
					   packings=[Side Right, Fill X, Expand true],
					   configs=[Text "NO",  Command  no],
					   bindings=[]}
				    ],
			   packings=[],
			   configs=[],
			   bindings=[]}],
            bindings = [],
	    init= noAction};

fun warning msg yes no = openWindow (warnwin msg yes no);



(* Enter Window *)

val mainWinId = mkWinId "meisterfenster"
val entern = mkWinId "entername"
val p1 = mkWidgetId "p1"
val p2 = mkWidgetId "p2"
val c1 = mkWidgetId "c1"
val it1 = mkCItemId "it1"
val it2 = mkCItemId "it2"
val e1 = mkWidgetId "e1"
val itemmenu = mkWidgetId "itemmenu"


val enterwin = 
    let 
	val inputok  = fn () => (changeTitle mainWinId (mkTitle(readTextAll e1));
				 closeWindow entern )
    in
	mkWindow{winId=  entern, 
		 config= [WinTitle "Please enter name"], 
		 widgets=
                    Pack [Label{widId=newWidgetId(),packings=[Side Left],
				configs=[Text "name:"],bindings=[]},
			  Entry{widId=e1,packings=[],configs=[Width 20],
				bindings=[BindEv(KeyPress "Return",
						 fn (_:TkEvent) => inputok())]}],
                 bindings = [],
		 init= noAction}
    end

val playername= fn () => openWindow enterwin;

(* Main Window *)

val entername= MCommand [Text "Enter name", Command playername];

val menu     = 
    let
	val nogoon   = mkSimpleAction(fn () => closeWindow warn)
	val yesquit  = mkSimpleAction(fn () => closeWindow mainWinId )
	val yesreset = mkSimpleAction(fn () => ())
	val newgame  = mkSimpleAction(fn() => warning "really reset?" yesreset nogoon)
	val quitgame = mkSimpleAction(fn () => warning "really quit?"  yesquit  nogoon)
    in
	Frame{widId=newWidgetId(),
	      widgets=
         Pack [Menubutton{widId=newWidgetId(),
			  mitems=[MCommand([Text "New",Command newgame]),
			   MSeparator,
			   MCommand([Text "Quit",Command quitgame])
			   ],
			  packings=[Side Left],
			  configs=[Text "File", Tearoff true],
			  bindings=[]}, 
	       Menubutton{widId=newWidgetId(),
			  mitems=[MCommand([Text "Enter name", Command playername])
			   ],
			  packings=[Side Left],
			  configs=[Text "Special", Tearoff true],
			  bindings=[]},
	       Menubutton{widId=itemmenu,
			  mitems=[MCommand([Text "Add",Command playername]),
			   MCommand([Text "Delete",Command (mkSimpleAction((fn () => ())))])
			   ],
			  packings=[Side Left],
			  configs=[Text "Item", Tearoff true],
			  bindings=[]}
	       ],
	      packings=[Fill X],
	      configs=[Relief Raised,Borderwidth 2],bindings=[]}
    end

fun Popup1 wid =
    Popup{widId   = wid,
	  mitems  = [MCommand([Text "Add",
			       Command (mkSimpleAction (fn () => ())) ]),
		     MCommand([Text "Delete",
			       Command (mkSimpleAction(fn () => ()))]),
		     MSeparator,
		     MCommand([Text "Properties",Command playername])],
	  configs = []}

val board    = 
    let
	val pos = ref(0:int,0:int)

	fun grabIt (wid:WidId) (cid:CItemId) (TkEvent(_,_,x,y,_,_)) =
	    pos := (x,y)

	fun moveIt (wid:WidId) (cid:CItemId) (TkEvent(_,_,x,y,_,_)) = 
	    let
		val cit_col  = readCItemCoords wid cid
		val (xP,yP)  = !pos
		val _        = (pos:=(x,y))
		val delta    = mkCoord (x-xP,y-yP)
	    in
		moveCItem wid cid delta
	    end

	fun popitupM (TkEvent(_,_,_,_,xr,yr)) = 
	    popUpMenu itemmenu NONE (mkCoord (xr,yr))
	fun popitupP (TkEvent(_,_,_,_,xr,yr)) = 
	    popUpMenu p1 (SOME 4) (mkCoord (xr,yr))
	fun popitupD (TkEvent(_,_,_,_,xr,yr)) = 
(*	    WidgetOps.createAndPopUpMenu (Popup1 p2) (SOME 4) (xr,yr) *)
	    createAndPopUpMenu (Popup1 p2) (SOME 4) (mkCoord (xr,yr))
	fun popitdownD _ =
	    delWidget p2
	fun itbds (wid:WidId) (cid:CItemId) = 
	    [BindEv(ButtonPress (SOME 2),mkAction(popitupM)),
	     BindEv(Shift(ButtonPress (SOME 3)),mkAction(popitupD)),
	     BindEv(Shift(ButtonRelease (SOME 3)),mkAction(popitdownD)),
	     BindEv(ButtonPress (SOME 3),mkAction(popitupP)),
	     BindEv(ButtonPress (SOME 1),mkAction(grabIt wid cid)),
	     BindEv(ModButton(1, Motion),mkAction(moveIt wid cid))
	     ]
    in
	Frame{widId=newWidgetId(),
	      widgets=
                 Pack [Canvas{widId=c1,scrolltype=LeftBotScb,
		       citems=[COval{citemId=it1, coord1=mkCoord (50,50), 
                                     coord2=mkCoord (100,100), 
                                     configs=[FillColor Red], 
                                     bindings=itbds c1 it1},
		               COval{citemId=it2,coord1=mkCoord (100,100),
                                     coord2=mkCoord (150,150),
			             configs=[FillColor Red],
                                     bindings=itbds c1 it2}
		       ],
		      packings=[],
		      configs=[ScrollRegion(0,0,400,400)],
		      bindings=[]}
	       ],
	      packings=[Side Left, Fill X],
	      configs=[Width 200,Height 200,Relief Raised,Borderwidth 2], 
	      bindings=[]}
    end

val area    = [menu,board,(Popup1 p1)]

val initwin  = [mkWindow{winId=  mainWinId, 
			 config= [WinTitle "Popup Example"], 
			 widgets= Pack area,
                         bindings = [],
			 init= noAction}]


val go = fn () => startTclExn initwin


end;

