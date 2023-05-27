
(* *********************************************************************** *)
(*									   *)
(* Project: sml/Tk: an Tk Toolkit for sml	 			   *)
(* Author: Burkhart Wolff, University of Bremen	 			   *)
(* Date: 25.7.95				 			   *)
(* Purpose of this file: Small example					   *)
(*									   *)
(* *********************************************************************** *)

structure Ex1 =
struct

local open SmlTk in

val main = mkWinId "main";
val warn = mkWinId "warning";
val e1   = mkWidgetId "e1"
val Entername = mkWinId "entername"
val menu = mkWidgetId "menu"

val yesquit  = mkSimpleAction(fn () => closeWindow main); 
val nogoon   = mkSimpleAction(fn () => closeWindow warn);


(* Warning Window *)
fun nobut   msg yes no = Button{widId=newWidgetId(), 
                                packings=[Side Right, Fill X, Expand true],
			        configs=[Text "NO", Command  no], 
                                bindings=[]};

fun Message1 msg yes no= Label {widId=newWidgetId(), 
                                packings=[Fill X, Expand true],
				configs=[Text msg, Relief Flat, 
                                         Width 25], 
                                bindings=[]};

fun yesbut  msg yes no = Button{widId=newWidgetId(), 
                                packings=[Side Left, Fill X, Expand true],
				configs=[Text "YES", Command yes], 
                                bindings=[]};

fun yesno   msg yes no = Frame {widId=newWidgetId(),
                                widgets=Pack [yesbut msg yes no, 
                                              nobut msg yes no],
			        packings=[], configs=[], bindings=[]};

fun tree2   msg yes no = [Message1 msg yes no, yesno msg yes no];

fun warnwin msg yes no = mkWindow{winId=warn, 
				  config=[WinTitle "Warning", 
					  WinTransient (SOME main)
				  (* , WinOverride true *)], 
				  widgets=Pack(tree2 msg yes no),
				  bindings = [],
				  init=noAction}

fun warning msg yes no = openWindow (warnwin msg yes no);



(* Enter Window *)
val inputok  = fn () => let val nm = mkTitle(readTextAll e1) 
                        in changeTitle main nm ;
                           closeWindow Entername 
                        end;

val nLabel   = Label{widId=newWidgetId(), 
                     packings=[Side Left], 
                     configs=[Text "name:"], 
                     bindings=[]};

val input    = Entry{widId=e1, packings=[], configs=[Width 20],
                     bindings=[BindEv(KeyPress "Return", 
                               fn _=> inputok())]};

val treesize = [nLabel,input];

val enterwin = mkWindow{winId=Entername, 
			config=[WinTitle "Please enter name",
				WinTransient (SOME main)], 
			widgets=Pack treesize,
			bindings = [],
			init=noAction};

val playername= mkSimpleAction(fn () => openWindow enterwin);

(* Main Window *)

val entername= MCommand [Text "Enter name", Command playername];


val player   = Menubutton{widId=newWidgetId(), 
                          mitems=[entername], packings=[],
			  configs=[Text "Special", MUnderline 0, Tearoff true], 
                          bindings=[]};

val yesreset = noAction;

fun newgame()= warning "really reset?" yesreset nogoon;

val new      = MCommand	  [Text "New", 
                           MUnderline 0, Accelerator "Ctrl+n",
                           Command (mkSimpleAction newgame)];

fun quitgame()= warning "really quit?" yesquit nogoon;

val quit     = MCommand([Text "Quit",  
                         MUnderline 0, Accelerator "Ctrl+q",
                         Command (mkSimpleAction quitgame)]); 


val game     = Menubutton{widId=newWidgetId(), 
                          mitems=[new, MSeparator, quit],
			  packings=[Side Left], 
                          configs=[Text "Control", MUnderline 0, Tearoff true],
                          bindings=[]}

val menu     = Frame{widId=newWidgetId(),widgets=Pack [game, player],
                     packings=[],configs=[],bindings=[]}

val board    = Frame{widId=newWidgetId(), 
                     packings=[Side Left, Fill X],widgets = Pack [],
		     configs=[Width 200,Height 200],
                     bindings=[BindEv(KeyPress "q",
                                      fn _ => quitgame()),
                               BindEv(Ctrl(KeyPress "n"),
                                      fn _ => newgame())]}

val initwin  = [mkWindow{winId=main, 
                         config=[WinTitle "Mini Example"], 
                         widgets=Pack [menu,board],
			 bindings = [],
			 init=noAction}]


val doit = fn () => startTclExn initwin
val go =  doit


end (* local open *)

end

