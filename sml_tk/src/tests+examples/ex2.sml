
(* *********************************************************************** *)
(*									   *)
(* Project: sml/Tk: an Tk Toolkit for sml	 			   *)
(* Author: Burkhart Wolff, University of Bremen	 			   *)
(* Date: 25.7.95				 			   *)
(* Purpose of this file: Small example					   *)
(*									   *)
(* *********************************************************************** *)

structure Ex2 =
struct

local open SmlTk BasicUtil in

exception NoFile of string;


val warn = mkWinId "warning";
val main = mkWinId "main";
val enter = mkWinId "entername";
val e1 = mkWidgetId "e1";
val liste = mkWidgetId "liste";
val state_wid = mkWidgetId "statewid";

fun prs s = TextIO.output (TextIO.stdOut, s);
fun writeln s = prs (s ^ "\n");


(* Cursor Get Action on Listboxes and TextWidgets *)

fun getcur wid = fn (_)=> let val Mark(n,m) = readCursor wid
                          in  TextIO.output(TextIO.stdOut, "POSITION :" ^ Int.toString n ^
						"."^ Int.toString m ^"\n") 
                          end;


(* Warning Window *)

val nogoon   = mkSimpleAction(fn () => closeWindow warn);

fun nobut   msg yes = Button{widId=newWidgetId(),packings=[Side Right, Fill X, Expand true],
			     configs=[Text "NO",  Command  nogoon], bindings=[]};

fun Message1 msg yes = Label{widId=newWidgetId(), packings=[Fill X, Expand true],
			     configs=[Text msg, Relief Flat, Width 25], bindings=[]};
fun yesbut  msg yes = Button{widId=newWidgetId(),packings=[Side Left,  Fill X, Expand true],
			     configs=[Text "YES", Command yes],bindings=[]};
fun yesno   msg yes = Frame{widId=newWidgetId(), widgets=Pack [yesbut msg yes, nobut msg yes],
			    packings=[], configs=[], bindings=[]};

fun tree2   msg yes = [Message1 msg yes, yesno msg yes];


fun warnwin msg yes = mkWindow{winId=warn, 
			       config= [WinTitle "Warning",
					WinTransient (SOME main)], 
			       widgets= Pack(tree2 msg yes),
			       bindings = [],
			       init=noAction}

fun warning msg yes = openWindow (warnwin msg yes);



(* Enter Window *)

val nLabel   = Label{widId=newWidgetId(), packings=[Side Left], configs=[Text "name:"],
		     bindings=[]};

fun input enteraction = 
     let val mrs = fn (_) => let val nm = readTextAll e1
		            in  enteraction nm ();closeWindow enter end 
     in Entry{widId=e1, packings=[], configs=[Width 20],
	      bindings=[BindEv(KeyPress "Return", mrs)]} end

fun treesize enteraction = [nLabel,input enteraction];

fun enterwin enteraction = mkWindow{winId=enter, 
				    config=[WinTitle "Please enter name",
					    WinTransient (SOME main)], 
				    widgets=Pack(treesize enteraction),
				    bindings = [],
				    init=noAction}



(* Main Window *)

val yesquit  = mkSimpleAction(fn () => closeWindow main); 

val entername = 
    let fun inputok nm = fn () => insertTextEnd liste nm
        val cmd        = mkSimpleAction(fn () => openWindow (enterwin inputok))
     in MCommand [Text "Enter name", Command (cmd)] end;

val m1 = newWidgetId();
val insertmenue= Menubutton{widId=m1, mitems=[entername], packings=[Side Left],
		   	    configs=[Text "Special", Tearoff true], bindings=[]};

val list     = Listbox{widId=liste, scrolltype=RightScb,
			packings=[Side Left,Side Top,Fill Y],configs=[Relief Raised], 		
			bindings=[BindEv(Double(ButtonPress (SOME 1)), 
					 getcur liste)]}

val MARKLIST = ref([] : (Mark * Mark) list);

val statewid = 
    TextWid{widId=state_wid, scrolltype=RightScb, annotext=mtAT, packings=[Side Left,Fill Both],
	    configs=[Relief Raised, Borderwidth 2],
	    bindings=[BindEv(Double(ButtonPress (SOME 1)), getcur state_wid),
		      BindEv(Double(ButtonPress (SOME 3)),
			     fn _ => let val t = readSelWindow();
					 val m = readSelRange state_wid;
				     in
					 case t of
					     NONE => 
						 writeln("SEL")
					   | SOME (win,wid) => 
						 writeln(mkWinString(win)^ 
							 mkWidgetString(wid));
						 MARKLIST := m
				     end),
		      BindEv(Double(ButtonPress (SOME 2)),
			     fn (_) => TextIO.output(TextIO.stdOut,
						     readTextAll state_wid))]}

val yesreset = noAction;

val loadfile = 
     let fun more str = let val in_str =

            (TextIO.openIn str
            handle (OS.SysErr(err, _)) => raise (NoFile ("Can't open file "^str^": "^err)))
            handle IO.Io {name,...} => raise (NoFile name)  





    		        in  while not(TextIO.endOfStream in_str) do
			      insertTextEnd state_wid (TextIO.inputN (in_str,100))
    		        end;
         fun doit nm = fn () => (clearText state_wid;more nm);
     in  mkSimpleAction(fn () => (openWindow (enterwin doit)))
     end

val load     = MCommand[Text "Load", Command loadfile];

val quitgame = mkSimpleAction(fn () => warning "really quit?"  yesquit);

val quit     = MCommand([Text "Quit", Command quitgame]); 

val m2 = newWidgetId();
val ctrmenue = Menubutton{widId=m2, mitems=[load, MSeparator, quit],
			  packings=[Side Left], configs=[Text "Control", Tearoff true],bindings=[]};

val space    = Frame{widId=newWidgetId(), widgets=Pack [], packings=[Side Left, Fill Both],
		     configs=[], bindings=[]};
val menu     = Frame{widId=newWidgetId(), widgets=Pack [ctrmenue,insertmenue,space], packings=[],
		     configs=[], bindings=[]};

val board    = Frame{widId=newWidgetId(), widgets=Pack [statewid,list], packings=[Side Bottom,Fill Both],
		     configs=[], bindings=[]};

val tree1    = [menu,board];

val initwin  = [mkWindow{winId=main, 
			 config=[WinTitle "Transformation System"], 
			 widgets= Pack tree1,
			 bindings = [],
			 init=noAction}]



(* Launching and Shutting System *)
(*
openWindow (hd initwin); 

start_tcl initwin;
start_tcl_exn initwin;

Cursor-Position fuer TextWidgets:
.text index insert

Cursor-Position fuer Listboxes:
 bind .dateien <Double-Button-1> {
   puts [.dateien curselection]
}


*)

val doit = fn () => startTclExn initwin
val go = doit
end

end;
