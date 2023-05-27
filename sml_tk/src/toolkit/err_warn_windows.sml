(* *********************************************************************** *)
(*									   *)
(* Project: sml/Tk: an Tk Toolkit for sml	 			   *)
(* Authors: Burkhart Wolff, Kolyang, University of Bremen		   *)
(* Date: 25.9.95				 			   *)
(* Purpose of this file: General Error- and Warning Windows		   *)
(*									   *)
(* *********************************************************************** *)

structure ErrWarnWindows:
sig 
    val errorwin        : string -> BasicTypes.SimpleAction -> 
	                  BasicTypes.Window
    val error		: string -> BasicTypes.SimpleAction -> unit
    val errs_occurred1 	: string -> unit -> unit
    val errs_occurred	: unit -> unit 
    val warning 	: string -> BasicTypes.SimpleAction -> unit
    val warnwin		: string -> BasicTypes.SimpleAction -> 
	                  BasicTypes.Window
    val this_warns 	: string -> unit -> unit 
end=
struct

local open BasicTypes in
local open SmlTk in

fun noaction () = ();

fun contbut quit = Button(newWidgetId(),[Side Left,  Fill X, Expand true],
					[Text "Continue", Command quit],[]);
fun message msg = Label(newWidgetId(),[Fill X, Expand true],
				[Text msg, Relief Flat, Width 50],[]);

fun tree2 msg quit = [message msg, contbut quit];

fun errorwin msg quit = ("error", [WinTitle "Error"], tree2 msg quit, noaction);

fun error msg yes = openWindow (errorwin msg yes);

fun errs_occurred ()= 
    let val errorquit  = fn () => closeWindow "error"
    in error "Errors occurred" errorquit end;
fun errs_occurred1 nm ()=(let val errorquit  = fn () => closeWindow "error"
    		    	  in error nm errorquit end);



val nogoon   = fn () => closeWindow "warning";

fun nobut   msg yes = Button(newWidgetId(),[Side Right, Fill X, Expand true],
			[Text "No",  Command  nogoon],[]);

fun message msg yes = Label(newWidgetId(),[Fill X, Expand true],
				[Text msg, Relief Flat, Width 25],[]);
fun yesbut  msg yes = Button(newWidgetId(),[Side Left,  Fill X, Expand true],
					[Text "Yes", Command yes],[]);
fun yesno   msg yes = Frame(newWidgetId(),[yesbut msg yes, nobut msg yes],
				[],[],[]);

fun tree2   msg yes = [message msg yes, yesno msg yes];


fun warnwin msg yes = ("warning", [WinTitle "Warning"], tree2 msg yes, noaction);

fun warning msg yes = openWindow (warnwin msg yes);

fun this_warns nm ()= (let val yesWarns = fn () => closeWindow "warning"
    		    in warning nm  yesWarns end) ; 

end; (* local open SML_TK *)
end; (* local open BasicTypes *)

end;
