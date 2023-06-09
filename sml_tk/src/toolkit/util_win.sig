(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/util_win.sig,v $
 
   Utitility windows -- signature file.
  
   Windows for errors, warnings, user confirmation and text entry,
   revised vision

   Supplements util_win.sml. 

   $Date: 2001/03/30 13:39:55 $
   $Revision: 3.0 $
   Author: cxl (Last modification by $Author: 2cxl $)

   (C) 1997-99, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

(* export signature *)
signature UTIL_WIN =
sig
    (* display error or warning, then continue *)    
    val error   : string -> unit
    val warning : string ->  unit

    (* display error or warning, then call continuation *)    
    val error_cc   : string* (unit-> unit)-> unit
    val warning_cc : string* (unit-> unit)-> unit

    (* Demand confirmation, then call continuation.
     * If user clicks "cancel", do nothing *)
    val confirm : string* (unit-> unit)->unit

    (* Display an informative message. The returned closure closes this window,
     * ideally after it has been displayed for at least 10 secs or something.
     * This window can't be closed by the user.
     *)
    val info_cc  : string-> (unit-> unit)
    (* as above, but let the user close the window ("display&forget") *)
    val info     : string-> unit
	
    (* Display a text
     *
     * There are two variations, one where the id of the text widget is
     * explicitly passed along (although the widget has not been created
     * at this point), one where it is created by this function and then
     * passed to the cc function.
     *)
    val display: {title: string, width: int, height: int,
		  text: SmlTk.AnnoText, cc: SmlTk.WidId-> unit}-> unit

    val display_id: {winId: SmlTk.WinId, widId: SmlTk.WidId, title: string,
		     width: int, height: int, text: SmlTk.AnnoText}-> unit

    (* prompt the user to enter a text in a separate window w/ a text widget
     *
     * parameters are pretty self-explanatory, except for cc which is
     * the continuation to be called with the entered text.
     *)

    val enterText : {title : string, prompt : string, default : string,
		     width : int, height : int, 
		     cc : string-> unit} -> unit


    (* prompt the user to enter a line of text in a separate windw 
     *
     * Parameters are as before (but no height). This function uses an 
     * artificially intelligent semi-heuristic fuzzy logic based algorithm 
     * implemented in Java to determine wether the text entry should be 
     * alongside the prompt or below it.
     *)
    val enterLine : {title : string, prompt : string, default : string, 
	             width : int, cc : string-> unit } -> unit

    (* (Actually, if the prompt is at least twice as long as the text entry,
     *  it is below the prompt, otherwise to its right). *)

    (* Auxiliary version of enterText which produces n entry widgets,
     * as specified by a list of heights, also takes some more widgets
     * and places them between the text widget, and the ok/cancel bottoms *)
    val enterText0 : {title : string, prompt : string, default : string,
		      widgetsbelow : SmlTk.Widget list, 
		      heights : int list, headers : string list, 
		      width : int, cc : string list -> unit} -> unit

end

(*

structure UW : UTIL_WIN = ?

*)
