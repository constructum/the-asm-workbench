(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/tests+examples/uw_ex.sml,v $
 
   Small demo for the utility windows.
 
   $Date: 2001/03/30 13:40:07 $
   $Revision: 3.0 $
   Author: cxl (Last modification by $Author: 2cxl $)

   (C) 1996, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

structure UW_Ex : sig val go : unit-> unit end = 

struct

open SmlTk

val mwi = newWinId()

(* Because of SML's linear visibility, we have to declare windows the opposite 
 * way they are going to appear. 
 *)

fun confq () = UW.confirm("Do you want to quit now?", fn()=> closeWindow mwi)

(* Information window. Note there are no modal information windows -- you'd
 * have to write them yourself using info_cc to bind the closing function
 * returned by info_cc to a button. *)
fun testi () = 
    (UW.info("All files have been deleted."); confq())

(* Confirm. No continuation for the Cancel option-- it just closes the window *)
fun testc () = 
    UW.confirm("Do you really want to delete all your files?", testi)

(* Modal warning window *)
fun testw () = UW.warning_cc("Your printer is on fire", testc)

(* Modal error window *)
fun teste () = let val go_on = fn x=> (print("The close button has been \
					     \clicked.\n"); testw x)
          in  UW.error_cc("File \"/home/cxl/rubbish\" \
			  \not found or not readable.", go_on);
              print "The window has just been opened.\n"
          end

(* Start button, and main window *)
val start_button = Button{widId=newWidgetId(), packings=[Side Top],
                         configs=[Text "Start", Command teste],
			 bindings=[]} 

val w = mkWindow{winId    = mwi,
		 config   = [WinTitle "Utility Window Test"], 
		 widgets  = Pack [start_button],
		 bindings = [],
		 init     = noAction}


(* ... and go! *)
fun go() = startTcl [w]

end




