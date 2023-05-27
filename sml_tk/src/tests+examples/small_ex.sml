(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/tests+examples/small_ex.sml,v $
 
   The small running example from the documentation. 
 
   $Date: 2001/03/30 13:39:34 $
   $Revision: 3.0 $
   Author: bu/cxl (Last modification by $Author: 2cxl $)

   (C) 1996, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

(*
 * This small example opens a window with a Label, a text Entry box and
 * a quit Button. (See the documentation in ../doc/DOC.html.)
 *)


let

(* open sml_tk: *)

open SmlTk



(* Get names for named objects: *)

val mainID  = newWinId()
val entID   = newWidgetId()

(* Define the widgets: *)
val lab   = Label{widId=newWidgetId(),
		  packings=[Side Left],
		  configs=[Text "name:"], 
		  bindings=[]}

val input   = let fun endInput _ = changeTitle mainID (mkTitle(readTextAll entID))
              in  Entry{widId=entID, packings=[], configs=[Width 20], 
			bindings=[BindEv(KeyPress "Return", endInput)]}
              end

val quit    = let fun stop _ = closeWindow mainID
	      in Button{widId=newWidgetId(), packings=[Side Bottom],
                        configs=[Text "Quit", Command stop], bindings=[]} 
	      end

(* Group together the text Entry and the Label *)
val topblock = Frame{widId=newWidgetId(), widgets=Pack [lab, input], 
		     packings=[Side Top], configs=[], bindings=[]}

(* Define the main window *)
val enterwin = {winId    = mainID,
                config   = [WinTitle "Please enter name"], 
		widgets  = Pack [topblock, quit],
                bindings = [],
                init     = noAction}

in

(* ... and go! *)
startTcl [mkWindow enterwin]

end



