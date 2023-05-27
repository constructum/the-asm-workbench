(* ***************************************************************************

   $Source: /repository/sml/sml_tk/src/eventloop.sig,v $

   sml_tk event handler.

   This module implements the sml_tk event handling mechanism. It exports
   two functions, startTcl and startTclExn, which take a list of windows
   and start the GUI main application loop (where the Exn variant handles
   all exceptions which might occur). 

   $Date: 2001/03/30 13:39:10 $
   $Revision: 3.0 $

   Author: bu (Last modification $Author: 2cxl $)

   (C) 1996-99, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

signature EVENTLOOP =
    sig

	val startTcl    : BasicTypes.Window list -> unit
	val startTclExn : BasicTypes.Window list -> string

	(* Interrupt handling *)
	type intr_listener 	
	val registerIntrListener   : (unit-> unit)-> intr_listener
	val deregisterIntrListener : intr_listener-> unit

    end
