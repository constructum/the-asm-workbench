(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/sys_dep.sig,v $

   System dependent functions (ie. depending in the SML used)

   This signature has one implementation each for the different SMLs used. 
   Currently, there are
   - njml.sml (loaded from sources.cm) for SML/NJ 110.
     (* bu: sml 109 no longer supported *)

   $Date: 2001/03/30 13:39:19 $
   $Revision: 3.0 $
   Author: stefan (Last modification by $Author: 2cxl $)

   (C) 1996, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

signature SYS_DEP =
    sig	

	(* Export an ML image *)
	val exportML : { init: unit-> unit, 
			 banner : string,
			 imagefile: string } -> unit

	val setPrintDepth : int -> unit

	(* Initialize TTY handler for sml_tk, and reset to previous state.
	 * This sets sigINT to be ignored (so it can be used to interrupt
         * diverging event handlers, see below), and sets up sigQUIT 
	 *)
	val initTTY  : (unit-> unit)-> unit	   
	val resetTTY : unit-> unit

        (* wrap an interrupt handler around a function f, s.t. sigINT (i.e.
         * CTRL-C) aborts the funtion. The second argument is a function 
	 * which is called when an interrupt occurs.
	 *)
	val interruptable : ('a-> unit)-> (unit-> unit) -> 'a -> unit

        (* This shouldn't be here if everybody would just implemented all 
	 * of the basis library according to the spec but there you go *)
	val exec    : string* string list-> bool

    end







