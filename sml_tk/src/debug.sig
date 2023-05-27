(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/debug.sig,v $
 
   Debugging.
 
   $Date: 2001/03/30 13:39:09 $
   $Revision: 3.0 $
   Author: Stefan Westmeier (Last modification by $Author: 2cxl $)

   (C) 1996, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

(* This module provides three output routines, errors, warnings and debug
 * messages. Warnings are printed to std_err with a leading "Warning: ", but
 * can be turned off wholesale; errors are printed with a leading "ERROR: " 
 * and can't be turned off.
 *
 * Debug messages come with a number, the "debuglevel". Each debuglevel can 
 * be turned on or off separately, allowing the selective debugging of
 * specific sections of sml_tk and/or modules built with sml_tk.
 *
 * When debugging a new module, use a number which is not used with other 
 * modules yet. 
 *
 * Here's a rough guide to the debuglevels already in use within sml_tk:
 * 1 - Eventloop (eventloop.sml, com.sml)
 * 2 - Widgets (widget_tree.sml), widget_ops.sml)
 * 3 - Canvas Items (c_item.sml)
 * 4 - Annotations (annotation.sml)
 * 5 - Fonts (fonts.sml)
 * 
 * 10 - Clipboard (toolkit/clipboard.sml)
 * 11 - GenGUI and D&D (toolkit/gen_gui.sml and toolkit/drag_and_drop.sml)
 * 12 - Filer (toolkit/filer.sml)
 * 13 - DGenGUI and the dag package (toolkit/{dgen_gui.sml,dag.sml})
 * 19 - Toolkit examples.
 *
 * Zero, as an argument to `on' or `off', turns all messages on or off (but
 * leaves the warnings alone).
 *)

signature DEBUG =
    sig
	val on      : int list-> unit
        val off     : int list-> unit

	val print   : int-> string -> unit

	val warning  : string-> unit
	val warn_off : unit-> unit
	val warn_on  : unit-> unit
	    
	val error    : string-> unit
    end
