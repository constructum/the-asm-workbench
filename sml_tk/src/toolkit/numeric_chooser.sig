(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/numeric_chooser.sig,v $

   Numeric choosers signature file

   $Date: 2001/03/30 13:39:47 $
   $Revision: 3.0 $
   Author: ludi (Last modification by $Author: 2cxl $)

   (C) 1999, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)

signature NUMERIC_CHOOSER_SIG =
    sig
	val numeric_chooser :
	      {initial_value      : int,
	       min                : int option,
	       max                : int option,
	       increment          : int,
	       width              : int,
	       orientation        : SmlTk.Orientation,
	       selection_notifier : int -> unit} ->
	      {chooser    : SmlTk.Widget,
	       set_value  : int -> unit,
	       read_value : unit -> int}
    end
