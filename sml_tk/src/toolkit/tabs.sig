(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/tabs.sig,v $

   SmlTk-Tabs signature file

   $Date: 2001/03/30 13:39:51 $
   $Revision: 3.0 $
   Author: ludi (Last modification by $Author: 2cxl $)

   (C) 2000, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)

signature TABSSIG =
    sig
	exception Error of string

	val tabs :
	    {pages     : {title    : string,                        (* title *)
			  widgets  : SmlTk.Widgets,          (* page content *)
			  show     : SmlTk.SimpleAction,
			  hide     : SmlTk.SimpleAction,
			  shortcut : int option} list,  (* nth char in title *)

	     (* 
	      * The show action is called when the widgets are allready
	      * displayed, so you can initialize them in there.
	      * The hide action is called just before the widgets are
	      * destroyed, so that you can save its content.
	      *)

	     configure :
	       {width       : int,            (* width of widget area       *)
		spare       : int,            (* space on the right         *)
		height      : int,            (* height of widget area      *)
		font        : SmlTk.Font,     (* font of card labels        *)
		labelheight : int}}           (* maximum height of font + x *)
	    -> SmlTk.Widget *      (* returned canvas widget *)
	       SmlTk.Binding list  (* shortcut bindings, you must bind these *)
	                           (* to the window containing the tabs      *)

	val std_conf : {width       : int,          (* see above *)
			spare       : int,
			height      : int,
			font        : SmlTk.Font,
			labelheight : int}

    end
