(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/table.sig,v $

   SmlTk-Tables signature file

   $Date: 2001/03/30 13:39:50 $
   $Revision: 3.0 $
   Author: ludi (Last modification by $Author: 2cxl $)

   (C) 1999, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)

signature TABLE_SIG =
    sig
	val table :
	    {constant_column_width : bool,
	     headline_relief       : SmlTk.RelKind,
	     headline_borderwidth  : int,
	     headline_foreground   : SmlTk.Color,
	     headline_background   : SmlTk.Color,
	     field_relief          : SmlTk.RelKind,
	     field_borderwidth     : int,
	     field_foreground      : SmlTk.Color,
	     field_background      : SmlTk.Color,
	     container_background  : SmlTk.Color} ->
	     SmlTk.AnnoText list list ->
	     SmlTk.Widget

	val std_conf : {constant_column_width : bool,
			headline_relief       : SmlTk.RelKind,
			headline_borderwidth  : int,
			headline_foreground   : SmlTk.Color,
			headline_background   : SmlTk.Color,
			field_relief          : SmlTk.RelKind,
			field_borderwidth     : int,
			field_foreground      : SmlTk.Color,
			field_background      : SmlTk.Color,
			container_background  : SmlTk.Color}
    end
