(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/filer_default_config.sml,v $

   Filer default configuration

   $Date: 2001/03/30 13:39:42 $
   $Revision: 3.0 $
   Author: ludi (Last modification by $Author: 2cxl $)

   (C) 1999, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)

(*--- filer default configuration -------------------------------------------*)

structure FilerDefaultConfig : FILER_CONFIG =
    struct

	(* comments on parameters: see signature file *)
	val title              = SOME "File selection"
	val font               = SmlTk.SansSerif [SmlTk.NormalSize]
	val font_height        = 20
	val foldersbox_width   = 250
	val filesbox_width     = 480 
	val filesbox_numcols   = 4
	val boxes_height       = 500
	val foldernames_cut    = 15
	val filenames_cut      = 20
	val icon_font          = SmlTk.SansSerif [SmlTk.Small]
	val icon_font_height   = 16
	val preferences        = {sort_names          = true,
				  sort_types          = true,
				  show_hidden_files   = false,
				  hide_icons          = false,
				  hide_details        = false}
    end
