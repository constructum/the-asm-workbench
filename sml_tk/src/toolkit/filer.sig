(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/filer.sig,v $

   Filer signature file

   $Date: 2001/03/30 13:39:40 $
   $Revision: 3.0 $
   Author: ludi (Last modification by $Author: 2cxl $)

   (C) 1999, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)

(*--- Filer signature -------------------------------------------------------*)

signature FILER_SIG =
    sig

	(* critical errors -- e.g. can't open root directory *)
	exception Error of string

	(* stand alone version *)
	val stand_alone : unit -> (string option * string option) option

	(* system versions *)
	val file_select : (((string option * string option) option) -> unit) ->
	                  unit
	val enter_file  : unit -> unit

	(* set preferences *)
	val set : {sort_names          : bool option,
		   sort_types          : bool option,
		   show_hidden_files   : bool option,
		   hide_icons          : bool option,
		   hide_details        : bool option}  -> unit
    end


(*--- Filer configs signature -----------------------------------------------*)
(* Default configuration in structure FilerDefaultConfig *)

signature FILER_CONFIG =
    sig
	                                                      (* Parameters: *)
	val title              : string option

	val font               : SmlTk.Font        (* files-/foldersbox font *)
	val font_height        : int               (* font height in pixels  *)

	val foldersbox_width   : int                          (* boxes´ size *)
	val filesbox_numcols   : int (* no. of colums of icons in the filebox*)
	val filesbox_width     : int 
	(* filesbox_width must be at least filesbox_numcols x the width of
	 * a label label containing text with filenames_cut characters      *)
	val boxes_height       : int

	val foldernames_cut    : int                (* maximum length of     *)
	val filenames_cut      : int                (* foldernames/filenames *)

	val icon_font          : SmlTk.Font  (* hidden                       *)
	val icon_font_height   : int         (* maximum height of label      *)
                                             (* containing icon_font in      *)
					     (* pixels                       *)

	val preferences        : {sort_names          : bool,
				  sort_types          : bool,
				  show_hidden_files   : bool,
				  hide_icons          : bool, (* preferences *)
				  hide_details        : bool} (* on startup  *)
    end
