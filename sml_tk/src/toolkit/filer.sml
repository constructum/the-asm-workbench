(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/filer.sml,v $

   Generic filer with clipboard support (structure Filer), including a
   partial instantiation for use without clipboard (structure SimpleFiler)

   $Date: 2001/03/30 13:39:40 $
   $Revision: 3.0 $
   Author: ludi (Last modification by $Author: 2cxl $)

   (C) 1999, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)

functor Filer(structure Options :
		  sig
		      val icons_path : unit -> string (* path to find icons  *)

		      val icons_size : int * int      (* width * height      *)
                                                      (* of label containing *)
						      (* an icon             *)

		      val root : unit -> string option     (* root directory *)

		      val default_pattern : string option (* default         *)
		                                          (* filtering of    *)
		                                          (* displayed files *)

		      structure CB : CLIPBOARD_W  (* clipboard instantiation *)

		      val filetypes :                     (* known filetypes *)
			  {ext     : string list,
			   display :
			     {comment     : string,
			      icon        : string,
			      preview     : ({dir  : string,
					      file : string} -> unit) option,
			      file_to_obj : ({dir  : string,
					      file : string} -> CB.obj)
			                    option}
			     option} list

		      structure Conf : FILER_CONFIG  (* other configurations *)

		  end) : FILER_SIG =
    struct
	open SmlTk


(*--- basic declarations ----------------------------------------------------*)

	exception Error of string

	type Preferences = {sort_names          : bool,
			    sort_types          : bool,
			    show_hidden_files   : bool,
			    hide_icons          : bool,
			    hide_details        : bool}

	type File = {dir  : string,
		     file : string}

	type DisplayType = {comment     : string,
			    icon        : string,
			    preview     : (File -> unit) option,
			    file_to_obj : (File -> Options.CB.obj)
			                  option} option

	type Filetype = {ext     : string list,
			 display : DisplayType}

	val file_select_winID = newWinId()

	val dir_labelID       = newWidgetId()
	val patternID         = newWidgetId()
	val toolbarID         = newWidgetId()
	val permissionsID     = newWidgetId()
	val foldersboxID      = newWidgetId()
	val foldersboxframeID = newWidgetId()
	val filesboxID        = newWidgetId()
	val filesboxframeID   = newWidgetId()
	val file_entryID      = newWidgetId()
	val fold_statusID     = newWidgetId()
	val file_statusID     = newWidgetId()
	val updirID           = newWidgetId()
	val backID            = newWidgetId()
	val forwardID         = newWidgetId()
(*	val homedirID         = newWidgetId()*)
	val reloadID          = newWidgetId()
(*	val mkDirID           = newWidgetId()*)
	val filedelID         = newWidgetId()

	val CURRENT_DIRECTORY = ref ""
	val CHOSEN_FILE       = ref NONE : string option ref
	val SORT_NAMES        = ref(#sort_names Options.Conf.preferences)
	val SORT_TYPES        = ref(#sort_types Options.Conf.preferences)
	val SHOW_HIDDEN       = ref(#show_hidden_files
				      Options.Conf.preferences)
	val HIDE_ICONS        = ref(#hide_icons Options.Conf.preferences)
	val HIDE_DETAILS      = ref(#hide_details Options.Conf.preferences)
	val UPDIR_ACTIVE      = ref false
	val INSIDE_UPDIR      = ref false
	val BACK_ACTIVE       = ref false
	val INSIDE_BACK       = ref false
	val FORWARD_ACTIVE    = ref false
	val INSIDE_FORWARD    = ref false
	val MKDIR_ACTIVE      = ref false
	val RELOAD_ACTIVE     = ref false
	val FILEDEL_ACTIVE    = ref false
	val ENTER_FILE        = ref false
	val SELECTED          = ref NONE : WidId option ref
	val EXIT_STATUS       = ref false

	val dummy_event = TkEvent(0, "", 0, 0, 0, 0)

	fun root_dir() =
	    if isSome(Options.root()) then valOf(Options.root()) else "/"

	fun max_comment_length() =
	    let
		fun seek_maxl ((f : Filetype) :: fs) l =
		    (case #display f of
			 SOME {comment,...} =>
			     seek_maxl fs (Int.max(size comment, l))
		       | NONE               => seek_maxl fs l)
		  | seek_maxl _ l                      = l
	    in
		seek_maxl Options.filetypes 0
	    end

(*--- useful functions ------------------------------------------------------*)

	fun sort (f :: fs) ord =
	    sort (List.filter (not o ord f) fs) ord @ [f] @
	    sort (List.filter (ord f) fs) ord
	  | sort [] _          = []

	fun shortleft a b =
	    if size a > b then
		".." ^ implode(List.drop(explode a, size a - b + 2))
	    else a

	fun shortright a b =
	    if size a > b then
		implode(List.take(explode a, b - 2)) ^ ".."
	    else a

	fun sub_dir p1 p2 =
	    let
		fun to_list' "/" = []
		  | to_list' p   =
		    OS.Path.file p :: to_list'(OS.Path.dir p)

		fun to_list p = rev(to_list' p)

		fun sub_dir' (x::xs) (y::ys) =
		    if not(x = y) then false else sub_dir' xs ys
		  | sub_dir' _ []            = true
		  | sub_dir' [] _            = false
	    in
		sub_dir' (to_list p1) (to_list p2)
	    end

	fun ext nm = valOf(OS.Path.ext nm) handle _ => ""

	fun busy() =
	    (addConf filesboxframeID [Cursor(XCursor("watch", NONE))];
	     addConf foldersboxframeID [Cursor(XCursor("watch", NONE))])

	fun ready() =
	    (addConf filesboxframeID [Cursor(XCursor("left_ptr", NONE))];
	     addConf foldersboxframeID [Cursor(XCursor("left_ptr", NONE))])


(*--- icons -----------------------------------------------------------------*)

	fun system_icons_path() = OS.Path.concat(getLibPath(), "icons/filer")

	fun noacc_fold_icon() =
	    FileImage(OS.Path.joinDirFile {dir  = system_icons_path(),
					   file = "noacc_Icon.gif"},
		      newImageId())

	fun acc_fold_icon()   =
	    FileImage(OS.Path.joinDirFile {dir  = system_icons_path(),
					   file = "acc_Icon.gif"},
		      newImageId())

	fun open_fold_icon()  =
	    FileImage(OS.Path.joinDirFile {dir  = system_icons_path(),
					   file = "open_Icon.gif"},
		      newImageId())


	fun updir_Icon() =
	    FileImage(OS.Path.joinDirFile {dir  = system_icons_path(),
					   file = "updir_Icon.gif"},
		      newImageId())

	fun updir_highlighted_Icon() =
	    FileImage(OS.Path.joinDirFile
		        {dir  = system_icons_path(),
			 file = "updir_highlighted_Icon.gif"},
		      newImageId())

	fun updir_outlined_Icon() =
	    FileImage(OS.Path.joinDirFile {dir  = system_icons_path(),
					   file = "updir_outlined_Icon.gif"},
		      newImageId())

	fun back_Icon() =
	    FileImage(OS.Path.joinDirFile {dir  = system_icons_path(),
					   file =  "back_Icon.gif"},
		      newImageId())

	fun back_highlighted_Icon() =
	    FileImage(OS.Path.joinDirFile {dir  = system_icons_path(),
					   file = "back_highlighted_Icon.gif"},
		      newImageId())

	fun back_outlined_Icon() =
	    FileImage(OS.Path.joinDirFile {dir  = system_icons_path(),
					   file = "back_outlined_Icon.gif"},
		      newImageId())

	fun forward_Icon() =
	    FileImage(OS.Path.joinDirFile {dir  = system_icons_path(),
					   file = "forward_Icon.gif"},
		      newImageId())

	fun forward_highlighted_Icon() =
	    FileImage(OS.Path.joinDirFile
		        {dir  = system_icons_path(),
			 file = "forward_highlighted_Icon.gif"},
		      newImageId())

	fun forward_outlined_Icon() =
	    FileImage(OS.Path.joinDirFile {dir  = system_icons_path(),
					   file = "forward_outlined_Icon.gif"},
		      newImageId())
(*
	fun homedir_Icon() =
	    FileImage(OS.Path.joinDirFile {dir  = system_icons_path(),
					   file = "homedir_Icon.gif"},
		      newImageId())

	fun homedir_highlighted_Icon() =
	    FileImage(OS.Path.joinDirFile
		        {dir  = system_icons_path(),
			 file = "homedir_highlighted_Icon.gif"},
		      newImageId())

	fun homedir_outlined_Icon() =
	    FileImage(OS.Path.joinDirFile {dir  = system_icons_path(),
					   file = "homedir_outlined_Icon.gif"},
		      newImageId())
*)
	fun reload_Icon() =
	    FileImage(OS.Path.joinDirFile {dir  = system_icons_path(),
					   file ="reload_Icon.gif"},
		      newImageId())

	fun reload_highlighted_Icon() =
	    FileImage(OS.Path.joinDirFile
		        {dir  = system_icons_path(),
			 file = "reload_highlighted_Icon.gif"},
		      newImageId())

	fun reload_outlined_Icon() =
	    FileImage(OS.Path.joinDirFile
		        {dir  = system_icons_path(),
			 file = "reload_outlined_Icon.gif"},
		      newImageId())
(*
	fun mkDir_Icon() =
	    FileImage(OS.Path.joinDirFile {dir  = system_icons_path(),
					   file = "mkDir_Icon.gif"},
		      newImageId())

	fun mkDir_highlighted_Icon() =
	    FileImage(OS.Path.joinDirFile
		        {dir  = system_icons_path(),
			 file = "mkDir_highlighted_Icon.gif"},
		      newImageId())

	fun mkDir_outlined_Icon() =
	    FileImage(OS.Path.joinDirFile {dir  = system_icons_path(),
					   file = "mkDir_outlined_Icon.gif"},
		      newImageId())
*)
	fun filedel_Icon() =
	    FileImage(OS.Path.joinDirFile {dir  = system_icons_path(),
					   file = "filedel_Icon.gif"},
		      newImageId())

	fun filedel_highlighted_Icon() =
	    FileImage(OS.Path.joinDirFile
		        {dir  = system_icons_path(),
			 file = "filedel_highlighted_Icon.gif"},
		      newImageId())

	fun filedel_outlined_Icon() =
	    FileImage(OS.Path.joinDirFile
		        {dir  = system_icons_path(),
			 file = "filedel_outlined_Icon.gif"},
		      newImageId())

	fun unknown_Icon() =
	    FileImage(OS.Path.joinDirFile {dir  = system_icons_path(),
					   file = "unknown_Icon.gif"},
		      newImageId())

	val DEFAULT_TYPE = ref (SOME {comment     = "Unknown filetype!",
				      icon        = "",
				      preview     = NONE,
				      file_to_obj = NONE} : DisplayType)


(*--- LazyTree instantiation ------------------------------------------------*)

	structure Obj (*: LAZY_TREE_OBJECTS *) =
	    struct
		datatype obj =
		    Leaf of string * string * IconKind * IconKind
		  | Node of string * string * IconKind * IconKind

		fun read_fo path =
		    let
			val dirstream = OS.FileSys.openDir path

			fun read ""  = []
			  | read new =
			    if (OS.FileSys.isDir(OS.Path.concat(path, new))
				handle NoAcc => false) then
				if (!SHOW_HIDDEN orelse
				    not(hd(explode new) = #".")) then
				    new :: read (valOf (OS.FileSys.readDir
				    			dirstream))
				else read (valOf (OS.FileSys.readDir dirstream))
			    else read (valOf (OS.FileSys.readDir dirstream))
		    in
			(sort (read (valOf (OS.FileSys.readDir dirstream)))
			      (fn x => fn y => String.<(x, y)) before
			 OS.FileSys.closeDir dirstream)
		    end

		fun children (Node(nm, path, _, _)) =
		     let
			 fun mk_obj nm =
			     let
				 val newpath =
				     OS.Path.concat(path, nm) handle _ => "/"

				 val b = (OS.FileSys.access
					    (newpath, [OS.FileSys.A_EXEC,
						       OS.FileSys.A_READ]))

				 val objdef =
				     (shortright nm
				        Options.Conf.foldernames_cut,
				      newpath,
				      if b then acc_fold_icon()
				      else noacc_fold_icon(),
				      if b then open_fold_icon()
				      else noacc_fold_icon())
			     in
				 if null(read_fo newpath) handle _ => true then
				     Leaf objdef
				 else Node objdef
			     end
		     in
			 map mk_obj (read_fo path)
		     end

		fun is_leaf (Leaf _) = true
		  | is_leaf _        = false

		fun sel_name (Leaf(nm, _, _, _)) = nm
		  | sel_name (Node(nm, _, _, _)) = nm

		fun icon (Leaf(_, _, ic, _)) = ic
		  | icon (Node(_, _, ic, _)) = ic

		fun selected_icon (Leaf(_, _, _, ic)) = ic
		  | selected_icon (Node(_, _, _, ic)) = ic

	    end (* structure Obj *)

	structure Tree = LazyTree(structure Obj = Obj)

	fun sel_path (Obj.Leaf(_, p, _, _)) = p
	  | sel_path (Obj.Node(_, p, _, _)) = p


(*--- make directory --------------------------------------------------------*)

(*	fun make_dir _ = UW.warning "Not yet implemented!"*)


(*--- toolbar icon management / actions -------------------------------------*)

	val UP       = ref noAction (* unschön !!! *)
	val BACK     = ref noAction
	val FORWARD  = ref noAction
	val POSITION = ref(fn () => Tree.hist_empty)

	fun updirentered _ =
	    (if !UPDIR_ACTIVE then
		 (setConf updirID [Icon(updir_highlighted_Icon())];
		  setBind updirID [BindEv(Leave, updirleft),
				   BindEv(ButtonPress(SOME 1),
					  fn _ => (!UP)())])
	     else ();
	     INSIDE_UPDIR := true)

	and updirleft _ =
	    (if !UPDIR_ACTIVE then
		 (setConf updirID [Icon(updir_Icon())];
		  setBind updirID [BindEv(Enter, updirentered)])
	     else ();
	     INSIDE_UPDIR := false)

	and disable_updir() =
	    if !UPDIR_ACTIVE then
		(setConf updirID [Icon(updir_outlined_Icon())];
		 setBind updirID [BindEv(Leave, updirleft),
				  BindEv(Enter, updirentered)];
		 UPDIR_ACTIVE := false)
	    else ()

	and enable_updir() =
	    if !UPDIR_ACTIVE then ()
	    else (UPDIR_ACTIVE := true;
		  if !INSIDE_UPDIR then
		      updirentered dummy_event
		  else updirleft dummy_event)

	and backentered _ =
	    (if !BACK_ACTIVE then
		 (setConf backID [Icon(back_highlighted_Icon())];
		  setBind backID [BindEv(Leave, backleft),
				  BindEv(ButtonPress(SOME 1),
					 fn _ => (!BACK)())])
	     else ();
	     INSIDE_BACK := true)

	and backleft _ =
	    (if !BACK_ACTIVE then
		 (setConf backID [Icon(back_Icon())];
		  setBind backID [BindEv(Enter, backentered)])
	     else ();
	     INSIDE_BACK := false)

	and disable_back() =
	    if !BACK_ACTIVE then
		(setConf backID [Icon(back_outlined_Icon())];
		 setBind backID [BindEv(Leave, backleft),
				 BindEv(Enter, backentered)];
		 BACK_ACTIVE := false)
	    else ()

	and enable_back() =
	    if !BACK_ACTIVE then ()
	    else (BACK_ACTIVE := true;
		  if !INSIDE_BACK then
		      backentered dummy_event
		  else backleft dummy_event)

	and forwardentered _ =
	    (if !FORWARD_ACTIVE then
		 (setConf forwardID [Icon(forward_highlighted_Icon())];
		  setBind forwardID [BindEv(Leave, forwardleft),
				     BindEv(ButtonPress(SOME 1),
					    fn _ => (!FORWARD)())])
	     else ();
	     INSIDE_FORWARD := true)

	and forwardleft _ =
	    (if !FORWARD_ACTIVE then
		 (setConf forwardID [Icon(forward_Icon())];
		  setBind forwardID [BindEv(Enter, forwardentered)])
	     else ();
	     INSIDE_FORWARD := false)

	and disable_forward() =
	    if !FORWARD_ACTIVE then
		(setConf forwardID [Icon(forward_outlined_Icon())];
		 setBind forwardID [BindEv(Leave, forwardleft),
				    BindEv(Enter, forwardentered)];
		 FORWARD_ACTIVE := false)
	    else ()

	and enable_forward() =
	    if !FORWARD_ACTIVE then ()
	    else (FORWARD_ACTIVE := true;
		  if !INSIDE_FORWARD then
		      forwardentered dummy_event
		  else forwardleft dummy_event)
(*
	fun mkdirentered _ =
	    (setConf mkDirID [Icon(mkDir_highlighted_Icon())];
	     setBind mkDirID [BindEv(Leave, mkdirleft),
			      BindEv(ButtonPress(SOME 1), fn _ => make_dir())])

	and mkdirleft _ =
	    (setConf mkDirID [Icon(mkDir_Icon())];
	     setBind mkDirID [BindEv(Enter, mkdirentered)])

	fun disable_mkDir() =
	    if !MKDIR_ACTIVE then
		(setConf mkDirID [Icon(mkDir_outlined_Icon())];
		 setBind mkDirID [];
		 MKDIR_ACTIVE := false)
	    else ()

	fun enable_mkDir() =
	    if !MKDIR_ACTIVE then ()
	    else (MKDIR_ACTIVE := true;
		  mkdirleft dummy_event)
*)
	fun filedelentered _ =
	    (setConf filedelID [Icon(filedel_highlighted_Icon())];
	     setBind filedelID [BindEv(Leave, filedelleft),
				BindEv(ButtonPress(SOME 1), del_file)])

	and filedelleft _ =
	    (setConf filedelID [Icon(filedel_Icon())];
	     setBind filedelID [BindEv(Enter, filedelentered)])

	and disable_filedel() =
	    if !FILEDEL_ACTIVE then
		(setConf filedelID [Icon(filedel_outlined_Icon())];
		 setBind filedelID [];
		 FILEDEL_ACTIVE := false)
	    else ()

	and enable_filedel() =
	    if !FILEDEL_ACTIVE then ()
	    else (FILEDEL_ACTIVE := true;
		  filedelleft dummy_event)
(*
	and homedir _ = UW.warning "Not yet implemented!"

	and homedirentered _ =
	    (setConf homedirID [Icon(homedir_highlighted_Icon())];
	     setBind homedirID [BindEv(Leave, homedirleft),
				BindEv(ButtonPress(SOME 1), homedir)])

	and homedirleft _ =
	    (setConf homedirID [Icon(homedir_Icon())];
	     setBind homedirID [BindEv(Enter, homedirentered)])
*)
	and reloadentered _ =
	    (setConf reloadID [Icon(reload_highlighted_Icon())];
	     setBind reloadID [BindEv(Leave, reloadleft),
			       BindEv(ButtonPress(SOME 1),
				      fn _ => show_files true ())])

	and reloadleft _ =
	    (setConf reloadID [Icon(reload_Icon())];
	     setBind reloadID [BindEv(Enter, reloadentered)])

	and disable_reload() =
	    if !RELOAD_ACTIVE then
		(setConf reloadID [Icon(reload_outlined_Icon())];
		 setBind reloadID [];
		 RELOAD_ACTIVE := false)
	    else ()

	and enable_reload() =
	    if !RELOAD_ACTIVE then ()
	    else (RELOAD_ACTIVE := true;
		  reloadleft dummy_event)
		

(*--- delete file -----------------------------------------------------------*)

	and del_file _ =
	    let
		val file = OS.Path.joinDirFile {dir  = !CURRENT_DIRECTORY,
						file = valOf(!CHOSEN_FILE)}

		fun del() = 
		    (OS.FileSys.remove file;
		     UW.info(file ^ " deleted!");
		     show_files true ())
		    handle _ => ()
	    in
		UW.confirm("Really delete " ^ file ^ " ?", del)
	    end


(*--- display files ---------------------------------------------------------*)

	and read_directory() =
	    let
		val dirstream = OS.FileSys.openDir(!CURRENT_DIRECTORY)

		fun displaytype ext (fts : Filetype list) =
		    let
			val ftp =
			    List.find (fn ft =>
				       List.exists (fn e => e = ext) (#ext ft))
			              fts
		    in
			if isSome ftp then
			    let
				val dp = #display(valOf ftp)
			    in
				if isSome dp then SOME(SOME(valOf dp))
				else NONE
			    end
			else if isSome(!DEFAULT_TYPE) then SOME NONE else NONE
		    end

		fun read ""  = []
		  | read new =
		    if (OS.FileSys.isDir(OS.Path.concat(!CURRENT_DIRECTORY,
							new))
			handle NoAcc => false) then
			read (valOf (OS.FileSys.readDir dirstream))
		    else
			if (!SHOW_HIDDEN orelse
			    not(hd(explode new) = #".")) then
			    let
				val dtp =
				    displaytype (ext new) Options.filetypes
			    in
				if isSome dtp then
				    (new, valOf dtp) ::
				    read (valOf (OS.FileSys.readDir dirstream))
				else read (valOf (OS.FileSys.readDir dirstream))
			    end
			else read (valOf (OS.FileSys.readDir dirstream))

		fun type_ord e1 e2 ((ft : Filetype) :: fts) =
		    if (not(e1 = "") andalso
			List.exists (fn x => x = e1) (#ext ft)) then SOME true
		    else (if (not(e2 = "") andalso
			      List.exists (fn x => x = e2) (#ext ft)) then
			      SOME false
			  else type_ord e1 e2 fts)
		  | type_ord _ _ []                         = NONE

		fun ord (e1 : string * DisplayType)
		        (e2 : string * DisplayType) =
		    if !SORT_TYPES then
			let
			    val tord = type_ord (ext(#1 e1)) (ext(#1 e2))
				                Options.filetypes
			in
			    if !SORT_NAMES then
				if isSome tord then valOf tord
				else String.<(#1 e1, #1 e2)
			    else
				if isSome tord then valOf tord
				else true
			end
		    else if !SORT_NAMES then String.<(#1 e1, #1 e2)
			 else true
	    in
		sort (read (valOf (OS.FileSys.readDir dirstream))) ord before
		OS.FileSys.closeDir dirstream
	    end

	and show_files pat () =
	    let
		fun enter id _ =
		    if isSome(!SELECTED) andalso id = valOf(!SELECTED) then ()
		    else addConf id [Background Grey, Foreground White]

		fun leave id _ =
		    if isSome(!SELECTED) andalso id = valOf(!SELECTED) then ()
		    else addConf id [Background White, Foreground Black]

		fun comment id com _ =
		    (addConf file_statusID [Foreground Black, Text com];
		     enter id ())

		fun press nm id _ =
		    (if !ENTER_FILE then () else clearText file_entryID;
		     if isSome(!SELECTED) then
			 addConf (valOf(!SELECTED)) [Relief Flat,
						     Background White,
						     Foreground Black]
		     else ();
		     if OS.FileSys.access(OS.Path.joinDirFile
					    {dir  = !CURRENT_DIRECTORY,
					     file = nm},
					  [OS.FileSys.A_WRITE]) then
			 enable_filedel()
		     else disable_filedel();
		     SELECTED := SOME id;
		     CHOSEN_FILE := SOME nm;
		     addConf id [Relief Sunken, Background Grey,
				 Foreground White];
		     if !ENTER_FILE then ()
		     else insertTextEnd file_entryID nm)

		fun show ((f : string * DisplayType) :: fs) y col b =
		    if (readTextAll patternID = "" orelse
			Rex.match (readTextAll patternID) (#1 f)
			handle _ =>
			    (addConf file_statusID
			             [Text
				       "Bad regular expression, ignoring...",
				      Foreground Red]; 
			     true)) then
		    let
			val _ = busy()

			val icon =
			    if isSome(#2 f) then
				FileImage
				  (OS.Path.concat(Options.icons_path(),
						  #icon(valOf(#2 f))),
				   newImageId())
			    else if #icon(valOf(!DEFAULT_TYPE)) = "" then
				     unknown_Icon()
				 else FileImage
				        (OS.Path.concat
					   (Options.icons_path(),
					    #icon(valOf(!DEFAULT_TYPE))),
					 newImageId())

			val maxwidth =
			    (Options.Conf.filesbox_width - 10)
			    div (if b then 3 else 2)

			fun doPut nm ev =
			    let
				fun fto ((ft : Filetype) :: fts) =
				    if (List.exists (fn x => x = ext nm)
					            (#ext ft)) then
					if (isSome(#display ft) andalso
					    isSome(#file_to_obj
						     (valOf
						        (#display ft)))) then
					    SOME
					      (valOf
					         (#file_to_obj
						    (valOf(#display ft))))
					else NONE
				    else fto fts
				  | fto []                       =
				    #file_to_obj(valOf(!DEFAULT_TYPE))

				val file_to_obj = fto Options.filetypes
			    in
				if isSome file_to_obj then
				    Options.CB.put
				      (valOf(file_to_obj)
				         {dir  =
					  if root_dir() = "/" then
					      !CURRENT_DIRECTORY
					  else
					      OS.Path.mkRelative
					        {path = !CURRENT_DIRECTORY,
						 relativeTo = root_dir()},
					  file = nm}) ev (fn () => ())
				else ()
			    end

			fun preview _ =
			    if isSome(#2 f) then
				if isSome(#preview(valOf(#2 f))) then
				    (valOf(#preview(valOf(#2 f))))
				      {dir  =
				         if root_dir() = "/" then
					     !CURRENT_DIRECTORY
					 else
					     OS.Path.mkRelative
					       {path = !CURRENT_DIRECTORY,
						relativeTo = root_dir()},
				       file = #1 f}
				else
				    addConf file_statusID
				      [Text
				      "No preview function for this filetype!",
				       Foreground Blue]
			    else ()

			val entry =
			    let
				val id = newWidgetId()
				val txt = shortright (#1 f)
				                     Options.Conf.filenames_cut
				val com =
				    if isSome (#2 f) then
					#comment(valOf(#2 f))
				    else #comment(valOf(!DEFAULT_TYPE))
				val binds =
				    (BindEv(Leave, leave id) ::
				     (if b then [BindEv(Enter, comment id com)]
				      else [BindEv(Enter, enter id)])) @
				    [BindEv(ButtonPress(SOME 1),
					    press (#1 f) id),
				     BindEv(ButtonRelease(SOME 1),
					    doPut(#1 f)),
				     BindEv(ButtonPress(SOME 2),
					    preview)]
			    in
				if b then
				    Frame
				      {widId    = newWidgetId(),
				       widgets  =
				         Pack
					   ((if !HIDE_ICONS then []
					    else [Label
						    {widId    = newWidgetId(),
						     packings = [],
						     configs  =
						       [Background White,
							Icon icon,
							Width maxwidth],
						     bindings = binds}]) @
					    [Label
					       {widId    = id,
						packings = [],
						configs  =
						  [Text txt,
						   Background White,
						   Font
						     Options.Conf.icon_font],
						bindings = binds}]),
				       packings = [],
				       configs  = [Background White],
				       bindings = []}
				else
				    let
					val date =
					    Date.toString
					      (Date.fromTimeLocal
					         (OS.FileSys.modTime
						    (OS.Path.joinDirFile
						       {dir  =
							  !CURRENT_DIRECTORY,
							file = #1 f})))
				    in
					Frame
					  {widId    = newWidgetId(),
					   widgets  =
					     Pack
					       ((if !HIDE_ICONS then []
						 else [Label
						         {widId    =
							    newWidgetId(),
							  packings =
							    [Side Left],
							  configs  =
							    [Background White,
							     Icon icon],
							  bindings =
							    binds}]) @
						[Label
						   {widId    = id,
						    packings = [Side Left],
						    configs  =
						      ([Text txt,
							Background White,
							Width
						    Options.Conf.filenames_cut,
							Font
						      Options.Conf.icon_font] @
						       (if !HIDE_ICONS then
							    [Anchor West]
							else [])),
						    bindings = binds},
						 Label
						   {widId    = newWidgetId(),
						    packings = [PadX 8,
								Side Left],
						    configs  =
						      [Text com,
						       Background White,
						       Width
						        (max_comment_length()),
							Font
						       Options.Conf.icon_font],
						    bindings = []},
						 Label
						   {widId    = newWidgetId(),
						    packings = [PadX 8,
								Side Left],
						    configs  =
						      [Text date,
						       Background White,
						       Font
						       Options.Conf.icon_font],
						    bindings = []}]),
					   packings = [],
					   configs  = [Background White],
					   bindings = []}
				    end
			    end

			val newcol = 
			    (col + 1) 
			    mod (if b then Options.Conf.filesbox_numcols
				 else 1)

			val newy =
			    if newcol = 0 then
				if b then
				    y + 2 +
				    (if !HIDE_ICONS then 0
				     else #2 Options.icons_size) +
				    Options.Conf.icon_font_height
				else y + 2 +
				     Int.max(Options.Conf.font_height,
					     if !HIDE_ICONS then 0
					     else #2 Options.icons_size)
			    else y
		    in
			addCItem filesboxID
			  (CWidget {citemId  = newCItemId(),
				    coord    = (5 + col * maxwidth, y),
				    widgets  = Pack [entry],
				    configs  = [Anchor NorthWest],
				    bindings = []});
			  show fs newy newcol b
		    end
		    else show fs y col b
		  | show _ y col b                                      =
		    addConf filesboxID
		            [ScrollRegion
			       (0, 0, 0,
				Int.max
				  (if col = 0 then y
				   else
				       if b then
					   y + 2 + (#2 Options.icons_size) +
					   Options.Conf.icon_font_height
				       else y + 2 +
					    Int.max(Options.Conf.font_height,
						    #2 Options.icons_size),
				   Options.Conf.boxes_height))]

		val files = read_directory() handle _ => []
	    in
		disable_filedel();
		if pat andalso isSome(Options.default_pattern) then
		    (clearText patternID;
		     insertTextEnd patternID (valOf(Options.default_pattern)))
		else ();
		if (!CURRENT_DIRECTORY = root_dir()) then disable_updir()
		else enable_updir();
		addConf file_statusID [Foreground Black,
				       Text "Reading directory..."];
		SELECTED    := NONE;
		CHOSEN_FILE := NONE;
		if (null files andalso
		    not(OS.FileSys.access(!CURRENT_DIRECTORY,
					  [OS.FileSys.A_READ]))) then
		    addConf fold_statusID
		            [Foreground Red, Text "Permission denied."]
		else
		    if OS.FileSys.access(!CURRENT_DIRECTORY,
					 [OS.FileSys.A_WRITE]) then
			(addConf fold_statusID [Text ""] (*;
			 enable_mkDir()*) )
		    else (addConf fold_statusID
			          [Foreground Black,
				   Text "Directory is read-only."] (*;
			  disable_mkDir()*));
		if !ENTER_FILE then () else clearText file_entryID;
		app (delCItem filesboxID)
		    (map selItemId (selCanvasItems(getWidget filesboxID)));
		if (null files orelse
		    List.all (fn f =>
			        (not(Rex.match (readTextAll patternID) (#1 f)))
				handle _ => false)
		             files) then
		    (addCItem filesboxID (CText {citemId  = newCItemId(),
						 coord    = (5, 5),
						 configs  =
						   [Anchor NorthWest,
						    Font
						      Options.Conf.icon_font,
						    Text "No files."],
						 bindings = []});
		     addConf filesboxID [ScrollRegion(0, 0, 0, 0)])
		else show files 0 0 (!HIDE_DETAILS);
		addConf file_statusID [Text "Reading directory... ready",
				       Foreground Black];
		case (!POSITION)() of
		    Tree.hist_empty  => (disable_back();
					 disable_forward()(*;
					 print "hist_empty\n"*))
		  | Tree.hist_start  => (disable_back();
					 enable_forward()(*;
					 print "hist_start\n"*))
		  | Tree.hist_middle => (enable_back();
					 enable_forward()(*;
					 print "hist_middle\n"*))
		  | Tree.hist_end    => (enable_back();
					 disable_forward()(*;
					 print "hist_end\n"*));
		ready()
	    end


(*--- widgets ---------------------------------------------------------------*)

	fun ch_dir ob =
	    if isSome ob then (enable_reload();
			       CURRENT_DIRECTORY := sel_path (valOf ob);
			       addConf dir_labelID [Text(!CURRENT_DIRECTORY)];
			       show_files true ())
	    else (CURRENT_DIRECTORY := "";
		  app (delCItem filesboxID)
		      (map selItemId (selCanvasItems(getWidget filesboxID)));
		  addConf file_statusID [Text ""];
		  disable_filedel();
		  disable_updir();
		  disable_reload() (*;
		  disable_mkDir()*))

	fun cnv ob =
	    let
		val {canvas, selection, up, position, back, forward} =
		    Tree.tree_list {width              =
				      Options.Conf.foldersbox_width,
				    height             =
				      Options.Conf.boxes_height,
				    font               =
				      Options.Conf.icon_font,
				    selection_notifier = ch_dir}
	    in
		(UP       := up;           (* unschön !!! *)
		 BACK     := back;
		 FORWARD  := forward;
		 POSITION := position;
		 canvas ob)
	    end

	val topmenu =
	    let
		fun sort_names _ = (SORT_NAMES := not(!SORT_NAMES);
				    show_files true ())
		fun sort_types _ = (SORT_TYPES := not(!SORT_TYPES);
				    show_files true ())
		fun show_hidden _ = (SHOW_HIDDEN := not(!SHOW_HIDDEN);
				     show_files true ())
		fun hide_icons _ = (HIDE_ICONS := not(!HIDE_ICONS);
				    show_files true ())
		fun hide_details _ = (HIDE_DETAILS := not(!HIDE_DETAILS);
				      show_files true ())
	    in
		Frame {widId    = newWidgetId(),
		       widgets  =
		         Pack [Menubutton
			         {widId    = newWidgetId(),
				  mitems   =
				    [MCommand
				       [Text "Quit",
					Command
					  (fn _ =>
					     closeWindow file_select_winID)]],
				  packings = [Side Left],
				  configs  = [Text "File", Tearoff false],
				  bindings = []},
			       Menubutton {widId    = newWidgetId(),
					   mitems   =
					     [(*MCommand [Text "New folder",
							Command make_dir],*)
					      MCommand
					        [Text "Delete file",
						 Command
						   (fn() =>
						      del_file dummy_event)]],
					   packings = [Side Left],
					   configs  = [Text "Edit",
						       Tearoff false],
					   bindings = []},
			       Menubutton {widId    = newWidgetId(),
					   mitems   =
					     [MCheckbutton
					        [Text "Show hidden files",
						 Command show_hidden,
						 Variable "showhidden"],
					      MCheckbutton
					        [Text "Hide icons",
						 Command hide_icons,
						 Variable "hideicons"],
					      MCheckbutton
					        [Text "Hide details",
						 Command hide_details,
						 Variable "hidedetails"],
					      MCheckbutton
					        [Text "Sort filenames",
						 Command sort_names,
						 Variable "namessort"],
					      MCheckbutton
					        [Text "Sort filetypes",
						 Command sort_types,
						 Variable "typessort"]],
					   packings = [Side Right],
					   configs  = [Text "Preferences",
						       Tearoff false],
					   bindings = []}],
		       packings = [Fill X],
		       configs  = [],
		       bindings = []}
	    end (* val topmenu *)

    fun toolbar() =
	let
	    val actions =
		Canvas {widId      = toolbarID,
			scrolltype = NoneScb,
			citems     =
			  [CWidget
			     {citemId  = newCItemId(),
			      coord    = (6, 6),
			      widgets  =
			        Pack [Label {widId    = updirID,
					     packings = [],
					     configs  =
					     [Icon(updir_outlined_Icon())],
					     bindings = []}],
			      configs  = [Anchor NorthWest],
			      bindings = []},
			   CWidget {citemId  = newCItemId(),
				    coord    = (39, 6),
				    widgets  =
				      Pack [Label
					      {widId    = backID,
					       packings = [],
					       configs  =
					         [Icon(back_outlined_Icon())],
					       bindings = []}],
				    configs  = [Anchor NorthWest],
				    bindings = []},
			   CWidget {citemId  = newCItemId(),
				    coord    = (72, 6),
				    widgets  =
				      Pack [Label
					      {widId    = forwardID,
					       packings = [],
					       configs  =
					         [Icon
						    (forward_outlined_Icon())],
					       bindings = []}],
				    configs  = [Anchor NorthWest],
				    bindings = []}] @
(*			  (if isSome(OS.Process.getEnv "HOME")
			      andalso sub_dir (valOf(OS.Process.getEnv
						     "HOME"))
			                      (root_dir()) then
			       [CWidget
				  {citemId  = newCItemId(),
				   coord    = (105, 6),
				   widgets  =
				     Pack [Label
					     {widId    = homedirID,
					      packings = [],
					      configs  =
					        [Icon(homedir_Icon())],
					      bindings =
					        [BindEv(Enter,
							homedirentered)]}],
				   configs  = [Anchor NorthWest],
				   bindings = []}]
			   else
			       [CWidget
				  {citemId  = newCItemId(),
				   coord    = (105, 6),
				   widgets  =
				     Pack
				       [Label
					  {widId    = homedirID,
					   packings = [],
					   configs  =
					     [Icon(homedir_outlined_Icon())],
					   bindings = []}],
				   configs  = [Anchor NorthWest],
				   bindings = []}]) @ *)
			       [CWidget
				  {citemId  = newCItemId(),
				   coord    = ((*138*) 105, 6),
				   widgets  =
				     Pack [Label
					     {widId    = reloadID,
					      packings = [],
					      configs  =
					        [Icon(reload_outlined_Icon())],
					      bindings = []}],
				   configs  = [Anchor NorthWest],
				   bindings = []},
(*				CWidget
				  {citemId  = newCItemId(),
				   coord    = (190, 6),
				   widgets  =
				     Pack [Label
					     {widId    = mkDirID,
					      packings = [],
					      configs  =
					        [Icon(mkDir_outlined_Icon())],
					      bindings = []}],
				   configs  = [Anchor NorthWest],
				   bindings = []},*)
				CWidget
				  {citemId  = newCItemId(),
				   coord    = ((*223*) 138, 6),
				   widgets  =
				     Pack [Label
					     {widId    = filedelID,
					      packings = [],
					      configs  =
					        [Icon
						   (filedel_outlined_Icon())],
					      bindings = []}],
				   configs  = [Anchor NorthWest],
				   bindings = []}],
			packings   = [Side Left],
			configs    = [Height 30, Width 250],
			bindings   = []}
	in
	    Frame {widId    = newWidgetId(),
		   widgets  = Pack [actions],
		   packings = [Fill X],
		   configs  = [],
		   bindings = []}
	end (* val toolbar *)

    val dir_label =
	Frame {widId    = newWidgetId(),
	       widgets  =
	         Pack [Label {widId    = newWidgetId(),
			      packings = [Side Left],
			      configs  = [Text "Directory:", Width 10],
			      bindings = []},
		       Label {widId    = dir_labelID,
			      packings = [Fill X, Expand true],
			      configs  = [Relief Sunken, Anchor West,
					  Font Options.Conf.font],
			      bindings = []}],
	       packings = [PadX 30, PadY 2, Fill X, Expand true],
	       configs  = [],
	       bindings = []}

	val pattern =
	    Frame {widId    = newWidgetId(),
		   widgets  =
		     Pack [Label {widId    = newWidgetId(),
				  packings = [Side Left],
				  configs  = [Text "Pattern:", Width 10],
				  bindings = []},
			   Entry {widId    = patternID,
				  packings = [Fill X, Expand true],
				  configs  = [Background White,
					      Font Options.Conf.font],
				  bindings = [BindEv(KeyPress "Return",
						     fn _ => show_files
						               false ())]}],
		   packings = [PadX 30, PadY 2, Fill X, Expand true],
		   configs  = [],
		   bindings = []}

	fun foldersbox() =
	    let
		val {dir, file} = OS.Path.splitDirFile(root_dir())
		val root_nm     = if file = "" then "/" else file
	    in
		Frame {widId    = foldersboxframeID,
		       widgets  =
		         Pack [cnv (Obj.Node(root_nm, root_dir(),
					     acc_fold_icon(),
					     open_fold_icon())),
			       Label {widId    = fold_statusID,
				      packings = [Fill X, Expand true],
				      configs  = [Relief Sunken, Anchor West,
						  Font Options.Conf.font],
				      bindings = []}],
		       packings = [],
		       configs  = [],
		       bindings = []}
	    end

	val filesbox =
	    Frame
	      {widId    = filesboxframeID,
	       widgets  =
	         Pack [Canvas {widId      = filesboxID,
			       scrolltype = RightScb,
			       citems     = [],
			       packings   = [],
			       configs    = [Background White,
					     Width Options.Conf.filesbox_width,
					     Height Options.Conf.boxes_height],
			       bindings   = []},
		       Label {widId    = file_statusID,
			      packings = [Fill X, Expand true],
			      configs  = [Relief Sunken, Anchor West,
					  Font Options.Conf.font],
			      bindings = []}],
	       packings = [Side Right],
	       configs  = [],
	       bindings = []}

	fun ok cont _ =
	    (if readTextAll file_entryID = "" then ()
	     else CHOSEN_FILE := SOME(readTextAll file_entryID);
	     EXIT_STATUS := true;
	     closeWindow file_select_winID;
	     cont(SOME(SOME(!CURRENT_DIRECTORY), !CHOSEN_FILE)))

	fun file_entry cont =
	    Frame {widId    = newWidgetId(),
		   widgets  =
		     Pack [Label {widId    = newWidgetId(),
				  packings = [Side Left],
				  configs  = [Text "File:", Width 10],
				  bindings = []},
			   Entry {widId    = file_entryID,
				  packings = [Fill X, Expand true],
				  configs  = [Background White,
					      Font Options.Conf.font],
				  bindings = [BindEv(KeyPress "Return",
						     ok cont)]}],
		   packings = [PadX 10, PadY 5, Side Left, Fill X,
			       Expand true],
		   configs  = [],
		   bindings = []}

	fun buttons cont =
	    if !ENTER_FILE then
		Button {widId    = newWidgetId(),
			packings = [PadX 5, Side Right],
			configs  = [Text "Close",
				    Command(fn _ =>
					      closeWindow file_select_winID),
				    Width 15],
			bindings = []}
	    else
		Frame {widId    = newWidgetId(),
		       widgets  =
		         Pack [Button {widId    = newWidgetId(),
				       packings = [],
				       configs  =
				         [Text "Ok", Command(ok cont),
					  Width 15],
				       bindings = []},
			       Button {widId    = newWidgetId(),
				       packings = [],
				       configs  =
				         [Text "Cancel",
					  Command
					    (fn _ =>
					       (closeWindow file_select_winID;
						cont NONE)),
					  Width 15],
				       bindings = []}],
		       packings = [PadX 5, PadY 5, Side Right],
		       configs  = [],
		       bindings = []}


(*--- and go... -------------------------------------------------------------*)

	fun set_vars() =
	    (if !SORT_NAMES then setVarValue "namessort"   "1"
	     else setVarValue "namessort"   "0";
	     if !SORT_TYPES then setVarValue "typessort"   "1"
	     else setVarValue "typessort"   "0";
	     if !SHOW_HIDDEN then setVarValue "showhidden"  "1"
	     else setVarValue "showhidden"  "0";
	     if !HIDE_ICONS then setVarValue "hideicons"   "1"
	     else setVarValue "hideicons"   "0";
	     if !HIDE_DETAILS then setVarValue "hidedetails" "1"
	     else setVarValue "hidedetails" "0")

	fun set_refs() =
	    (UPDIR_ACTIVE   := false;
	     INSIDE_UPDIR   := false;
	     BACK_ACTIVE    := false;
	     INSIDE_BACK    := false;
	     FORWARD_ACTIVE := false;
	     INSIDE_FORWARD := false;
	     MKDIR_ACTIVE   := false;
	     FILEDEL_ACTIVE := false;
	     RELOAD_ACTIVE  := false)

	fun set_default_filetype ((ft : Filetype) :: fts) =
	    if List.exists (fn x => x = "") (#ext ft) then
		DEFAULT_TYPE := #display ft
	    else set_default_filetype fts
	  | set_default_filetype []                       = ()

	fun initialize _ =
	    (CURRENT_DIRECTORY := root_dir();
	     SELECTED := NONE;
	     CHOSEN_FILE := NONE;
	     insertTextEnd patternID (if isSome Options.default_pattern then
					  valOf Options.default_pattern
				      else "");
	     set_default_filetype Options.filetypes;
	     set_vars())

	fun file_select_win cont =
	    mkWindow {winId    = file_select_winID,
		      config   = [WinTitle (if isSome(Options.Conf.title) then
						valOf(Options.Conf.title)
					    else "File selection")],
		      widgets  =
		        Pack([topmenu, dir_label, pattern, toolbar(),
			      Frame {widId    = newWidgetId(),
				     widgets  = Pack [filesbox, foldersbox()],
				     packings = [PadX 10, PadY 5],
				     configs  = [],
				     bindings = []}] @
			     (let
				  val wids = if !ENTER_FILE then [buttons cont]
					     else [file_entry cont,
						   buttons cont]
			      in
				  [Frame {widId = newWidgetId(),
					  widgets = Pack wids,
					  packings = [PadX 30, Fill X,
						      Expand true],
					  configs  = [],
					  bindings = []}]
			      end)),
		      bindings = [],
		      init     = initialize}

	fun set (x : {sort_names          : bool option,
		      sort_types          : bool option,
		      show_hidden_files   : bool option,
		      hide_icons          : bool option,
		      hide_details        : bool option}) =
	    (if isSome(#sort_names x) then SORT_NAMES := valOf(#sort_names x)
	     else ();
	     if isSome(#sort_types x) then SORT_TYPES := valOf(#sort_types x)
	     else ();
	     if isSome(#show_hidden_files x) then
		 SHOW_HIDDEN := valOf(#show_hidden_files x)
	     else ();
	     if isSome(#hide_icons x) then HIDE_ICONS := valOf(#hide_icons x)
	     else ();
	     if isSome(#hide_details x) then
		 HIDE_DETAILS := valOf(#hide_details x)
	     else ();
	     set_vars())

	fun checkPathsOfVisibleFiletypes() =
	    let
		fun check ((x : Filetype) :: xs) =
		    if isSome(#display x) then
			if OS.FileSys.access
			    (OS.Path.joinDirFile
			       {dir  = Options.icons_path(),
				file = #icon(valOf(#display x))},
			       []) handle NoAcc => false then
			    check xs
			else
			    (print("Could not find " ^
				   OS.Path.joinDirFile
				     {dir = Options.icons_path(),
				      file = #icon(valOf(#display x))});
			     raise Error("Could not find " ^
					 OS.Path.joinDirFile
					   {dir = Options.icons_path(),
					    file = #icon(valOf(#display x))}))
		    else check xs
		  | check [] = true
	    in
		check Options.filetypes
	    end

	fun stand_alone() =
	    if checkPathsOfVisibleFiletypes() then
		(ENTER_FILE := false;
		 startTcl [file_select_win (fn _ => ())];
		 if !EXIT_STATUS then
		     SOME(if !CURRENT_DIRECTORY = "" then NONE
			  else SOME(!CURRENT_DIRECTORY), !CHOSEN_FILE)
		 else NONE)
	    else NONE

	fun file_select cont =
	    if checkPathsOfVisibleFiletypes() then
		(ENTER_FILE := false;
		 openWindow (file_select_win cont))
	    else ()
(*		 if !EXIT_STATUS then
		     SOME(if !CURRENT_DIRECTORY = "" then NONE
			  else SOME(!CURRENT_DIRECTORY), !CHOSEN_FILE)
		 else NONE)
	    else NONE
*)
	fun enter_file() =
	    if checkPathsOfVisibleFiletypes() then
		(ENTER_FILE := true;
		 openWindow (file_select_win (fn _ => ())))
	    else ()

    end (* functor Filer *)


(*--- simple filer without clipboard ----------------------------------------*)

functor SimpleFiler
    (structure Options :
	 sig
	     val icons_path      : unit -> string
	     val icons_size      : int * int
	     val root            : unit -> string option
	     val default_pattern : string option

	     val filetypes : {ext     : string list,
			      display : {comment     : string,
					 icon        : string,
					 preview     : ({dir  : string,
							 file : string}
							-> unit) option,
	(* instantiate with NONE ! *)    file_to_obj : ({dir  : string,
							 file : string}
							-> DummyCB.obj)
					               option}
                                        option} list

	     structure Conf : FILER_CONFIG
	 end) : FILER_SIG =
	 Filer (structure Options =
		    struct
			val icons_path      = Options.icons_path
			val icons_size      = Options.icons_size
			val root            = Options.root
			val default_pattern = Options.default_pattern
			val filetypes       = Options.filetypes
			structure Conf      = Options.Conf
			structure CB        = DummyCB
		    end)
