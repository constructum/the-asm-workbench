(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/tests+examples/filer_ex.sml,v $

   Complete example instantiation of functor filer

   $Date: 2001/03/30 13:40:02 $
   $Revision: 3.0 $
   Author: ludi (Last modification by $Author: 2cxl $)

   (C) 1999, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)

structure FilerEx : sig
			val go : unit -> unit
		    end =
struct
    structure Options =
	struct
	    fun icons_path()    = OS.Path.concat(SmlTk.getLibPath(),
						 "icons/filer/example")
	    val icons_size      = (24, 24)
	    fun root()          = NONE (* SOME "/home/ludi" *)
	    val default_pattern = NONE (* SOME ".*.sml" *)
	    structure CB        = Clipboard(type obj = string)

	    val filetypes =
		[{ext     = [""],
	          display = SOME {comment     = "Default filetype",
	                          icon        = "default_Icon.gif",
				  preview     = NONE,
				  file_to_obj = SOME OS.Path.joinDirFile}},
		                          (* example for a default filetype *)

		     (* the empty string in the extensions list sets the     *
		      * default filetype;                                    *
                      * if there is no default filetype, unmatched files are *
		      * displayed as "Unknown filetype" with the system icon *)

		 {ext     = ["sml"],
		  display = SOME {comment     = "Standard ML file",
				  icon        = "sml_Icon.gif",
				  preview     = NONE,
				  file_to_obj = SOME OS.Path.joinDirFile}},
		 {ext     =["gif"],
		  display = SOME {comment     = "GIF - image",
				  icon        = "gif_Icon.gif",
				  preview     = NONE,
				  file_to_obj = SOME OS.Path.joinDirFile}},
		 {ext     = ["ps"],
		  display = SOME {comment     = "PostScript file",
				  icon        = "ps_Icon.gif",
				  preview     =
				     SOME(fn {dir, file} =>
					  (OS.Process.system
					     ("ghostview " ^
					      OS.Path.joinDirFile
					        {dir  = (if isSome(root()) then
							     OS.Path.concat
							       (valOf(root()),
								dir)
							 else dir),
						 file = file} ^ " &"); ())
					   handle _ => ()),
				  file_to_obj = SOME OS.Path.joinDirFile}},
		 {ext     = ["html"],
		  display = SOME {comment     = "HTML document",
				  icon        = "html_Icon.gif",
				  preview     =
				    SOME(fn {dir, file} =>
					  (OS.Process.system
					     ("netscape -raise -remote 'openFile("^
					      OS.Path.joinDirFile
					        {dir  = (if isSome(root()) then
							     OS.Path.concat
							       (valOf(root()),
								dir)
							 else dir),
						 file = file}^")' &"); ())
					   handle _ => ()),
				  file_to_obj = SOME OS.Path.joinDirFile}}]

	    structure Conf = FilerDefaultConfig
	end (* structure Options *)

    open SmlTk

    structure Example = Filer(structure Options = Options)

    val txtID = newWidgetId()

    fun dropped ev = if Options.CB.isEmpty ev then ()
		     else (addConf txtID [Active true];
			   insertTextEnd txtID
			                 (Options.CB.get ev ^ " dropped\n");
			   addConf txtID [Active false])

    fun cont sel =
	(addConf txtID [Active true];
	 case sel of
	     SOME(SOME f, SOME d)=> 
		  insertTextEnd txtID("Dir "^d^", file "^f^" selected.\n")
	   | _ => insertTextEnd txtID("Not a lot selected.\n");
         addConf txtID [Active false])

    fun go () = 
	(addConf txtID [Active true];
	 Example.file_select cont)

    val drop_win = mkWindow {winId    = newWinId(),
			     config   = [WinTitle "Drop field"],
			     widgets  =
			       Pack [TextWid {widId      = txtID,
					      scrolltype = RightScb,
					      annotext   = mtAT,
					      packings   = [],
					      configs    = [Height 5,
							    Active false],
					      bindings   =
					        [BindEv(Enter, dropped)]},
				     Button {widId = newWidgetId(),
					     packings = [],
					     configs  =
					       [Text "Start Filer",
						Command go],
					     bindings = []},
				     Button {widId = newWidgetId(),
					     packings = [],
					     configs  =
					     [Text "Quit",
					      Command(fn _ => exitTcl())],
					     bindings = []}],
			     bindings = [],
			     init     = fn () => ()}

    fun go() = startTcl [drop_win]
end (* structure FilerEx *)
