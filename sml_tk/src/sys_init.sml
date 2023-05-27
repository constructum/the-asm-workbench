(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/sys_init.sml,v $
 
   Initialization functions for sml_tk
 
   $Date: 2001/03/30 13:39:19 $
   $Revision: 3.0 $

   Author: stefan (Last modification by $Author: 2cxl $)

   (C) 1996, Bremen Institute for Safe Systems (BISS), University of Bremen. 

  ************************************************************************** *)

signature SYS_INIT =
sig
    val getEnvSetting : string -> string Option.option 
    val initSmlTk : unit -> unit
end;

structure SysInit : SYS_INIT =
struct

    open BasicTypes BasicUtil.FileUtil ComState

    val oldDisplay = ref ""

    fun getDisplay () =    
	let val dpy = Option.getOpt(OS.Process.getEnv "DISPLAY", "")
	    val host= Option.getOpt(OS.Process.getEnv "HOSTNAME", "")
	in  (if String.sub(dpy, 0)= #":" then host^dpy   
	     (* prefix with host name if display name is ":0.0" or some such *)
	     else dpy)
	    handle Subscript=> host^":0"
    end
	    
    fun isFileRdAndEx pn =
	(*	OS.FileSys.access (pn,[OS.FileSys.A_READ,OS.FileSys.A_EXEC]) *)
	OS.FileSys.access (pn,[OS.FileSys.A_READ])
	
    fun isFileRd pn =
	OS.FileSys.access (pn,[OS.FileSys.A_READ])
	
    fun isDirRdAndWr pn =
	(OS.FileSys.access (pn,[OS.FileSys.A_READ,OS.FileSys.A_WRITE])) andalso
	(OS.FileSys.isDir pn)

    fun getEnvSetting name = 
	(* read an environment variable NAME. A command line setting of
	 * --name=... overrides the environment variable. 
	 *)
	let (* this is the command line option which overrides the env.var *)
	    val envsetting = "--" ^ (String.map Char.toLower name)
  	    (* get command line args (so we reparse them for every variable 
	     * but this only happens when we start so it's ok) *)
	    val cmds = (map (String.fields (fn c=> c= #"="))) 
			   (CommandLine.arguments())
	in  case (List.find (fn name::arg::_=> name= envsetting 
                              | _ => false) cmds) of
	       SOME (_::setting::_) => SOME setting  		   
	     | NONE => (* cinnae find anything, get it oaf the environment *)
		       OS.Process.getEnv name
	end

    fun checkUpdPaths () =	
	((* check and update settings if necessary.
	  * note that logging is turned off it SMLTK_LOG is not set,
	  * whereas the paths to the lib and the wish remain unchanged
	  * if SMLTK_LIB and SMLTK_TCL do not exist. *)      
	 updLibPath(Option.valOf(getEnvSetting(#name SysConf.libVar)))
	    handle Option.Option=> ();
	 updLogfilename(getEnvSetting(#name SysConf.logfileVar));
	 updWishPath(Option.valOf(getEnvSetting(#name SysConf.wishVar)))
	    handle Option.Option=> ();
	 (* now check the (possbily updated) paths: *)
	 let val wish_ok = isFileRdAndEx(getWishPath())
	     val lib_ok  = isDirRdAndWr(getLibPath()) (* Writeable ?!?! *)
	     val testfont = Fonts.getTestfontPath(getLibPath())
	     val font_ok = isFileRdAndEx(testfont)
	     val dpy_ok  = Option.isSome(OS.Process.getEnv "DISPLAY")
	 in
	     TextIO.output(TextIO.stdOut, "\nsml_tk parameter settings:\n\
		             \--------------------------\n");
	     TextIO.output(TextIO.stdOut, "wish (SMLTK_TCL)       : "^(getWishPath())^	       
		     (if not wish_ok then 
			 " *** WARNING: no executable found!\n"
		      else "\n"));
	     TextIO.output(TextIO.stdOut, "library (SMLTK_LIB)    : "^(getLibPath())^
		     (if not lib_ok then
			 " *** WARNING: not a r/w directory!\n" 
		      else "\n"));	      
	     if not font_ok then 
		  TextIO.output(TextIO.stdOut, 
				"*** WARNING: no executable `testfont` found at "^testfont^"\n")
	     else ();
	     if not dpy_ok then 
		   TextIO.output(TextIO.stdOut, 
				 "*** WARNING: environmnent variable DISPLAY not set.\n")
	     else ();
             (case (getLogfilename ()) of
                 NONE   => TextIO.output(TextIO.stdOut, "logfile (SMLTK_LOGFILE): NONE\n")
	       | SOME f => TextIO.output(TextIO.stdOut, "logfile (SMLTK_LOGFILE): "^f^"\n"));
	     if not (wish_ok andalso font_ok andalso lib_ok andalso dpy_ok) then
		  TextIO.output(TextIO.stdErr, "\n*** Warnings have occured, sml_tk malfunction likely.\n\n")
	     else ()
	 end)

    (* Thee following functions should go into sys_dep, but that leads to 
     * a cycle in the definitions. *)
(*
    local open Signals Posix.TTY in

    fun initTTY () =
	 let (* configure TTY driver to make ^\ generate sigQUIT *)
	     val {iflag, oflag, cflag, lflag, cc, ispeed, ospeed} = 
	                 fieldsOf (getattr Posix.FileSys.stdin)
	     val nuattr= termios {iflag=iflag, oflag=oflag, cflag=cflag, 
				  lflag=lflag, ispeed=ispeed, ospeed=ospeed,
				  cc=V.update(cc, [(V.quit, Char.chr 28)])}
	 in  setattr(Posix.FileSys.stdin, TC.sanow, nuattr);
	     (* install the top level continuation as QUIT signal handler *)
	     (* (This doesn't really work because we get uncaught exceptions,
	      *  but at least we return to the top level...) *)
	     setHandler(UnixSignals.sigQUIT, 
			HANDLER (fn _ => !Unsafe.topLevelCont));
       	     (* ignore broken pipes, so SML doesn't terminate when wish dies *)
	     setHandler(UnixSignals.sigPIPE, IGNORE);
	     (* ignore interrupts-- they are only enabled (and handled) while
	      * calling functions bound to events *)
	     setHandler(sigINT, IGNORE);
	     (* announce these changes *)
	     print "\nNote: use INTR (Ctrl-C) to stop diverging computations,\
                   \\n      use QUIT (Ctrl-\\) to abort sml_tk's toplevel.\n\n"
         end
 
    fun resetTTY () =
       ignore(setHandler(sigINT, inqHandler UnixSignals.sigQUIT);
	      setHandler(UnixSignals.sigQUIT, IGNORE))
    end
*)
	
    fun initSmlTk () =
	(checkUpdPaths();
      
	 SysDep.initTTY (fn ()=> print"[smltk] Abort.\n");

	 (* default initializiation for the wish *)       
	 updTclInit
	   " set tcl_prompt1 \"puts -nonewline {} \" \n \
	    \ set tcl_prompt2 \"puts -nonewline {} \" \n ";
			      
	(* only if DISPLAY has changed, re-initialize fonts *)
	let val nuDisplay= getDisplay()
	in  if nuDisplay= (!oldDisplay) then ()
	    else (oldDisplay:= nuDisplay; Fonts.init(getLibPath()))
	end	           
       )

end



