(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/njml.sml,v $
 
   Implementation of system-dependend functions for SMLNJ 109/110.
  
   $Date: 2001/03/30 13:39:14 $
   $Revision: 3.0 $
   Author: stefan (Last modification by $Author: 2cxl $)

   (C) 1996, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

structure SysDep : SYS_DEP
	
=

struct

    (* from Isabelle --- to be used in Makefiles *)
    fun exportML{init, banner, imagefile} =
      let val runtime = List.hd (SMLofNJ.getAllArgs())
          val suffix  = implode(#"." ::
                                map Char.toLower 
                                (explode (SMLofNJ.SysInfo.getHostArch() ^ "-" 
                                          ^ SMLofNJ.SysInfo.getOSName())));
          (* became necessary sometime recently *)
	  val exec_file = TextIO.openOut imagefile
      in (TextIO.output (exec_file,
			 String.concat
			 ["#!/bin/sh\n",
			  runtime, " @SMLdebug=/dev/null @SMLload=", imagefile,
			  ".heap",suffix, "$*", "\n"]);
	  (*"@SMLdebug=..." sends GC Messages to /dev/null*)
				      
	  TextIO.closeOut exec_file;
	  OS.Process.system ("chmod a+x " ^ imagefile);
          SMLofNJ.exportML (imagefile^".heap");
	  (* --- This code is executed when the image is loaded -- *)
          print(banner^"\n");
	  init()
	 )
	  
      end

    fun setPrintDepth n = (Control.Print.printDepth := n div 2;
			   Control.Print.printLength := n)

   (* Set the terminal to a state suitable for sml_tk. Disables INTR
    * s.t. we can use it to abort functions called from bindings, and 
    * sets up QUIT (CTRL-\) to terminate sml_tk instead. Bits of the
    * following are system-independent, but unfortunately the basis library
    * merely allows you to specfiy signals but not install a handler for them
    * which is bloody useless if you ask me :-) *)

    local open Signals Posix.TTY Posix.TTY.TC in

    fun initTTY abort =
	if Posix.ProcEnv.isatty (Posix.FileSys.stdin) then
	 let (* configure TTY driver to have ^\ generate sigQUIT *)
	     val {iflag, oflag, cflag, lflag, cc, ispeed, ospeed} = 
	                 fieldsOf (getattr Posix.FileSys.stdin)
	     val nuattr= termios {iflag=iflag, oflag=oflag, cflag=cflag, 
				  lflag=lflag, ispeed=ispeed, ospeed=ospeed,
				  cc=V.update(cc, [(V.quit, Char.chr 28)])}
	 in  setattr(Posix.FileSys.stdin, TC.sanow, nuattr);
	     (* install the top level continuation as QUIT signal handler *)
	     setHandler(UnixSignals.sigQUIT, 
			HANDLER (fn _ => (abort();
					  !Unsafe.topLevelCont)));
       	     (* ignore broken pipes, so SML doesn't terminate when wish dies *)
	     setHandler(UnixSignals.sigPIPE, IGNORE);
	     (* ignore interrupts-- they are only enabled (and handled) while
	      * calling functions bound to events *)
	     setHandler(sigINT, IGNORE);
	     (* announce these changes *)
	     print "\nNote: INTR (Ctrl-C) disabled, use QUIT (Ctrl-\\) \
		   \ to terminate sml_tk.\n\n"
	 end
	else
	    ()  (* stdin is not a tty *)
	    
 
   fun resetTTY () =
       if Posix.ProcEnv.isatty Posix.FileSys.stdin then
	   ignore(setHandler(sigINT, inqHandler UnixSignals.sigQUIT);
		  setHandler(UnixSignals.sigQUIT, IGNORE))
       else ()

   (* Wrap an interrupt handler around a function f *)
   fun interruptable f i a =
       SMLofNJ.Cont.callcc 
       (fn c=> let val oldh= setHandler(sigINT, HANDLER (fn _ => 
							 (c before (i()))))
	       in  (f a) before ignore (setHandler(sigINT, oldh))
	       end)	      

   end


   (* This shouldn't be here, but SML/NJ implements Unix.reap incorrectly--
    * it returns Posix.Process.status whereas it should return 
    * OS.Process.Process.status *)
    
    fun exec (s,sl) = 
	let
	    val pr = Unix.execute (s,sl)
	in  case Posix.Process.fromStatus (Unix.reap pr) of 
	     Posix.Process.W_EXITED => true
	   | _                      => false
	end
    
end


