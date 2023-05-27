(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/com_state_clib.sml,v $
 
   The communication state (tightly coupled version -- tk_inside).
  
   $Date: 2001/03/30 13:39:06 $
   $Revision: 3.0 $
   Author: bu & behrends

   (C) 1998, ALU Freiburg
 
  ************************************************************************** *)



structure ComState : COMSTATE =
struct

  open BasicTypes BasicUtil

  val prelude_tcl = 
"proc Write {msg} {                     \n \
\  toSML $msg                           \n \
\}                                      \n \
\proc WriteSec {tag msg} {              \n \
\  set status [catch {eval $msg} res]   \n \
\  if {$status == 0} {                  \n \
\    toSML  \"$tag $res\"               \n \
\  } else {                             \n \
\    toSML \"ERROR $res\"               \n \
\  }                                    \n \
\}                                      \n \
\proc WriteCmd {tag msg} {              \n \
\  set status [catch {eval $msg} res]   \n \
\  if {$status == 0} {                  \n \
\   toSML \"$tag\"                      \n \
\  } else {                             \n \
\   toSML \"ERROR $res\"                \n \
\  }                                    \n \
\}                                      \n \
\proc WriteM {msg} {                    \n \
\  toSML $msg                           \n \
\  toSML \"EOM\"                        \n \
\}                                      \n "; 

  fun getEnv(ev:SysConf.envVar) = 
      Option.valOf(OS.Process.getEnv (#name ev))
      handle Option.Option=> #default ev

  (* As opposed to the loosely coupled version, the wish does not 
   * posses an input/output stream here, so we only keep a flag here which
   * indicates wether the eventloop should be running or not. 
   *)

  val COM_state	= ref
      {act= false,
       tclProt = NONE : TextIO.outstream Option.option,
       logfile= OS.Process.getEnv(#name SysConf.logfileVar),
       wish=    getEnv(SysConf.wishVar),
       tclInit= " set tcl_prompt1 \"puts -nonewline {} \" \n \
	        \ set tcl_prompt2 \"puts -nonewline {} \" \n ",
       libPath= getEnv(SysConf.libVar),
       tclAnswers = []:TclAnswer list
      }

  fun wishActive() =  #act(!COM_state)
  fun updAct act =
      let val {tclProt, logfile, wish, tclInit, libPath, tclAnswers,...} = !COM_state
      in  COM_state:= {act=act, tclProt=tclProt, logfile=logfile, wish= wish,
                       tclInit= tclInit, libPath= libPath, tclAnswers=tclAnswers} 
      end

  fun getWishProt() = #tclProt (!COM_state)

  fun getLogfilename () = #logfile(!COM_state)
  fun updLogfilename log =
      let val {act, tclProt, wish, tclInit, libPath, tclAnswers,...} = !COM_state
      in  COM_state:= {act=act, tclProt=tclProt, logfile=log, wish= wish,
                       tclInit= tclInit, libPath= libPath, tclAnswers=tclAnswers} 
      end


  fun getWishPath () = #wish(!COM_state)
  fun updWishPath wp =
      let val {act, tclProt, logfile, tclInit, libPath, tclAnswers,...} = !COM_state
      in  COM_state:= {act=act, tclProt=tclProt, logfile=logfile, wish= wp,
                       tclInit= tclInit, libPath= libPath, tclAnswers=tclAnswers} 
      end

  fun getTclInit () = #tclInit(!COM_state)
  fun updTclInit ti =
      let val {act, tclProt, logfile, wish, libPath, tclAnswers,...} = !COM_state
      in  COM_state:= {act=act, tclProt=tclProt, logfile=logfile, wish= wish,
                       tclInit= ti, libPath= libPath, tclAnswers=tclAnswers} 
      end


  fun getLibPath () = #libPath(!COM_state)
  fun updLibPath rp =
      let val {act, tclProt, logfile, wish, tclInit, tclAnswers, ...} = !COM_state
      in  COM_state:= {act=act, tclProt=tclProt, logfile=logfile, wish= wish,
                       tclInit= tclInit, libPath= rp, tclAnswers=tclAnswers} 
      end

  fun getTclAnswersGUI ()    = #tclAnswers(!COM_state)
  fun updTclAnswersGUI nansw =
      let val {act, tclProt, logfile, wish, tclInit, libPath, tclAnswers} = !COM_state
      in  COM_state:= {act=act, tclProt=tclProt, logfile=logfile, wish= wish,
                       tclInit= tclInit, libPath= libPath, tclAnswers=nansw} 
      end


(* getEvent in the sense of Com is a malconception in the sense
   it is used for both: reading values from the tcl-interpreterstate
   as well as "getting Events", i.e. waiting until they occur,
   and passing event-information to SmlTk. The following 
   function attempts to mimick this double character *)
  fun getEvent()  = 
      let val ev = Access.getEvent()  (* read possible value . . . *)
      in  case ev of
	  "\n"=> (Access.doOneEvent(); 
		  (* wait until event occurs, copy it to EventCmd-Buffer*)
		  Access.getEvent()
	          (* get it from there . . . *))
	| _   => ev
      end

  fun eval ps        = Access.eval ps

  fun initWish()     = 
      (Access.init(); updAct true)

  fun closeWish()  = 
      (Access.reset(); updAct false)

  fun initComState() = 
     (Access.reset();
      COM_state:= {act= false, 
		   tclProt= NONE,
		   logfile=getLogfilename (),
		   wish= getWishPath(),
		   tclInit= getTclInit(),
		   libPath= getLibPath(),
		   tclAnswers=[]})



  
end


