(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/com_state_pipe.sml,v $
 
   The communication state (loosely coupled version).
  
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
\  puts  stdout $msg                    \n \
\  flush stdout                         \n \
\}                                      \n \
\proc SWrite {msg val} {                \n \
\  puts  stdout \"$msg $val\"           \n \
\  flush stdout                         \n \
\}                                      \n \
\proc WriteSec {tag msg} {              \n \
\  set status [catch {eval $msg} res]   \n \
\  if {$status == 0} {                  \n \
\    puts stdout \"$tag $res\"          \n \
\  } else {                             \n \
\    puts stdout \"ERROR $res\"         \n \
\  }                                    \n \
\  flush stdout                         \n \
\}                                      \n \
\proc WriteCmd {tag msg} {              \n \
\  set status [catch {eval $msg} res]   \n \
\  if {$status == 0} {                  \n \
\    puts stdout \"$tag\"               \n \
\  } else {                             \n \
\    puts stdout \"ERROR $res\"         \n \
\  }                                    \n \
\  flush stdout                         \n \
\}                                      \n \
\proc WriteM {msg} {                    \n \
\  puts  stdout $msg                    \n \
\  flush stdout                         \n \
\  puts  \"EOM\"                        \n \
\  flush stdout                         \n \
\}                                      \n "; 


  fun getEnv(ev:SysConf.envVar) = 
      Option.valOf(OS.Process.getEnv (#name ev))
      handle Option.Option=> #default ev
	  
  type wish_app = { inp : TextIO.instream, 
		    out : TextIO.outstream, 
		    prot: TextIO.outstream Option.option }
      
  val COM_state	= ref
      {wapp = NONE : wish_app Option.option,
       logfile= OS.Process.getEnv(#name SysConf.logfileVar),
       wish=    getEnv(SysConf.wishVar),
       tclInit= " set tcl_prompt1 \"puts -nonewline {} \" \n \
	        \ set tcl_prompt2 \"puts -nonewline {} \" \n ",
       libPath= getEnv(SysConf.libVar),
       tclAnswers = []:TclAnswer list
      }


  fun getWishData () = 
      Option.valOf (#wapp (!COM_state)) 

  fun wishActive() = Option.isSome (#wapp (!COM_state))

  val getWishIn   = #inp o getWishData
  val getWishOut  = #out o getWishData
  val getWishProt = #prot o getWishData

  fun updWishData nw = 
      let val {wish, logfile, tclInit, libPath, tclAnswers,...} = !COM_state
      in  COM_state := {wapp= nw,
			logfile=logfile, wish= wish, tclInit= tclInit,
			libPath= libPath, tclAnswers=tclAnswers }
      end
	  
  fun getLogfilename () = #logfile(!COM_state)
  fun updLogfilename log =
      let val {wapp, wish, tclInit, libPath, tclAnswers,...} = !COM_state
      in  COM_state:= {wapp=wapp, logfile=log, wish= wish,
                       tclInit= tclInit, libPath= libPath, tclAnswers=tclAnswers} 
      end


  fun getWishPath () = #wish(!COM_state)
  fun updWishPath wp =
      let val {wapp, logfile, tclInit, libPath, tclAnswers,...} = !COM_state
      in  COM_state:= {wapp=wapp, logfile=logfile, wish= wp,
                       tclInit= tclInit, libPath= libPath, tclAnswers=tclAnswers} 
      end

  fun getTclInit () = #tclInit(!COM_state)
  fun updTclInit ti =
      let val {wapp, logfile, wish, libPath, tclAnswers,...} = !COM_state
      in  COM_state:= {wapp=wapp, logfile=logfile, wish= wish,
                       tclInit= ti, libPath= libPath, tclAnswers=tclAnswers} 
      end


  fun getLibPath () = #libPath(!COM_state)
  fun updLibPath rp =
      let val {wapp, logfile, wish, tclInit, tclAnswers, ...} = !COM_state
      in  COM_state:= {wapp=wapp, logfile=logfile, wish= wish,
                       tclInit= tclInit, libPath= rp, tclAnswers=tclAnswers} 
      end

  fun getTclAnswersGUI ()    = #tclAnswers(!COM_state)
  fun updTclAnswersGUI nansw =
      let val {wapp, logfile, wish, tclInit, libPath, tclAnswers} = !COM_state
      in  COM_state:= {wapp=wapp, logfile=logfile, wish= wish,
                       tclInit= tclInit, libPath= libPath, tclAnswers=nansw} 
      end


(*  fun initStream  st = streamToIODesc st; *)

  fun initComState() = 
     (COM_state:= {wapp= NONE,
		   logfile=getLogfilename (),
		   wish= getWishPath(),
		   tclInit= getTclInit(),
		   libPath= getLibPath(),
		   tclAnswers=[]})


  fun initWish () =
      let
	  val (inp, out)= FileUtil.execute (getWishPath (),[])
	  val prot = Option.map TextIO.openOut (getLogfilename())
      in  updWishData (SOME {inp= inp, out= out, prot= prot})
      end


  fun getEvent () = 
      valOf (TextIO.inputLine(getWishIn()))
      handle Option.Option => "" (* wish has been closed in the meantime *) 
  
  fun eval ps =  
      let val out= getWishOut()
      in  TextIO.output(out, ps ^ "\n"); TextIO.flushOut(out)
      end handle Option.Option => () (* wish has been closed in the meantime *)


  fun closeWish () =
      let val {inp, out, ...}= getWishData()
      in (TextIO.closeIn inp; 
	  TextIO.closeOut out;
	  updWishData NONE)
      end handle _ => updWishData NONE;
	       
(* dummy functions (they are used in integrated version) 
       to keep the code consistent *)

  fun doOneEvent() = 1          (* why not 2 ?!? *)
  fun doOneEvent_nowait() = 1   (* dito  *)
  fun reset_tcl_interp() = ();


end


