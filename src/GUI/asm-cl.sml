(* ******************************************************************* *\
 *
 *   The ASM Workbench for SML/NJ 1.10
 *
 *   File:         asm-cl.sml
 *
 *   Description:  ASM Workbench: Command-Line UI
 *
 *   Date:         $Date: 2001/04/28 13:14:53 $
 *   Revision:     $Revision: 1.5 $
 *
\* ******************************************************************* *)

structure ASM_WB_CL =
struct
  fun ASM_WB_Usage () =
  ( print
      "\n\
      \Usage: asm-wb [OPTION]... [SERVICE] [FILE]...\n\n\
      \Calling \"asm-wb\" without the service parameter starts the ASM Workbench GUI.\n\n\
      \Services:\n\
      \  -check       type-check the files\n\
      \  -version     print version information\n\n\
      \Options:\n\
      \  -oracle filename  use program 'filename' as oracle process\n\
      \  -random           use random choice for external functions\n\n\
      \Please report bugs on https://github.com/constructum/the-asm-workbench \n\
      \      or by e-mail to asm.workbench@gmail.com\n\n" )

(*      \Services:\n\ *)
(*    \  -signatures  check the files and output signatures\n\ *)

(*      \  -no_history       do not store computation history\n\ *)
(*      \  -no_memo          do not memoize static functions\n\n\ *)



  fun ASM_WB_Version () =
  ( print ( String_.replace
      "The ASM Workbench - Version 1.0-pre, compiled on $1\n\
      \Copyright (c) 1997-2001 Paderborn University\n"
      [ Date.toString (Date.fromTimeLocal (Time.now ())) ] ) )

  local
    fun forEachFile (F_pre, F_post) filenames =
      let fun forOneFile (sgn, fileName) =
	let val _ = F_pre (sgn, fileName);
            val deltaSgn = ASM.checkDefFileWithSign' sgn fileName
	in F_post (fileName, deltaSgn);
	   ASM_Signature.override (sgn, deltaSgn)
	end
      in List_.foldll (ASM.catch forOneFile) (!ASM_Top.sign) filenames; ()
      end

    fun checkOnlyPre (sgn, fileName)      = ( print (fileName ^ "\n") )
    fun checkOnlyPost (fileName, deltaSgn) = ()

(*
    fun printSignature (filename, D, sign) =
    ( ASM.print_filtered_signature sign (ASM.AST.get_def_name D); print "\n" )
*)
  in
    fun ASM_WB_Check filenames      = forEachFile (checkOnlyPre, checkOnlyPost) filenames
(*
    fun ASM_WB_Signatures filenames = forEachFile printSignature filenames (* check_files signatures filenames *)
*)

    fun ASM_WB_Signatures _ = ()
  end




  fun ASM_WB_Init ()     = ( ASM.init () )
  fun ASM_WB_GUI ()      = ( ASM_GUI.start () )
  fun ASM_WB_Shutdown () = ( ASM.shutdown () )

(*
  val _ =
    let open SMLofNJ.Internals.CleanUp
        fun ASM_WB_Cleaner AtInitFn = ASM_WB_Init ()
          | ASM_WB_Cleaner AtExit   = ASM_WB_Shutdown ()
          | ASM_WB_Cleaner _        = ()
    in addCleaner ("ASM_Workbench", [ AtInitFn, AtExit ], ASM_WB_Cleaner)
    end
*)


  local
    fun printErr s = TextIO.output (TextIO.stdErr, s)
    fun detailed_IO_errMsg (cause :exn) =
      case cause of
        OS.SysErr (s, _) => s
      | Subscript => "subscript"
      | IO.BlockingNotSupported  => "blocking not supported"
      | IO.NonblockingNotSupported => "non blocking not supported"
      | IO.TerminatedStream => "terminated stream"
      | IO.ClosedStream => "closed stream"
      | _ => "???"
  in
    fun ASM_Workbench (progName, args) :OS.Process.status =
    ( let exception Exit
	  fun error () = ( ASM_WB_Usage (); raise Exit )
          fun set_oracle_process s =
	    ( ASM.catch ASM_Oracle.start_oracle s )
          fun set_random_choice () =
            ( ASM_Run.setOracleOption ASM_Run.RandomChoice )
	  fun set_options args =
	    case args of
(*	      "-no_memo" :: rest      => ((*!!!ASM_History.memo_flag := false;*) set_options rest)
	    | "-no_history" :: rest   => ((*!!!ASM_History.save_history := false;*) set_options rest)
	    | *)
              "-random" :: rest       => (set_random_choice (); set_options rest)
	    | "-oracle" :: s :: rest  => (set_oracle_process s; set_options rest)
	    | _			  => args
      in ( case (set_options args) of
	     []                          => ASM_WB_GUI ()
	   | "-check" :: filenames       => ASM_WB_Check filenames
  (*	   | "-signatures" :: filenames  => ASM_WB_Signatures filenames *)
	   | "-version" :: _             => ASM_WB_Version ()
	   | _                           => error ();
	 ASM_WB_Shutdown ();
         OS.Process.success )
	 handle Exit => ( ASM_WB_Shutdown (); OS.Process.success )
      end
      handle IO.Io {name, function, cause} =>
	     ( printErr (String_.replace "I/O Error: $1 / $2 / $3\n"
			                 [ name, function, detailed_IO_errMsg cause ]);
               ASM_WB_Shutdown ();
               OS.Process.failure )
	   | ex =>
             ( printErr "\nASM-WB: Abnormal termination...\n"; raise ex   (* OS.Process.failure*) ) )
  end
end
