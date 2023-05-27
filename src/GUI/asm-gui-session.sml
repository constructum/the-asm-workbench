structure ASM_GUI_Session =
struct
  fun debug s L = GUI_Misc.debug true "ASM_GUI_State" s L


  (* ----------------------------------------------------------------- *)
  (* --  save/restore sessions                                      -- *)

  type SESSION_INFO =
  { fileList   : string list,
    termList   : string list,
    runOptions : string * string * string * int,
    visualOptions : string * string }

  fun save () = ()

  exception LoadSessionFile of string
  fun loadSessionFile (filename :string) :SESSION_INFO =
    if (File.exists filename) andalso (GUI_Misc.readable filename)
    then let open Read FileInputStream
             val f  = (TextIO.openIn filename)
                        handle IO.Io _ => raise LoadSessionFile ("I/O error opening file '"^filename^"'.")
	     val f0 = (newStream f) handle IO.Io _ => raise LoadSessionFile ("I/O error accessing file '"^filename^"'.")
	     val (fileList, f1) = read (list string) f0
	     val (termList, f2) = read (list string) f1
	     val (runOptions, f3) = read (tuple4 (string, string, string, int)) f2
	     val (visualOptions, f4) = read (pair (string, string)) f3
         in (TextIO.closeIn f) handle IO.Io _ => raise LoadSessionFile ("I/O error trying to close file '"^filename^"'.");
            { fileList   = fileList,
              termList   = termList,
              runOptions = runOptions,
              visualOptions = visualOptions }
         end handle ex as Read.ReadError _ => raise LoadSessionFile ("in file '"^filename^"', "^(Read.errorMsg ex))
    else raise LoadSessionFile ("Session file '"^filename^"' not found or not readable.")

  exception SaveSessionFile of string
  fun saveSessionFile (filename :string, session :SESSION_INFO) :unit =
    let open TextIO Write
        val f = openOut filename
        fun write writer = Output.toOpenFile f writer
        fun newlines () = output (f, "\n\n")
    in write (list string) (#fileList session); newlines ();
       write (list string) (#termList session); newlines ();
       write (tuple4 (string, string, string, int)) (#runOptions session); newlines ();
       write (pair (string, string)) (#visualOptions session); newlines ();
       closeOut f
    end handle _ => raise SaveSessionFile filename
            

  val report_bug_msg = "Unrecognized error"

  fun restore () =
  ( let val session = loadSessionFile "asm-wb.cfg"
    in debug "restore () #1" [];
       Filelist.loadNewFiles (#fileList session);
       debug "restore () #2" [];
       TermObservation.restore (#termList session);
       RunOptions.restore (#runOptions session);
       VisualOptions.restore (#visualOptions session);
       StateDepWidget.full_gui_update ()
    end
    handle LoadSessionFile message =>
	     ASM_Interface.simpleMsg
               ("WARNING: "^message^"\n\
                \         Starting a new session...\n")
         | ex => (ASM_Interface.simpleMsg report_bug_msg; raise ex)  )
    
  fun save () =
  ( let open RunOptions
    in saveSessionFile ( "asm-wb.cfg",
			 { fileList = map File.mkRelative (Filelist.getFilenames ()),
			   termList = map ParsedTerm.argument (TermObservation.term_list ()),
			   runOptions = (program(), halting_cond(), update_cond(), iter_limit ()),
                           visualOptions =
			     (VisualOptions.visualization_term (), VisualOptions.update_cond ()) } )
    end
    handle SaveSessionFile filename => ASM_Interface.simpleMsg
               ("WARNING: Session file '"^filename^"' could not be properly saved (I/O error).\n")
	   | File.MkRelative s => ASM_Interface.simpleMsg (String_.replace "I/O error: file '$1'\n" [s])
	   | _ => ASM_Interface.simpleMsg report_bug_msg )
end
