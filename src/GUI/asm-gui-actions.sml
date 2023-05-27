(* ******************************************************************* *\
 *
 *   The ASM Workbench - GUI
 *
 *   Description:  ASM Workbench GUI - Actions
 *
\* ******************************************************************* *)

structure ASM_GUI_Actions =
struct
  open GUI_Misc ASM_GUI_State

  fun reinit () =
  ( ASM.reinit ();
    redraw_upon_asm_state_change () )

  fun possiblyUpdateAfterStateChange () =
  ( if RunOptions.eval_update_cond ()
    then redraw_upon_asm_state_change ()
    else if ASM.currStage () mod 10 = 0
         then redraw_upon_asm_state_change ()
	 else () )

  fun doGotoStage destStage =
    let fun rgoto n =
	  let val stg = ASM.currStage ()
          in if n > stg
	     then ( ASM.step (); possiblyUpdateAfterStateChange (); rgoto n )
	     else if stg > 0 andalso n < stg
	     then ( ASM.back (); possiblyUpdateAfterStateChange (); rgoto n )
	     else ()
	  end
        val currStage = ASM.currStage ()
    in if destStage > currStage
       then ( rgoto (destStage-1); ASM.step (); redraw_upon_asm_state_change () )
       else if destStage < currStage andalso currStage > 0
       then ( rgoto (destStage+1); ASM.back (); redraw_upon_asm_state_change () )
       else ()
    end

  fun back ()      = ASM_Interface.catchAndDismiss (redraw_upon_asm_state_change o ASM.back') ()
  fun step ()      = ASM_Interface.catchAndDismiss (redraw_upon_asm_state_change o ASM.step') ()

  fun gotoStage i      = ASM_Interface.catchAndDismiss doGotoStage i
  fun gotoLastStage () = gotoStage (ASM.lastStage ())

  fun reloadAll () = ASM_Interface.catchAndDismiss (fn _ => Filelist.reloadOldFiles ()) ()


  local
    fun iterate direction initial_stage =
      let val cond = RunOptions.eval_halting_cond ()
		       handle RunOptions.HaltingCond => ( ASM_Interface.simpleMsg
			 "The halting condition is not a well-formed boolean term \n\
			 \  (see: \"Options\" menu, \"Run Options\", \"Halting Condition\")";
			 raise RunOptions.HaltingCond )
          val step_or_back = if direction < 0 then ASM.back else ASM.step
      in ( if (not cond) andalso
              (not (direction < 0 andalso ASM.currStage () = 0)) andalso
              (abs (ASM.currStage () - initial_stage) < RunOptions.iter_limit ())
	   then ( step_or_back (); possiblyUpdateAfterStateChange (); iterate direction initial_stage )
	   else redraw_upon_asm_state_change () )
      end
      handle RunOptions.HaltingCond => ()
  in
    fun backUntilCond () =
    ( let val stage = ASM.currStage ()
      in ASM_Interface.catchAndDismiss (fn () => (ASM.back (); iterate (~1) stage)) ()
      end ) handle _ => ()

    fun stepUntilCond () =
    ( let val stage = ASM.currStage ()
      in ASM_Interface.catchAndDismiss (fn () => (ASM.step (); iterate 1 stage)) ()
      end ) handle _ => ()
  end


  (* *** Load File Dialog ******************************************** *)

  local 
    fun selectAndProcessFile (fileProcessingAction :string -> unit) ()  =
      let fun act_on_file (directory, filename) =
	    let val nameoffile = (directory ^ "/" ^ filename)
	    in if nameoffile = "/"
	       then StdDialog.warning_dialog "No file selected" (selectAndProcessFile fileProcessingAction)
	       else ( print (String_.replace "$1\n$2\n" [ nameoffile, File.mkRelative nameoffile ]);
		      fileProcessingAction (File.mkRelative nameoffile) )
	    end
      in (Filer.enter_file_with_filter "*.asm" act_on_file) handle _ => ()
      end
  in
    fun addFileDialog () = selectAndProcessFile Filelist.addFile ()

    fun insertFileDialog () =
      if Filelist.validPosition ()
      then (selectAndProcessFile Filelist.insertFile) ()
	   handle UndefinedPosition => ()
      else StdDialog.error "Undefined position:\n\
      		           \please select from the list the file\n\
			   \before which the new file should be inserted\n"
			   
    fun removeFileDialog () =
      if Filelist.validPosition ()
      then StdDialog.confirm (String_.replace "Remove file '$1' from list ?" [ Filelist.getSelectedFile () ])
			     Filelist.removeFile
      else StdDialog.error ("No selection:\nplease select the file\nto be removed from the list\n")
  end
end
