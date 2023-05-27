structure RunOptions =
struct
  local
    val initial_program = ""
    val initial_halting_cond = "false"
    val initial_update_cond = "true"
    val initial_iter_limit = 500

    val program_ref      = ParsedRule.new (initial_program)
    val halting_cond_ref = ParsedBooleanTerm.new (initial_halting_cond)
    val update_cond_ref	 = ParsedBooleanTerm.new (initial_update_cond)
    val iter_limit_ref	 = ref initial_iter_limit
  in
    exception HaltingCond and UpdateCond

    fun reset () = (
      ParsedRule.update (program_ref, initial_program);
      ParsedBooleanTerm.update (halting_cond_ref, initial_halting_cond);
      ParsedBooleanTerm.update (update_cond_ref, initial_update_cond);
      iter_limit_ref := initial_iter_limit
    )

    fun reparse () = (
      ParsedRule.invalidate (program_ref);
      ParsedBooleanTerm.invalidate (halting_cond_ref);
      ParsedBooleanTerm.invalidate (update_cond_ref)
    )

    local open ASM_Interface ParsedRule
    in 
      fun set_program s   = update (program_ref, s)
      fun program ()      = argument (program_ref)
      fun eval_program () =
        if argument (program_ref) = "" then ()
	else ASM_Top.setProgram (value (program_ref))
	     handle ex => ASM_Interface.simpleMsg
	       ( "Program is not a correct rule (see: \"Options\" menu, \"Run Options\", \"Program\")\n"
		 ^ (ASM.errorMessage ex) )
    end
    local open ASM_Interface ParsedBooleanTerm
	  fun equal (x, y) = (ASM_Value.compare (x, y) = EQUAL)
          val FALSE        = ASM_Value.Prim.False
          val TRUE         = ASM_Value.Prim.True
    in 
      fun set_halting_cond s   = update (halting_cond_ref, s)
      fun halting_cond ()      = argument (halting_cond_ref)
      fun eval_halting_cond () = not (equal (ASM.evalTerm' (value (halting_cond_ref)), FALSE)) handle _ => raise HaltingCond

      fun set_update_cond s   = update (update_cond_ref, s)
      fun update_cond ()      = argument (update_cond_ref)
      fun eval_update_cond () = equal (ASM.evalTerm' (value (update_cond_ref)), TRUE) handle _ => raise UpdateCond
    end

    fun set_iter_limit i = iter_limit_ref := i
    fun iter_limit ()    = !iter_limit_ref

    fun restore (program, halting_cond, update_cond, iter_limit) =
    ( set_program program;
      set_halting_cond halting_cond;
      set_update_cond update_cond;
      set_iter_limit iter_limit;
      eval_program () )

(*
    (* precondition: the input stream should be already open *) 
    fun read () =
    ( set_program (GUI_Reader.string_ ());
      set_halting_cond (GUI_Reader.string_ ());
      set_update_cond (GUI_Reader.string_ ());
      set_iter_limit (GUI_Reader.int_ ()) )

    (* precondition: the output stream should be already open *) 
    fun write () =
      ( output (!GUI_Writer.stream, "(* --- Run Options --- *)\n");
	GUI_Writer.string_ (program ()); GUI_Writer.newline ();
	GUI_Writer.string_ (halting_cond ()); GUI_Writer.newline ();
	GUI_Writer.string_ (update_cond ()); GUI_Writer.newline ();
	GUI_Writer.int_ (iter_limit ()); GUI_Writer.newline () )
*)
  end
end
