structure VisualOptions =
struct
  val gse_visual_options = GUI_StateElem.register_elem "visual_options"

  local
    structure ParsedVPrimitiveListTerm =
      ParsedTermWithTypeConstraint ( struct
        val T = ASM_Type.List (ASM_Type.BaseType ("V_PRIMITIVE", []))
      end )

    val initial_visualization_term = "[]"
    val initial_update_cond = "true"

    val visualization_term_ref = ParsedVPrimitiveListTerm.new (initial_visualization_term)
    val update_cond_ref	 = ParsedBooleanTerm.new (initial_update_cond)
  in
    exception VisualizationTerm and UpdateCond

    fun reset () = (
      ParsedVPrimitiveListTerm.update (visualization_term_ref, initial_visualization_term);
      ParsedBooleanTerm.update (update_cond_ref, initial_update_cond)
    )

    fun reparse () = (
      ParsedVPrimitiveListTerm.invalidate (visualization_term_ref);
      ParsedBooleanTerm.invalidate (update_cond_ref)
    )

    local open ASM_Interface
	  fun equal (x, y) = ASM_Value.compare (x, y) = EQUAL
    in 
      fun set_visualization_term s   = ParsedVPrimitiveListTerm.update (visualization_term_ref, s)
      fun visualization_term ()	     = ParsedVPrimitiveListTerm.argument (visualization_term_ref)
      fun eval_visualization_term () = (ASM.evalTerm (ParsedVPrimitiveListTerm.value (visualization_term_ref)))
				       handle _ => raise VisualizationTerm

      fun set_update_cond s   = ParsedBooleanTerm.update (update_cond_ref, s)
      fun update_cond ()      = ParsedBooleanTerm.argument (update_cond_ref)
      fun eval_update_cond () = (equal (ASM.evalTerm (ParsedBooleanTerm.value (update_cond_ref)), ASM_Value.BOOL true))
				handle _ => raise UpdateCond
    end

    fun restore (vis_term, vis_update_cond) =
    ( set_visualization_term vis_term;
      set_update_cond vis_update_cond; () )

(*
    (* precondition: the input stream should be already open *) 
    fun read () =
    ( set_visualization_term (GUI_Reader.string_ ());
      set_update_cond (GUI_Reader.string_ ()) )

    (* precondition: the output stream should be already open *) 
    fun write () =
      ( output (!GUI_Writer.stream, "(* --- Visualization Options --- *)\n");
        GUI_Writer.string_ (visualization_term ()); GUI_Writer.newline ();
	GUI_Writer.string_ (update_cond ()); GUI_Writer.newline () )
*)
  end


  (* ***************************************************************** *)

  exception VisualFileError of string
  fun do_load_visual_file () =
    let val ASM_WB_HOME = ASM_Global.ASM_WB_HOME ()
	val VISUAL_ASM_FILE = ASM_WB_HOME ^ "/bin/visual.asm"
    in ( if File.exists VISUAL_ASM_FILE then () else raise VisualFileError "not found";
	 (ASM.loadFile' VISUAL_ASM_FILE) handle _ => raise VisualFileError "not correct"; () )
(* !!!!!!!!!!!!!!!         
       handle VisualFileError kind_of_problem =>
       ( ErrorBox.simpleMsg ("WARNING: File \"" ^ VISUAL_ASM_FILE ^ "\" " ^ kind_of_problem ^ ". \
			     \Visualization feature disabled!\n");
	 raise VisualFileError kind_of_problem )
*)
    end

  val enable_visualization = ref false
  fun visualization_enabled () = (!enable_visualization)

  fun load_visual_file () =
  ( enable_visualization := true;
    (do_load_visual_file ()) (* handle _ => enable_visualization := false *) )
end
