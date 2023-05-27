(* state of the term observation window *)

structure TermObservation =
struct
  open GUI_Misc
  val gse_termlist = GUI_StateElem.register_elem "termlist"
  fun termlist_changed () = GUI_StateElem.set_updated gse_termlist
 
  local
    val InitialTermList = ([] :ParsedTerm.CELL list)
    val TermList = ref InitialTermList
  in
    fun set_term_list L = TermList := L
    fun term_list ()    = (!TermList)

    fun reset ()   = TermList := InitialTermList
    fun reparse () = TermList := map (ParsedTerm.invalidate) (!TermList)

    fun restore (L :string list) =
    ( TermList := map (ParsedTerm.new) L )

(*
    fun read ()  =
    ( TermList := map (ParsedTerm.new) ((GUI_Reader.list GUI_Reader.string_) ()) )
    fun write () =
    ( output (!GUI_Writer.stream, "(* --- Term Observation List --- *)\n");
      (GUI_Writer.list GUI_Writer.string_) (map (ParsedTerm.argument) (!TermList));
      GUI_Writer.newline () )
*)

    fun insert_term_at (t, pos) =
      let val t' = ParsedTerm.new t
	  val _  = ParsedTerm.value (t')  handle ex => (ASM_Interface.asmErrorMsg ex; raise ex)
      in TermList := ( List_.insert_at (t', !TermList, pos)  handle _ => (!TermList @ [ t' ]) )
      end

    fun change_term_at (t, pos) =
      let val t' = ParsedTerm.new t
	  val _  = ParsedTerm.value (t')  handle ex => (ASM_Interface.asmErrorMsg ex; raise ex)
      in TermList := ( List_.replace_at (t', !TermList, pos)  handle _ => (!TermList) )
      end

    fun remove_term_at pos =
      TermList := ( List_.remove_at (!TermList, pos) handle _ => !TermList )

    fun get_term_at pos =
      List.nth (!TermList, pos)
  end
end

