structure TermObservationWindow =
struct
  open GUI_Misc

  fun redraw_upon_termlist_change () =
  ( TermObservation.termlist_changed ();
    StateDepWidget.full_gui_update () )

  val enterTerm_winId = mkWinId "enterTerm"
  val editTerm_winId  = mkWinId "editTerm"
				
  val enterTermText_widId = mkWidgetId "enterTermText"
  val editTermText_widId  = mkWidgetId "editTermText"

  fun show_term_value t =
    ASM_Value.toString (ASM.evalTerm' t)

  fun collect_from_term_list () =
    let fun show_term_and_result cell =
          (ParsedTerm.argument cell, (show_term_value (ParsedTerm.value cell)) handle _ => "")
    in map show_term_and_result (TermObservation.term_list ())
    end

  structure Listbox =
  struct
    structure Widget = MakeListbox (
      type ELEM = string * string
      val configure = [ Width 80, Height 24 ]
      fun collect_data () = collect_from_term_list ()
      fun display (t, res) = if res <> "" then (t ^ " = " ^ res) else (t ^ ": ERROR!")
      val default_elem = ("", "")
    )
    structure SDW = MakeStateDepWidget (
      val widget = Widget.widget
      val redraw = fn _ => Widget.update ()
      val dependencies = [ ASM_GUI_Actions.gse_asm_state, Filelist.gse_filelist, TermObservation.gse_termlist ]
    )
  end

  (*** Insert & Update Term Windows ***************************************)

  fun termText wid_id =
    TextWid ( wid_id, RightScb, AnnoText (NONE, "", []), [Side Top, Fill Both, Expand true],
	      [Width 60, Height 5, Font ASM_GUI_Fonts.textWidFont ], [] )

  fun insert_term t = 
  ( TermObservation.insert_term_at (String_.trim t, Listbox.Widget.curr_pos ());
    redraw_upon_termlist_change () ) handle _ => ()

  fun change_term t =
  ( TermObservation.change_term_at (String_.trim t, Listbox.Widget.curr_pos ());
    redraw_upon_termlist_change () ) handle _ => ()

  fun insertNewTerm () = LeftExpandButton ("Insert", fn () => insert_term (readTextAll enterTermText_widId))
  fun updateTerm ()    = LeftExpandButton ("Update", fn () => change_term (readTextAll editTermText_widId))
  fun clearTerm wid_id  = LeftExpandButton ("Clear", fn () => clearText wid_id)
  fun quitButton win_id = LeftExpandButton ("Close", fn () => closeWindow win_id)

  fun enter_term_quitter () = Frame ( newWidgetId (),
    [insertNewTerm (), clearTerm enterTermText_widId, quitButton enterTerm_winId ],
    [Fill X], [Relief Ridge, Borderwidth 2], [] )

  fun edit_term_quitter () = Frame ( newWidgetId (),
    [updateTerm (), clearTerm editTermText_widId, quitButton editTerm_winId ],
    [Fill X], [Relief Ridge, Borderwidth 2], [] )

  val enter_term_win = mkWin4 ( enterTerm_winId, [ WinTitle "Please enter term" ],
				[termText enterTermText_widId, enter_term_quitter ()], noAction )

  val edit_term_text_wid = termText editTermText_widId

                                   
  fun update_edit_term_text_wid () =
    let val id = selWidgetId edit_term_text_wid
    in clearText id;
       insertTextEnd id ((ParsedTerm.argument (TermObservation.get_term_at (Listbox.Widget.curr_pos ()))) handle _ => "")
    end handle _ => ()

  val edit_term_win = mkWin4 ( editTerm_winId, [ WinTitle "Please update term" ],
			       [edit_term_text_wid, edit_term_quitter ()], noAction )

  val _ = Listbox.Widget.action_left_mouse :=
          (fn (evt: TkEvent) => (Listbox.Widget.set_pos (); update_edit_term_text_wid ()))

  (************************************************************************)

  local
    type ELEM = WinId * WidId * ParsedTerm.CELL
    val closeWindowRef = ref (fn (elem :ELEM) => ())
  in

  structure DisplayTermWindows = CHILD_WINDOWS (
    type ELEM = ELEM   (* WinId * ParsedTerm.CELL*)
    val closeWindowRef = closeWindowRef

    structure TextWidget =
    struct
      fun widget (elem as (_, textwid_id, _)) =
        termText textwid_id
      val dependencies = [ ASM_GUI_Actions.gse_asm_state,
                           Filelist.gse_filelist, TermObservation.gse_termlist ]
      fun redraw (_, textwid_id, term_cell) =
        TextWid.redraw textwid_id
                       ( Listbox.Widget.display (
	                   ParsedTerm.argument term_cell,
	                   (show_term_value (ParsedTerm.value term_cell))
                           handle _ => ""  (* !!! ERROR MESSAGE ? *) ) )
      fun register (elem as (_, textwid_id, _)) =
      ( StateDepWidget.register_widget (textwid_id, fn _ => redraw elem, dependencies); () )
      fun unregister (_, textwid_id, _) = StateDepWidget.unregister_widget textwid_id
    end
     
    fun get_win_id (win_id, _, _) = win_id

    fun contents (elem as (_, textwid_id, term)) =
	[ TextWidget.widget elem,
          RightButton ("Close", fn _ => (!closeWindowRef) elem) ]

    fun init elem = TextWidget.register elem
    fun postprocess elem = TextWidget.redraw elem
    fun terminate elem = TextWidget.unregister elem
  )
					       
  end (* local *)

  (************************************************************************)

  fun insert () = openWindowIfNotOpen enter_term_win
  fun edit ()	= (openWindowIfNotOpen edit_term_win; update_edit_term_text_wid () )
  fun delete () = ( TermObservation.remove_term_at (Listbox.Widget.curr_pos ());
                    redraw_upon_termlist_change () )

  fun display () =
  ( DisplayTermWindows.create (
      [ WinTitle "Display Term" ],
      ( newWinId (),
        mkWidgetId "displayTermTextWid",
        ParsedTerm.new (#1 (Listbox.Widget.at (Listbox.Widget.curr_pos ()))) )
    ) )

  (************************************************************************)

  val closeWindowRef = ref (fn () => ())

  val TermMenu = Frame ( newWidgetId (),
    ( map LeftExpandButton
        [ ("Insert", insert), ("Edit", edit), ("Delete", delete), ("Display", display)] ) @
    [ Label (newWidgetId (), pack_lfX, [ Text "   " ], []),
      LeftExpandButton ("Close", fn () => (!closeWindowRef)()) ],
    [Side Top, Fill X], [], []
  )

  (************************************************************************)

  structure MainWindow = MakeWindow (
    val win_id_str = SOME (mkWinId "termObservationWindow")
    fun window id = ( [ WinTitle "Term Observation Window" ],
                      [ TermMenu, Listbox.SDW.widget ] )
    fun init ()        = Listbox.SDW.register ()
    fun postprocess () = Listbox.SDW.redraw ()
    fun terminate ()   = ( DisplayTermWindows.closeAll (); Listbox.SDW.unregister () )
    val closeWindowRef = closeWindowRef
  )

  fun select () = MainWindow.select ()
end
