functor MakeListbox (
  ListBoxInfo :sig
    type ELEM
    val collect_data  :unit -> ELEM list
    val configure     :SmlTk.Configure list
    val display	      :ELEM -> string
    val default_elem  :ELEM			
  end ) =
struct
  open GUI_Misc ListBoxInfo

  val id = mkWidgetId "listbox"

  local
    val data = ref (collect_data ())
    val pos  = ref noPosition
  in
    fun valid_pos n     = ( n >= 0 andalso n < length (!data) )
    fun normalize_pos n = if not (valid_pos n) then noPosition else n
    fun selection ()	= valid_pos (!pos)

    fun when_selected f args =
      if selection () then f args else StdDialog.error "No selection!"

    fun cursor () = (fn Mark (n, _) => n | _ => ~1) (readCursor id)

    fun set_pos () = pos := normalize_pos (cursor ())

    fun refresh () =
    ( clearText id;
      map ((insertTextEnd id) o display) (!data);
      () )

    fun update () =
    ( data := collect_data ();
      refresh () )

    fun at i = if (valid_pos i) then List.nth (!data, i) else default_elem

    val action_left_mouse  = ref (fn (_: TkEvent) => (set_pos ()))
    val action_right_mouse = ref (fn (_: TkEvent) => ())
    val action_key_down  = ref (fn (_: TkEvent) => ())
    val action_key_up  = ref (fn (_: TkEvent) => ())

    val widget = Listbox (
      id, RightScb, [ Side Left, Side Top, Fill Both, Expand true ],
      [ Relief Raised, Font ASM_GUI_Fonts.listboxFont ] @ configure, 
      [ BindEv (ButtonPress (SOME 1), fn event => (!action_left_mouse) event),
	BindEv (ButtonPress (SOME 3), fn event => (!action_right_mouse) event),
        BindEv (KeyPress "Up", fn event => (!action_key_up) event),
	BindEv (KeyPress "Down", fn event => (!action_key_down) event) ]
    )

    fun curr_pos ()  = !pos
    fun curr_data () = !data
  end
end
