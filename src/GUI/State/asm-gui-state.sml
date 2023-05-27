structure ASM_GUI_State =
struct
  val gse_asm_state = GUI_StateElem.register_elem "asm_state"
  fun asm_state_changed () = GUI_StateElem.set_updated gse_asm_state
  val full_gui_update = StateDepWidget.full_gui_update
  fun redraw_upon_asm_state_change () = ( asm_state_changed (); full_gui_update () )
end

