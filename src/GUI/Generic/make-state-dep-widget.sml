functor MakeStateDepWidget (
  MakeStateDepWidgetInfo :sig
    val widget        :SmlTk.Widget
    val redraw        :SmlTk.WidId -> unit
    val dependencies  :GUI_StateElem.GSE list
  end
) =
struct
  val widget = MakeStateDepWidgetInfo.widget
  val dependencies = MakeStateDepWidgetInfo.dependencies
  val id = SmlTk.selWidgetId widget

  local open MakeStateDepWidgetInfo
  in fun register () = ( StateDepWidget.register_widget (id, redraw, dependencies); () )
     fun unregister () = StateDepWidget.unregister_widget id
     val redraw = fn () => redraw id
  end                                 
end

