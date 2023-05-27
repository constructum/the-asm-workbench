structure SourceBrowser =
struct
  open GUI_Misc
  val debug = debug false "SourceBrowser"

  val sourceBrowser_id = mkWidgetId "sourceBrowser"

  fun create () =
  ( debug "create ()" [];
    Browser.create (sourceBrowser_id, "", [ Height 32, Width 80 ]) )

  fun redraw widId = Browser.load (widId, Filelist.getSelectedFile ())
  
  structure SDW = MakeStateDepWidget (
    val widget = create ()
    val redraw = redraw
    val dependencies = [ Filelist.gse_filelist, Filelist.gse_filelist_pos ]
  )
                                     
(*  
  fun create () =
    let val _ = 
	val 
    in StateDepWidget.register_widget
	 ( sourceBrowser_id,
	   
	    );
       widget
    end

  fun destroy () =
  ( debug "Listbox.destroy";
    StateDepWidget.unregister_widget sourceBrowser_id )
*)
end
