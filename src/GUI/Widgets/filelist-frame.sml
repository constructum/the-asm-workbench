structure FilelistFrame =
struct
  open GUI_Misc
  val debug = debug false "FilelistFrame"
			   
  val frame_id   = mkWidgetId "filelistFrame"

  structure Listbox =
  struct
    val id = mkWidgetId "filelistListbox"

    fun create () =
      Listbox (
	      id, RightScb, [ Side Left, Side Top, Fill Both, Expand true ],
	      GUI_Misc.Listbox.stdConfig (),
	      GUI_Misc.Listbox.stdBindings changePosition )

    and register () =
      StateDepWidget.register_widget (id, fn id => redraw (), [ Filelist.gse_filelist ])
                                       
    and unregister () =
    ( debug "Listbox.destroy";
      StateDepWidget.unregister_widget id )

    and changePosition () =
      let val pos = GUI_Misc.Listbox.setPosition id
      in  Filelist.setSelectedPosition pos;
	        StateDepWidget.full_gui_update ()
      end

    and process_list_item (filename, state) =
      case state of
        Filelist.NotLoaded => "[NOT LOADED] " ^ filename
      | Filelist.Error _ => "[ERROR] " ^ filename
      | Filelist.Loaded _ => filename
			
    and redraw () =
    ( GUI_Misc.Listbox.fill id (map process_list_item (Filelist.getFilelist ())) )
  end

  fun create () =
    let val _ = debug "create ()" []
        fun newButton x = Button (newWidgetId (), [Expand true, Fill Both], x, [])
    in mkframe [ Frame ( frame_id,
                  [ newButton [ Text "   Reload   ", Command Filelist.reloadOldFiles ],
                    newButton [ Text "   Add File   ", Command ASM_GUI_Actions.addFileDialog ],
                    newButton [ Text "   Insert File   ", Command ASM_GUI_Actions.insertFileDialog ],
                    newButton [ Text "   Remove File   ", Command ASM_GUI_Actions.removeFileDialog ] ],
                  [ Side Right, Fill Y ], [], [] ),
                    Listbox.create () ]
    end

  structure SDW = struct
    fun register ()   = Listbox.register ()
    fun redraw ()     = Listbox.redraw ()
    fun unregister () = Listbox.unregister ()
  end
end
