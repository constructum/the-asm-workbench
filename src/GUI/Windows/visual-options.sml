structure VisualOptionsWindow =
struct
  open GUI_Misc

  val visualizationTerm_widId = mkWidgetId "visualOptionsVisualizationTerm"
  val updateCond_widId        = mkWidgetId "visualOptionsUpdateCond"
					   
  val pack_ttB = [ Side Left, Side Top, Expand false, Fill X ]
  val pack_lfX = [ Side Left, Expand false, Fill X ]

  fun mkframe L =
    Frame (newWidgetId (), L, [ Side Top, Expand true, Fill Both ], [Relief Ridge, Borderwidth 2], [])

  fun mkfixframe L =
    Frame (newWidgetId (), L, [ Side Top, Expand false, Fill Both ], [Relief Ridge, Borderwidth 2], [])

  val visualizationTermFrame =
    mkframe [ Label (newWidgetId (), pack_ttB, [ Text "Visualization Term: " ], []),
	      stdTextWid (visualizationTerm_widId, [ Width 60, Height 5 ]) ]

  val updateCondFrame =
    mkframe [ Label (newWidgetId (), pack_ttB, [ Text "Update Condition:" ], []),
	      stdTextWid (updateCond_widId, [ Width 60, Height 5 ]) ]

  fun get_data () =
    let open VisualOptions
    in set_visualization_term (String_.trim (readTextAll visualizationTerm_widId));
       set_update_cond (String_.trim (readTextAll updateCond_widId));
       GUI_StateElem.set_updated gse_visual_options;
       StateDepWidget.full_gui_update ()
    end

  fun update_widgets () =
  ( insertNewText visualizationTerm_widId (VisualOptions.visualization_term ());
    insertNewText updateCond_widId (VisualOptions.update_cond ()) )

  val updateButton  = RightButton ("Update", get_data)

  val closeWindowRef = ref (fn () => ())
  val quitButton = RightButton (" Quit ", fn () => (!closeWindowRef)())

  structure MainWindow = MakeWindow (
    val win_id_str = SOME (mkWinId "visualOptionsWindow")
    fun window id = ( [ WinTitle "Visualization Options" ],
                      [ visualizationTermFrame, updateCondFrame, quitButton, updateButton ] )
    val (init, postprocess, terminate) = (noAction, update_widgets, noAction)
    val closeWindowRef = closeWindowRef
  )

  open MainWindow
end
