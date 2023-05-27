structure RunOptionsWindow =
struct
  open GUI_Misc

  val setProgram_widId  = mkWidgetId "runOptionsSetProgram"
  val haltingCond_widId = mkWidgetId "runOptionsHaltingCond"
  val updateCond_widId  = mkWidgetId "runOptionsUpdateCond"
					 
  val pack_ttB = [ Side Left, Side Top, Expand false, Fill X ]
  val pack_lfX = [ Side Left, Expand false, Fill X ]

  fun mkframe L =
    Frame (newWidgetId (), L, [ Side Top, Expand true, Fill Both ], [Relief Ridge, Borderwidth 2], [])

  fun mkfixframe L =
    Frame (newWidgetId (), L, [ Side Top, Expand false, Fill Both ], [Relief Ridge, Borderwidth 2], [])

  val setProgramFrame =
    mkframe [ Label (newWidgetId (), pack_ttB, [ Text "Program: " ], []),
	      stdTextWid (setProgram_widId, [ Width 60, Height 5 ]) ]

  val updateCondFrame =
    mkframe [ Label (newWidgetId (), pack_ttB, [ Text "Update Condition:" ], []),
	      stdTextWid (updateCond_widId, [ Width 60, Height 5 ]) ]

  val haltingCondFrame =
    mkframe [ Label (newWidgetId (), pack_ttB, [ Text "Halting Condition:" ], []),
	      stdTextWid (haltingCond_widId, [ Width 60, Height 5 ]) ]

  val iterLimitLabel = Label (newWidgetId (), pack_lfX, [ Text "Iteration limit: " ], [])
  val (iterLimitEntry, _, readIterLimit, updateIterLimitEntry) =
    Entry.int_ ((0, 99999999), pack_lfX, fn _ => ())
  val iterLimitFrame = mkfixframe [ iterLimitLabel, iterLimitEntry ]

  fun get_data () =
    let open RunOptions
    in set_program (String_.trim (readTextAll setProgram_widId));
       set_update_cond (String_.trim (readTextAll updateCond_widId));
       set_halting_cond (String_.trim (readTextAll haltingCond_widId));
       set_iter_limit (readIterLimit ());
       eval_program ()
    end

  fun redraw_widgets () =
  ( insertNewText setProgram_widId (RunOptions.program ());
    insertNewText haltingCond_widId (RunOptions.halting_cond ());
    insertNewText updateCond_widId (RunOptions.update_cond ());
    updateIterLimitEntry (RunOptions.iter_limit ()) )

  val updateButton  = RightButton ("Update", get_data)

  val closeWindowRef = ref (fn () => ())
  fun quitButton () = RightButton (" Close ", !closeWindowRef)

  structure MainWindow = MakeWindow (
    val win_id_str = SOME (mkWinId "runOptionsWindow")
    fun window id = ( [ WinTitle "Run Options" ],
                      [ setProgramFrame, haltingCondFrame, updateCondFrame, iterLimitFrame,
                        quitButton (), updateButton ] )
    val (init, postprocess, terminate) = (noAction, redraw_widgets, noAction)
    val closeWindowRef = closeWindowRef
  )

  val select = MainWindow.select
end
