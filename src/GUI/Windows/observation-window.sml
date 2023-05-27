structure ObservationWindow =
struct
  open ASM_Value ASM_Run GUI_Misc
  structure Action = ASM_GUI_Actions
                       
  (* simplified functor, because most widgets in this module
     (except the terms box, which also depends on 'gse_termlist')
     are dependent on 'gse_asm_state' as well as 'gse_filelist'
     (i.e. if the ASM spec is changed / reloaded)  *)
  functor MakeSDW (MakeStateDepWidgetInfo :sig
    val widget        :SmlTk.Widget
    val redraw        :SmlTk.WidId -> unit
  end) = MakeStateDepWidget (
    val widget        = MakeStateDepWidgetInfo.widget
    val redraw        = MakeStateDepWidgetInfo.redraw
    val dependencies  = [ ASM_GUI_State.gse_asm_state, Filelist.gse_filelist ]
  )
					                         
  val (replace, newline) = (String_.replace, fn s => s ^ "\n")
                  
  fun collect_external_state () = ExternalState.toList (ASM_Top.lastExternalState ())
  fun collect_updates () = UpdateSet.toList (ASM_Top.lastUpdateSet ())
  fun collect_terms () = TermObservationWindow.collect_from_term_list ()
                                                                      
  fun mkframe L =
      Frame (newWidgetId (), L, [ Side Top, Expand true, Fill Both ], [Relief Ridge, Borderwidth 2], [])
            
  (* *** External State Frame **************************************** *)
            
  structure ExternalState = struct

    structure Box = MakeSDW (
      val widget =
          TextWid ( mkWidgetId "externalStateBox",
	            RightScb, AnnoText (NONE, "", []), [Side Top, Fill Both, Expand true],
	            [Width 50, Height 12, Font ASM_GUI_Fonts.textWidFont], [] )
                  
      fun redraw id = replaceText
                        id ( String.concat (map (newline o ASM_Run.ExternalStateElem.toString)
				                (collect_external_state ())) )
    )
                            
    structure StageLabel = MakeSDW (
      val widget =
          Label ( mkWidgetId "externalStateLabel",
	          [ Side Right, Expand false, Fill X, PadX 2, PadY 2 ], [ Text "" ], [] )
	        
      fun redraw id =
          let val state = ASM.currStage ()
          in Label.redraw id (
	      if ASM.currStage () = 0 then ""
	      else replace "(stage $1)" [ Int.toString (ASM.currStage () - 1) ] )
          end
    )

    fun frame () = mkframe [
          Frame ( newWidgetId (),
	          [ Label (newWidgetId (), [ Side Left, Expand false, Fill X, PadX 2, PadY 2 ],
		           [ Text "External state" ], []),
	            StageLabel.widget ],
	          [ Side Top, Expand false, Fill Both ], [], [] ),
          Box.widget ]
                           
    fun register ()   = ( Box.register ();   StageLabel.register () )
    fun redraw ()     = ( Box.redraw ();     StageLabel.redraw () )
    fun unregister () = ( Box.unregister (); StageLabel.unregister () )
  end
                              
  (* *** Update Set Frame ******************************************** *)

  structure UpdateSet =
  struct
    structure Box = MakeSDW (
      val widget =
          TextWid ( mkWidgetId "updateSetBox", RightScb, AnnoText (NONE, "", []),
	          [Side Top, Fill Both, Expand true],
	          [Width 50, Height 12, Font ASM_GUI_Fonts.textWidFont], [] )

      fun redraw id = TextWid.redraw
                        id (String.concat (map (newline o ASM_Run.Update.toString) (collect_updates ())))
    )
    structure StageLabel = MakeSDW (
      val widget = Label ( mkWidgetId "updateSetStageLabel",
                           [ Side Right, Expand false, Fill X, PadX 2, PadY 2 ], [ Text "" ], [])
      fun redraw id =
        let val stage = ASM.currStage ()
        in Label.redraw id (
	    if stage = 0 then ""
	    else replace "(stage $1 -> stage $2)" (map Int.toString [ stage-1, stage ]) )
        end
    )
    fun frame () =
        mkframe [
          Frame ( newWidgetId (),
                  [ Label (newWidgetId (), [ Side Left, Expand false, Fill X, PadX 2, PadY 2 ],
		           [ Text "Update set" ], [] ),
	            StageLabel.widget ],
                [ Side Top, Expand false, Fill Both ], [], [] ),
          Box.widget ]

    fun register ()   = ( Box.register ();  StageLabel.register () )
    fun redraw ()     = ( Box.redraw ();    StageLabel.redraw () )
    fun unregister () = ( Box.unregister (); StageLabel.unregister () )
  end
        
  (* *** Term Observation Frame ************************************** *)

  structure Terms =
  struct

    structure Box = MakeStateDepWidget (
      val widget =
          TextWid ( mkWidgetId "termsBox", RightScb, AnnoText (NONE, "", []),
	          [Side Top, Fill Both, Expand true],
	          [Width 50, Height 12, Font ASM_GUI_Fonts.textWidFont], [] )

      fun redraw id =
        let fun display_one_term (t, res) = if res <> "" then (t ^ " = " ^ res ^"\n") else (t ^ ": ERROR!\n")
        in replaceText id (String.concat (map display_one_term (collect_terms ())))
        end
      val dependencies = [ASM_GUI_State.gse_asm_state, Filelist.gse_filelist, TermObservation.gse_termlist]
    )

    structure StageLabel = MakeSDW (
      val widget = Label ( mkWidgetId "termsStageLabel", [ Side Right, Expand false, Fill X (*, PadX 2, PadY 2*) ], [ Text "(stage 0)" ], [] )
      fun redraw id = Label.redraw id (String_.replace "(stage $1)" [ Int.toString (ASM.currStage ()) ])
    )

    fun frame () =
        mkframe [ Frame ( newWidgetId (),
                          [ Button ( newWidgetId (), [ Side Left, Expand false, Fill X, PadX 2, PadY 2 ],
                                     [ Text " Observed Terms ", Command TermObservationWindow.select ], []),
(*
                        [ Label ( newWidgetId (), [ Side Left, Expand false, Fill X, PadX 2, PadY 2 ],
		                  [ Text "Observed terms" ], [] ), *)
	                  StageLabel.widget ],
                        [ Side Top, Expand false, Fill Both ], [], [] ),
                Box.widget ]

    fun register ()   = ( Box.register ();   StageLabel.register () )
    fun redraw ()     = ( Box.redraw ();     StageLabel.redraw () )
    fun unregister () = ( Box.unregister (); StageLabel.unregister () )
  end        
        
  (* *** Left Column ************************************************* *)
        
  fun leftColumn () =
      Frame ( newWidgetId (),
              [ ExternalState.frame (), UpdateSet.frame () ],
              [ Expand false, Side Left, Fill Both ], [], [] )

  (* *** Stage Indicator ********************************************* *)

  structure CurrStageEntry = MakeSDW (
    val (widget, _, _, redraw_int) =
        Entry.int_ ((0, 99999999), pack_rfX, Action.gotoStage)
    fun redraw id = redraw_int (ASM.currStage ())
  )
  structure LastStageLabel = MakeSDW (
    val widget = Label (mkWidgetId "lastStageLabel", pack_rfX, [ Text (Int.toString (ASM.lastStage ())) ], [])
    fun redraw id = Label.redraw id (Int.toString (ASM.lastStage ()))
  )
                                        
  (* *** Button Bar ************************************************** *)

  fun discardFuture () = ( ASM.discardFuture (); LastStageLabel.redraw () )
                           
  val closeWindowRef = ref (fn () => ())

  fun buttonBar id =
      let exception InvalidStage
          fun button (wid_id, text, command) =
	      Button (wid_id, [ Side Left, Fill X ], [ Text text, Command command ], [])
          fun buttonR (wid_id, text, command) =
	      Button (wid_id, [ Side Right, Fill X ], [ Text text, Command command ], [])
	  val currStageLabel   = Label (newWidgetId (), pack_rfX, [ Text "   Stage: " ], [])
	  val untilStageLabel   = Label (newWidgetId (), pack_rfX, [ Text " / " ], [])
	  fun emptyLabelL s = Label (newWidgetId (), pack_lfX, [ Text s ], [])
	  fun emptyLabelR s = Label (newWidgetId (), pack_rfX, [ Text s ], [])
      in Frame ( newWidgetId (),
	         [ button (newWidgetId (),     " | << ",   Action.reinit),
	           button (newWidgetId (),     " <..< ",   Action.backUntilCond),
                   button (newWidgetId (),     "  <<  ",   Action.back),
	           button (newWidgetId (),     "  >>  ",   Action.step),
	           button (newWidgetId (),     " >..> ",   Action.stepUntilCond),
	           button (newWidgetId (),     " >> | ",   Action.gotoLastStage),
                   emptyLabelL "            ",
	           button (newWidgetId (), " Close ",      fn () => (!closeWindowRef)()),
                   emptyLabelL "  ",
	           button (newWidgetId (), " Run Options ", RunOptionsWindow.select),
	           emptyLabelR " ", LastStageLabel.widget, untilStageLabel, CurrStageEntry.widget, currStageLabel,
                   emptyLabelR " ",
	           buttonR (newWidgetId (), " Discard Future ", discardFuture) ],
	         [ Side Top, Fill X ], [], [] )
      end
        
  (* ***************************************************************** *)

  fun register ()   = ( ExternalState.register ();  UpdateSet.register ();  Terms.register ();
                        CurrStageEntry.register (); LastStageLabel.register () ) 
  fun redraw ()     = ( ExternalState.redraw ();  UpdateSet.redraw ();  Terms.redraw ();
                        CurrStageEntry.redraw (); LastStageLabel.redraw () ) 
  fun unregister () = ( ExternalState.unregister (); UpdateSet.unregister (); Terms.unregister ();
                        CurrStageEntry.unregister (); LastStageLabel.unregister () )

  (* *** Main Window ************************************************* *)
        
  fun windowContents id =
    Frame ( newWidgetId (),
            [ buttonBar id, leftColumn (), Terms.frame () ],
            [ Side Top, Expand true, Fill Both ], [], [] )

  structure MainWindow = MakeWindow (
    val win_id_str = SOME (mkWinId "observationWindow")
    fun window id = ( [ WinTitle "Run Observation Window" ],
		      [ windowContents id ] )
    val (init, postprocess, terminate) = (register, redraw, unregister)
    val closeWindowRef = closeWindowRef
  )
                          
  fun select () = MainWindow.select ()
end
