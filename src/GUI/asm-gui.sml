(* ****************************************************************** *\
 *
 *   The ASM Workbench for SML/NJ 1.10
 *
 *   File:         asm-gui.sml
 *
 *   Description:  ASM Workbench GUI
 *
 *   Date:         $Date: 2001/04/28 13:14:53 $
 *   Revision:     $Revision: 1.5 $
 *
\* ******************************************************************* *)

structure ASM_GUI =
struct
  open GUI_Misc
  val debug = debug true "ASM_GUI"

  val mainWindowId = mkWinId "main"

  structure Menu   = ASM_GUI_Menus
  structure Action = ASM_GUI_Actions

  val sourceBrowser_id = SourceBrowser.sourceBrowser_id

  (* *** widgets of the main window ********************************** *)

  fun sourceBrowser () = SourceBrowser.create ()
  fun filelistFrame () = FilelistFrame.create ()

  (* *** Error Messages ********************************************** *)

  fun errorBox () = ErrorBox.ErrorBox


  (* *** Board ******************************************************* *)

  fun board () = Frame ( newWidgetId (),
			 [], [Side Left, Fill X], [Width 780], [] )


  (* *** Reset Dialog ************************************************ *)

  fun resetWorkbench () =
  ( Filelist.resetFilelist [];
    StateDepWidget.full_gui_update () )

  fun resetDialog () =
    StdDialog.confirm "Do you really want to reset the ASM Workbench?" resetWorkbench

  (* *** Main Workbench Window *************************************** *)

  fun initMainWorkbenchWindow () =
  ( resetWorkbench ();
    ASM_GUI_Session.restore () )

  fun register_widgets () = ( SourceBrowser.SDW.register ();
                              FilelistFrame.SDW.register ();
                              ErrorBox.SDW.register () )     
  fun redraw_widgets ()   = ( SourceBrowser.SDW.redraw ();
                              FilelistFrame.SDW.redraw ();
                              ErrorBox.SDW.redraw () )
  fun unregister_widgets () = ( SourceBrowser.SDW.unregister ();
                                FilelistFrame.SDW.unregister ();
                                ErrorBox.SDW.unregister () )

  fun init ()        = ( register_widgets (); initMainWorkbenchWindow () )
  fun postprocess () = ( redraw_widgets () )
  fun terminate ()   = ( VisualizationWindow.terminate () handle ex => ();
                         (* closing Main window by Destroy event instead of Quit causes the above function
                            to throw an exception, if the visualization window is open: as there are no further
                            consequences, such an exception is suppressed in order to continue the orderly
                            termination of main window *)
                         unregister_widgets ();
                         StateDepWidget.shutdown () )

  fun quit save_config_flag () =
  ( if save_config_flag then ASM_GUI_Session.save () else ();
    terminate ();    
    closeWindow mainWindowId )

  fun quitDialog () =
    StdDialog.yesNoCancelDialog "Save current session before leaving the ASM Workbench?"
      (quit true)
      (quit false)
      (noAction)

  (* *** Menu Bar **************************************************** *)

  fun MItem (text, action) = MCommand [ Text text, Command action ]

  val mb1 = newWidgetId()
  fun File () = MenuButton (mb1, true, [
    MItem (" Load File ", Action.addFileDialog ),
    MItem (" Reset ",     resetDialog ),
(*  MSeparator, *)
    MItem (" Quit ",      quitDialog )
  ], [Side Left, Fill X], [Text "  File  "], [] )

  val mb2 = newWidgetId()
  fun Options () = MenuButton (mb2, true, [
    MItem ( " Run Options  ",     RunOptionsWindow.select ),
    MItem ( " Observed Terms ",   TermObservationWindow.select ),
(*  ] @ (if (VisualOptions.visualization_enabled ()) then [ *)
    MItem ( " Visualization Options ",	  VisualOptionsWindow.select )
(*  ] else [] *) ], [Side Left, Fill X], [Text " Options "], [] )

  val mb3 = newWidgetId()
  fun Display () = MenuButton (mb3, true, [
    MItem (" Signature ",           SignatureWindow.select ),
    MItem (" Run Observation ",	  ObservationWindow.select ),
(*  ] @ (if (VisualOptions.visualization_enabled ()) then [ *)
    MItem (" State Visualization ", VisualizationWindow.select )
(*  ] else [] *) ], [Side Left, Fill X], [Text " Windows "], [] )

  val mb4 = newWidgetId()
  fun Help () = MenuButton (mb4, true, [
(*  MItem ("ASM-SL Lexical Structure",   HELP_WINDOW.lexical_structure ),
    MItem ("ASM-SL Concrete Syntax",	 HELP_WINDOW.concrete_syntax ),
    MSeparator, *)
    MItem ("About the ASM Workbench...", AboutWindow.select ),
    MItem ("License",		         LicenseWindow.select )
  ], [Side Right, Fill X], [Text " Info "], [] )

  fun Menu () = Frame ( newWidgetId (),
		        [ File (), Display (), Options (), Help () ],
		        [ Side Top, Fill X ], [], [] )

  fun mainWorkbenchWindow () =
    mkWin5 ( mainWindowId,
             [ WinTitle "The ASM Workbench" ],
             [ Menu (),
               sourceBrowser (),
	       filelistFrame (),
	       errorBox (),
               board () ],
	     [ BindEv ( Destroy,
                        fn _ => ( debug "Destroy\noccursWin $1\n" [ Bool.toString (occursWin mainWindowId) ];
                                  terminate () ) ) ],             fn () => (init (); postprocess ()) )


  val _ = ( ASM_Interface.asmErrorMsgRef := ErrorBox.errorMsg )
  val _ = ( ASM_Interface.simpleMsgRef   := ErrorBox.simpleMsg )

  (* *** Start Workbench ********************************************* *)

  fun start ()   = ( SmlTk.init ();
		     startTcl [ mainWorkbenchWindow () ] )
		   handle ex as BasicTypes.TCL_ERROR s => (print s; raise ex)
			| ex as TkTypes.WIDGET s       => (print s; raise ex)
			| ex as TkTypes.CONFIG s       => (print s; raise ex)
			| ex as TkTypes.WINDOWS s      => (print s; raise ex)
			| ex as TkTypes.CITEM s        => (print s; raise ex)
                        | ex => (print "unrecognized exception\n"; raise ex)


  (* *** Functions for Testing *************************************** *)

  fun cd () = ( OS.FileSys.chDir (ASM_Global.ASM_WB_HOME () ^ "/src/GUI") )

  fun WhileLanguage () = ( OS.FileSys.chDir (ASM_Global.ASM_WB_HOME () ^ "/examples/WhileLanguage");
		           start (); cd () )

  fun InstructionSet () = ( OS.FileSys.chDir (ASM_Global.ASM_WB_HOME () ^ "/examples/InstructionSet");
		            start (); cd () )

  fun Graph () = ( OS.FileSys.chDir (ASM_Global.ASM_WB_HOME () ^ "/examples/Graph");
	  	   start (); cd () )

  fun PT_Nets () = ( OS.FileSys.chDir (ASM_Global.ASM_WB_HOME () ^ "/examples/PT_Nets");
		     start (); cd() )

  fun NoSession () = ( OS.FileSys.chDir (ASM_Global.ASM_WB_HOME () ^ "/examples");
		       start (); cd() )

end

