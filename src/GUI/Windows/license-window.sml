structure LicenseWindow =
struct
  open GUI_Misc

  fun button (text, comm) =
    Button (newWidgetId (), [], [ Text text, Command comm ], [])

  fun mkframe relief margin L =
    Frame ( newWidgetId (), L, [ Side Top, Fill X ],
           (if relief then [Relief Groove] else []) @ [Borderwidth margin],
	    [] )

  val pack_ttB = [ Side Top, Expand true, Fill X ]
  val pack_btB = [ Side Top, Expand true, Fill X ]
  val pack_lfX = [ Side Left, Expand false, Fill X ]

  fun cLabel text = Label (newWidgetId (), pack_ttB, [ Anchor Center, Text text ], [])
  fun lLabel text = Label (newWidgetId (), pack_ttB, [ Anchor West, Text text ], [])

  fun empty () = cLabel ""

  val textWidgetId = mkWidgetId "licenseBox"

  val text = mkframe false 1 [
    mkframe false 4 [ mkframe true 1 [ mkframe false 4 ( 
                                         [ TextWid ( textWidgetId, RightScb, AnnoText (NONE, "", []),
	                                             [Side Top, Fill Both, Expand true],
	                                             [Width 70, Height 12, Font ASM_GUI_Fonts.textWidFont], [] ) ]
    ) ] ],
    empty ()
  ]
                        
  fun insertText () =
    ( insertTextEnd textWidgetId
        "The ASM Workbench is licensed under the\n\
        \GNU Lesser General Public License v2.1 (LGPL 2.1)\n\
        \https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.en\n\
        \\n\
        \see also:\n\
        \https://github.com/constructum/the-asm-workbench/blob/main/LICENSE"  (*;
      setTextWidReadOnly textWidgetId true*) )


  val closeWindowRef = ref (fn () => ())
  fun quitButton () = button (" OK ", fn () => (!closeWindowRef)())

  structure MainWindow = MakeWindow (
    val win_id_str = SOME (mkWinId "licenseWindow")
    fun window id = ( [ WinTitle "License" ],
                      [ mkframe false 6 [ text, quitButton () ] ] )
    val (init, postprocess, terminate) = (noAction, insertText, noAction)
    val closeWindowRef = closeWindowRef
  )

  val select = MainWindow.select
end
