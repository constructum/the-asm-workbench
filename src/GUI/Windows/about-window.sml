structure AboutWindow =
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

  fun empty () = cLabel ""

  val text = mkframe false 1 [
    mkframe false 4 [ mkframe true 1 [ mkframe false 4 [
      mkframe false 1 [ cLabel "The ASM Workbench - version 0.99" ],
      mkframe false 1 [ cLabel "Originally developed at Universität Paderborn 1997-2000" ],
      mkframe false 1 [ cLabel "Restored in 2022-2023" ],
      mkframe false 1 [ cLabel "https://github.com/constructum/the-asm-workbench" ]
    ] ] ],
    mkframe false 4 [ mkframe true 1 [ mkframe false 4 [
      mkframe true 2 [ cLabel "Authors" ],
      mkframe true 1 [ mkframe false 2 (map cLabel [
	"Giuseppe Del Castillo (Parser, Type Checker, Interpreter)",
	"Phi Hoang Nguyen & Giuseppe Del Castillo (GUI)"
      ]) ]
    ] ] ],
    mkframe false 4 [ mkframe true 1 [ mkframe false 4 [
      mkframe true 2 [ cLabel "Contact Address" ],
      mkframe true 1 [ mkframe false 2 (map cLabel [
	"asm.workbench@gmail.com"
      ]) ]
    ] ] ],
    mkframe false 4 [ mkframe true 1 [ mkframe false 4 [
      mkframe true 2 [ cLabel "Third-Party Software Included" ],
      mkframe true 1 [ mkframe false 2 (map cLabel [
	"GUI implemented using 'sml_tk', developed at Universität Bremen",
	"by Christoph Lüth, Stefan Westmeier and Burkhart Wolff",
	"http://www.informatik.uni-bremen.de/~clueth/sml_tk"
      ]) ]
    ] ] ],
    empty ()
  ]

  val closeWindowRef = ref (fn () => ())
  fun quitButton () = button (" OK ", fn () => (!closeWindowRef)())

  structure MainWindow = MakeWindow (
    val win_id_str = SOME (mkWinId "aboutWindow")
    fun window id = ( [ WinTitle "About the ASM Workbench" ],
                      [ mkframe false 6 [ text, quitButton () ] ] )
    val (init, postprocess, terminate) = (noAction, noAction, noAction)
    val closeWindowRef = closeWindowRef
  )

  val select = MainWindow.select
end
