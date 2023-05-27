(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/tests+examples/canvas_ex.sml,v $
 
   An Example for Canvasses.

   This example displays a Canvas with three boxes (Rectangle Canvas items)
   on it. With mouse Button 1, one can ``grab'' a box and move it about 
   whilst holding the mouse Button pressed. 

   $Date: 2001/03/30 13:39:31 $
   $Revision: 3.0 $
   Author: cxl (Last modification by $Author: 2cxl $)

   (C) 1996, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)


structure CanvasEx :> 
    sig val go : unit-> string end  =

struct 

  open SmlTk (* SmlTk21 *)


  (* Some parameters *)

  val boxSize      = mkCoord(50, 55)

  val firstBoxPos  = mkCoord(20, 20)
  val secondBoxPos = mkCoord(80, 20)
  val thirdBoxPos  = mkCoord(140, 20)


  val offset       = ref (mkCoord(0, 0))


  (* getBoxPos and moveBoxPos are the functions being bound to
   * pressing mouseButton1, and moving the mouse with the Button pressed,
   * respectively. Note that they are bound directly to the Canvas items, 
   * so we do not need to check which item the mouse is over, but rather
   * bind a closure with the item's id to the corresponding item. 
   *)

  fun getBoxPos wid cid (TkEvent(_,_, x, y, _, _)) =
      let                         
	  val wpos    = readCItemCoords wid cid 
      in 
	 (TextIO.output (TextIO.stdOut, "Grabbed box " ^ mkCItemString(cid) ^ "\n");
	   offset     := subCoord(mkCoord(x,y)) (hd wpos))
      end
  
  and moveBoxPos wid cid (TkEvent(_,_, x, y, _, _)) =
      let
	  val wpos      = readCItemCoords wid cid      
	  val nu_pos    = subCoord(mkCoord(x,y)) (! offset)  
	  val wsize     = subCoord (hd (tl wpos)) (hd wpos)
	  val nu_coords = nu_pos :: (addCoord nu_pos wsize) :: [] 
      in
	 (TextIO.output(TextIO.stdOut, "Moved box "^ mkCItemString(cid)^"\n");
	   setCItemCoords wid cid nu_coords)
      end 

  and boxBindings wid boxId = 
      [ BindEv(ButtonPress (SOME 1),  mkAction(getBoxPos wid boxId)),
        BindEv(ModButton(1, Motion),  mkAction(moveBoxPos wid boxId))
       ]
  and littleBoxes wid =
      let fun oneBox(cid, pos, colour)=CRectangle{citemId=cid, coord1=pos, 
                                                  coord2=addCoord pos boxSize,
                                                  configs=[FillColor colour, 
                                                           Outline Black], 
                                                  bindings=boxBindings wid cid}

      in
	  [
	   oneBox(newCItemId(), firstBoxPos, Red),
	   oneBox(newCItemId(), secondBoxPos, Blue),
	   oneBox(newCItemId(), thirdBoxPos, Green)
	  ]
      end

  (* This defines the Canvas with the three boxes on it *)

  val yeAuldCanvasse =
      let val CanvasId = newWidgetId()
      in Canvas{widId=CanvasId, scrolltype=NoneScb,
		citems=littleBoxes CanvasId,
		packings=[Side Top, Fill X, Expand true],
		configs=[Height 300, Width 400, Relief Groove, 
		 Background (Mix{red=200, blue=240, green=240})],
		bindings=[]}
      end

 
  fun quitButton win = Button{widId=newWidgetId(),
			 packings=[Side Top, Fill X, Expand true],
			 configs=[Text "Quit", Command (mkSimpleAction(fn ()=> closeWindow win))],
			 bindings=[]}
      
  val testwin = 
      let val winid = newWinId()
      in  mkWindow{winId=winid, 
		   config=[WinTitle "Little Boxes",
			   WinAspect(4,3,4,3),
			   WinMinSize(400,300),
			   WinMaxSize(500,400)], 
		   widgets=Pack [yeAuldCanvasse, quitButton winid], 
		   bindings = [],
		   init= noAction}
      end

  fun go () = startTclExn [ testwin ];

end

