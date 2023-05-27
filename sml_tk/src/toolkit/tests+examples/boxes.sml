(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/tests+examples/boxes.sml,v $
 
   Test and example program for the drag&drop package. 

   It presents the amazed user with a window in which he can move
   around wee blue, red and green boxes. Moving a box on the red box
   makes it go away, moving it onto the green box makes it replicate
   itself. You can't move the green or red box onto anything. In fact,
   you can't move the red box at all. 

   Use WeeBoxes.go() to start. You have to call SysInit.initSmlTk() first,
   and the DragAndDrop functor has to be in the environment.
 
   $Date: 2001/03/30 13:40:01 $
   $Revision: 3.0 $
   Author: cxl (Last modification by $Author: 2cxl $)

   (C) 1996, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)


structure WeeBoxes : sig val go : unit-> string end = 

struct

    open BasicUtil SmlTk

    
    datatype box = redBox of CItemId
	         | greenBox of CItemId
		 | blueBox of CItemId
		 
    fun isBlue (blueBox _) = true
      | isBlue _           = false
     
    val backdrop = newWidgetId()

    (* Bit of a hack, this: this variable will point towards the function
     * exported from DragAndDrop by which we can place items on the
     * drag&drop area.
     *)
    val addNewBoxFun = ref (fn e:box=> ()) 

    val boxSize = 50
    val backdropHeight = 300
    val backdropWidth = 400


    fun debug str = Debug.print 19 ("Boxes: "^str)

    (* This function adds a new box to the area. You can only use it
     * after initializing addNewBoxFun above. 
     *)
    fun addNewBox (boxitem, boxcit) =
	(addCItem backdrop boxcit; (!addNewBoxFun) boxitem)
 

    (* Auxiliary functions to create new Boxes in a format to 
     * use with addNewBox above. 
     *)
    fun newBox colour wHere = 
        CRectangle{citemId=newCItemId(), coord1=wHere, 
                   coord2=addCoord wHere (mkCoord(boxSize, boxSize)), 
                   configs=[FillColor colour, Outline Black, OutlineWidth 3], 
                   bindings=[]}
 
    fun newGreenBox wHere =
        let val nuBox = newBox Green wHere
        in  (greenBox(selItemId(nuBox)), nuBox)
        end
 
    fun newRedBox wHere = 
        let val nuBox = newBox Red wHere
        in  (redBox(selItemId(nuBox)), nuBox)
        end
 
    fun newBlueBox wHere =
        let val nuBox = newBox Blue wHere
        in  (blueBox(selItemId(nuBox)), nuBox)
        end

    (* List of the initial boxes *) 
    val allMyBoxes =
        [newBlueBox (mkCoord(10,10)), newBlueBox (mkCoord(10,80)), newBlueBox (mkCoord(10,150)),
         newGreenBox (mkCoord(10, backdropHeight-10-boxSize)), 
         newRedBox (mkCoord(backdropWidth-10-boxSize, backdropHeight-10-boxSize))] 
 
 
    fun selBoxId (redBox cit)   = cit
      | selBoxId (greenBox cit) = cit
      | selBoxId (blueBox cit)  = cit
	
    fun enlargeBox cit =
	let val coords= readCItemCoords backdrop cit;
	    val nuc   = (subCoord (hd coords) (mkCoord(5, 5))):: 
		        (addCoord (hd (tl coords)) (mkCoord(5, 5))) :: nil
        in  setCItemCoords backdrop cit nuc
	end

    fun shrinkBox cit =
	let val coords= readCItemCoords backdrop cit;
	    val nuc   = (addCoord (hd coords) (mkCoord(5, 5)))::
		        (subCoord (hd (tl coords)) (mkCoord(5, 5))) :: nil
        in  setCItemCoords backdrop cit nuc
	end

    fun colourBox cit colour =
	addCItemConf backdrop cit [FillColor colour]

    fun hilightBox (redBox cit)   = enlargeBox cit
      | hilightBox (greenBox cit) = shrinkBox cit
      | hilightBox (blueBox cit)  = colourBox cit Yellow

    fun enterBox box boxes =
	let val cinDrop = List.all isBlue boxes
	    val _ = if cinDrop then hilightBox box
		    else ()
        in (debug (CItemIdToString(selBoxId box)^" entered by "^
	           (StringUtil.concatWith " " (map (CItemIdToString o selBoxId) boxes))^
		   ": "^(Bool.toString cinDrop));
	    cinDrop)
	end 

    fun leaveBox (redBox cit)   = shrinkBox cit
      | leaveBox (greenBox cit) = enlargeBox cit
      | leaveBox (blueBox cit)  = colourBox cit Blue      

    fun lowlightBox (blueBox cit) = colourBox cit Blue
      | lowlightBox _             = ()


    fun dropBox (redBox _)  _    = false
      | dropBox (blueBox _) _    = 
	(Posix.Process.sleep (Time.fromSeconds 20); true)
      | dropBox (greenBox cit) _ = 
	  let val wHere          = readCItemCoords backdrop cit
	  in  (addNewBox (newBlueBox (addCoord (mkCoord(60, 0)) (hd wHere))); true)
	  end


    val moveOpaque = ref false  
    (* changed by toggleMove, see moveButton() below *)

    fun moveBox box delta =
	if (!moveOpaque) then
	    moveCItem backdrop (selBoxId box) delta
	else ()
	    


    (* Boxes as Drag&Drop-Items: *)

    structure BoxItems : DDITEMS =
	struct
	    type item = box
	    type item_list = box list
            fun  item_list_rep x = x
            fun  item_list_abs x = x

	    val selItemId     = selBoxId

            fun isImmobile (redBox _) = true
	      | isImmobile _          = false
		
	    fun selDropZone _ = mkRect(mkCoord(2, 2), mkCoord(boxSize- 2, boxSize -2))

	    fun grab _   = ()
	    val release  = lowlightBox
            val move     = moveBox

	    val select   = hilightBox
	    val deselect = lowlightBox

	    val enter = enterBox
	    val leave = leaveBox
	    fun drop b bb = (debug ("drop "^(StringUtil.concatWith " " 
			                     (map (CItemIdToString o selBoxId) bb))^" on "^
			                    (CItemIdToString(selBoxId b))); dropBox b bb)

	    (* Although we do not use the clipboard, it has to be here. 
	     *)
	    structure CB= Clipboard(type obj= box list)
        end


    structure DragDropBoxes = DragAndDrop(BoxItems)


    fun initBoxes () =
	let val ddboxes = DragDropBoxes.init backdrop
	    fun placeNewBlueBox (TkEvent(_,_, x, y,_,_)) =
		addNewBox(newBlueBox (mkCoord(x, y)))
	in
	   (
	    addNewBoxFun := DragDropBoxes.place ddboxes;
	    app addNewBox allMyBoxes;
	    addBind backdrop [BindEv(Double(ButtonPress(SOME 1)), mkAction(placeNewBlueBox))] 
	   )            
	end


    (* Get a window *)

    fun backdropBindings ()=
	[BindEv(Leave, mkAction(fn TkEvent(_,_,x,y,_,_) =>
	       Debug.print 19 ("Leave event occured at "^
			       (Int.toString x)^", "^(Int.toString y))))]
	
    fun backdropCanvas () =
	Canvas {widId=backdrop, scrolltype=NoneScb, citems=[],
		packings=[Side Top, Fill X, Expand true],
		configs=[Height backdropHeight, Width backdropWidth, Relief Groove,
		 Background Grey],bindings=backdropBindings()}


    fun quitButton win = 
	Button{widId=newWidgetId(),
	       packings=[Side Bottom, Fill X, Expand true],
	       configs=[Text "Quit", Command (mkSimpleAction(fn _ => SmlTk.closeWindow win)),
	                Relief Ridge, Borderwidth 2],
	       bindings=[]}

    fun toggleMove mb () =
	if (!moveOpaque)
	then (moveOpaque:= false;
	      addConf mb [Text "Move Opaque", Command (mkSimpleAction(toggleMove mb))])
	else (moveOpaque:= true;
	      addConf mb [Text "Move Invisible", Command (mkSimpleAction(toggleMove mb))])
	

    fun moveButton () = 
	let val mb= newWidgetId()
	in  Button{widId=mb,
	       packings=[Side Bottom, Fill X, Expand true],
	       configs=[Text "Move Opaque", Command (mkSimpleAction(toggleMove mb)),
	                Relief Ridge, Borderwidth 2],
	       bindings=[]}
	end
	

    fun go () = 
    let val mw     = newWinId()
        val boxwin = mkWindow{winId= mw, 
			      config= [WinTitle "LittleBoxes"], 
			      widgets= Pack [backdropCanvas(), 
					     quitButton mw, moveButton()],
			      bindings = [],
			      init= initBoxes}

    in startTclExn [boxwin]
       handle DragDropBoxes.DragAndDrop why => why
    end

end 








