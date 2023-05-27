(* ***************************************************************************

   $Source: /repository/sml/sml_tk/src/toolkit/drag_and_drop.sml,v $

   A small drag&drop package for sml_tk. 

   It is generic over drag&items as given by signature DDITEMS.
  
   See the documentation for more details. 
   "tests+examples/boxes.sml" contains a small example of how to use this 
   package. 
 

   $Date: 2001/03/30 13:39:38 $
   $Revision: 3.0 $

   Author: cxl (Last modification $Author: 2cxl $)

   (C) 1996, 1998, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)


functor DragAndDrop(DDitems: DDITEMS) : 
    DRAG_DROP_SIG (* where type item = DDitems.item *) =

struct 

    open SmlTk BasicUtil

    type item= DDitems.item

    type DDCanvas = WidId

    exception DragAndDrop of string


    (* local variables *)


    val dropZones      = ref ([]: (WidId* item* Rect) list)

    fun printDropZones() = ddebug("dropZones "^ 
				  (StringUtil.concatWith ", " 
				   ((map (CItemIdToString o DDitems.selItemId o #2) 
					       (!dropZones)))))

    and ddebug(str)      = Debug.print 11 ("DD: "^str)
 
    type grabItem  = item* Rect Option.option* Coord
    val  grabItems  = ref ([]: grabItem list)
    val  selItems   = ref ([]: item list)
    val  lasso      = ref (NONE: (CItemId* Coord) option) 
    val  oldPos     = ref (mkCoord(0, 0))
    val  canDrop    = ref false
    val  grabPos    = ref (mkCoord(0, 0))

    datatype enterStatus = Entered of item 
	                 | NothingEntered
			 | LeftCanvas

    val enteredItem    = ref NothingEntered

    (* initialize all the references above *)
    fun initRefs () = (dropZones   := [];
		       grabItems   := [];
		       selItems    := [];
		       lasso       := NONE;
		       oldPos      := (0, 0);
		       canDrop     := false;
		       grabPos     := (0, 0);
		       enteredItem := NothingEntered)

    (* equality for items *)
    fun eq it1 it2 = ((DDitems.selItemId it1)= DDitems.selItemId it2)

    (* apply function on items on grabItem list *)
    fun appIt f = app (fn (it, _, _)=> f it)

    fun overDropZone cnv coords =
	map #2 (List.filter (fn (c0, _, r)=> 
			   (inside coords r) andalso (c0= cnv))
		(! dropZones))

   (* this could be done in a much more efficient way, eg.
    * use btrees or something (oh aye ;-) 
    *)
	
    (* find all items inside rectangle r on canvas cnv *)
    fun dropZonesInRect cnv r =
	map #2 (List.filter (fn (c0, _, r0)=> 
			     (intersect r0 r) andalso (c0= cnv))
		(!dropZones))

    (* get current drop zone (i.e. on-the-canvas coordinates) of item
     * it (taken from !dropZones for efficiency, wow).
     * Note that c_item id's are global, so there's no need to check
     * the widget it (I hope :-) 
     *)
    fun getCurrentDropZone it =
	Option.map #3 (List.find ((eq it) o #2) (! dropZones))

    (* delete item from drop zone list *)
    fun delDropZone item =
 	dropZones := List.filter (not o (eq item) o #2) (!dropZones)


    (* not clear wethere we want bindings on *all* items of CTag or
     * just the first one.. currently the former:
     *)
    fun addTagBinding wid id bindings =
    let val item = getCItem wid id
    in  case item of
	CTag{citemIds=ls,...} => app (fn id=> addTagBinding wid id bindings) ls
      | _                     => addCItemBind wid id bindings
    end


    (* "private" version of delCItem, this one deletes 
     * `subitems' of CTag-items (delCItem _doesn't_).
     *)
    fun recDelete wid cid = 
    let val cit = getCItem wid cid	    
    in  (delCItem wid cid;
	 case cit of 
	     CTag{citemIds=subitems,...} => app (recDelete wid) subitems
	   | _                           => ()
	)
    end


    (* default cursors *)   

    (* the "hand" appearing when you are ready to grab an item *)
    val enterCursor = Cursor(XCursor(mkCursorName("hand1"), NONE))

    (* the crosshair for dragging an item *)
    val dragCursor = Cursor(XCursor(mkCursorName("fleur"), NONE))

    (* the clipboard -- unless we reexport it, this structure is just 
     * a shortcut *)
    structure CB = DDitems.CB 

    (* unfortunately, we cinnae use setCItemCoords with CTag(..) items,
     * so we have to moveCItem the buggers-- hence the following function:
     *)
    fun moveItem dd id wHere = 
	moveCItem dd id (subCoord wHere (hd (readCItemCoords dd id)))
   
    fun btwyc dd (item, dz, oldItemPos) =
	(* "back to whence you came",
	 * reinstall a grabbed item and dropZone at original position 
	 *)
	(case dz of NONE    => () 
                  | SOME dz => dropZones := (dd, item, dz):: (!dropZones);
	 moveItem dd (DDitems.selItemId item) oldItemPos;
	 DDitems.release item
	)

    fun moveGrabIt dd off (item, SOME dz, oldItemPos) =
	(* move a grabbed item, plus its drop zone, to a new position.	 
	 *)
	btwyc dd (item, SOME (moveRect dz off), addCoord oldItemPos off)
      | moveGrabIt dd off (item, NONE, oldItemPos) =
	btwyc dd (item, NONE, addCoord oldItemPos off)
	 	 

    (* enter/leave are bound to particular canvas items 
     * these _should_ only be called when there's no grab. Unfortunately,
     * the wish seems to generate spurious enter/leave events, so we better 
     * check. Defensive programming an' aw.
     *)
    fun enterItem ddCanvas it _ =
	 if not (DDitems.isImmobile it)
	    andalso (null (!grabItems)) then
	    (addConf ddCanvas [enterCursor];	     
	     enteredItem := Entered it;
	     ddebug ("Entered "^(CItemIdToString (DDitems.selItemId it)))
	    )
	 else ()

    (* Some wish´s (e.g. Tk8.0 under Linux KDE) generate Leave-Events
     * if you press the button while over a CItem when the PressButton is bound
     * to the canvas below, before they generate a PressButton event (if you
     * can follow, I admit it confused either me or Tcl as well). Hence,
     * we only generate a "leave" here if we have really left (i.e. the
     * coordinates are outside the dropzone of the item). 
     *)
    and leaveItem ddCanvas it (TkEvent(_, _, x, y, _, _)) =
	if null (!grabItems) then
	    case (getCurrentDropZone it) of 
		NONE => ()
	      | SOME dz => if not (inside (x, y) dz) 
			       then
				   (addConf ddCanvas [Cursor(NoCursor)]; 
				    enteredItem := NothingEntered;
				    ddebug ("Left "^
					    (CItemIdToString 
					     (DDitems.selItemId it)))
				    )
			   else ()
	else ()

    (* press/releaseGrabButton are bound to the canvas *)
    and pressSelButton ddCanvas (TkEvent(_, _, x, y, _, _)) =
        case !enteredItem of
	    Entered overIt =>
		if not (List.exists (eq overIt) (!selItems)) then
		    (selItems := overIt:: (!selItems);
		     DDitems.select overIt;
		     ddebug ("selected item: "^(CItemIdToString(DDitems.selItemId  overIt)))
		    )
		else ()
          | NothingEntered =>
	        ((* click over empty canvas: deselect items *)
		 app DDitems.deselect (!selItems);
		 selItems := [])		 
	  | _ => ()
	
    and pressGrabButton ddCanvas (TkEvent(_, _, x, y, _, _)) =
	case !enteredItem of 
	    Entered overIt =>
		let fun wHere it  =  hd (readCItemCoords ddCanvas 
					  (DDitems.selItemId it))
		    (* lose grabbed item if also selected: *)
		    val items     = (List.filter (not o (eq overIt)) (!selItems))
		    (* grab a selected item *)
		    fun grabIt it = 
			let val cur_dz = getCurrentDropZone it
			in  (delDropZone it; (it, cur_dz, wHere it))
			end
	        in  ((* revert map of grabbed items, since they are "the wrong
		      * way around" -- last ones selected first *)
		     grabItems    := rev (map grabIt (overIt :: items));
		     enteredItem  := NothingEntered;
		     oldPos       := mkCoord(x, y);
		     grabPos      := mkCoord(x, y);
		     selItems     := [];
		     (* looks like bloody basic eh? *)
		     addConf ddCanvas [dragCursor];
		     appIt DDitems.grab (!grabItems);
		     ddebug ("grabbed items: "^
			     (StringUtil.concatWith " "
			      (map (CItemIdToString o DDitems.selItemId o #1) 
				    (!grabItems))))
		    )
		end
	  | _ => (* start new lasso. *)
		let val rid       = newCItemId()
		    val lassoRect = CRectangle{citemId= rid,
					       coord1= mkCoord(x, y),
					       coord2= mkCoord(x, y),
					       configs= [Width 2],
					       bindings= []}
		in  (addCItem ddCanvas lassoRect;
		     lasso := SOME (rid, mkCoord(x, y)))
		end

    and grabbedMotion ddCanvas (TkEvent(_, _, x, y, _, _)) =
	if (null (!grabItems)) then 
	    case (!lasso) of
		SOME(rectId, lhc) => 
		    setCItemCoords ddCanvas rectId [lhc, mkCoord(x, y)]
              | NONE => () 
	else
	    let val grabIds = map #1 (!grabItems)
		fun mvGrabItem it = DDitems.move it (subCoord (mkCoord(x,y)) 
						     (!oldPos))
	    in
		let val cs= mkCoord(x, y) 
		in  (app mvGrabItem grabIds;
		     oldPos := cs;
		     case !enteredItem of 
			 Entered it => 
			     (case getCurrentDropZone it of NONE => ()
				 | SOME dz => 
				     if inside (!oldPos) dz
					 then () (* have already entered, 
						  * are still inside *)
				     else (* have left entered item *)
					 (enteredItem := NothingEntered;
					  if !canDrop then DDitems.leave it
					  else ();
					      canDrop := false
					 ))                               
		       | _ => (* Have we entered an item? *)
			     let val over= overDropZone ddCanvas cs
			     in case over of
				 oo::_ => (enteredItem := Entered oo;
					   canDrop:= DDitems.enter oo grabIds;
					   ddebug("have entered "^
						  (CItemIdToString(DDitems.selItemId oo))^
						  ": "^(Bool.toString (!canDrop)))
					   )
			       | []   => ()
			     end)
		end
	    end

    and releaseGrabButton ddCanvas (ev as TkEvent(_, _, x, y, _, _)) =
	if (null (!grabItems)) then 
	    (case !lasso of
		 SOME (rid, llc as (x0, y0)) =>
		     ((* delete lasso: *)
		      delCItem ddCanvas rid;
		      lasso := NONE;
		     (* throw lasso: if the lasso has not been thrown 
		      * further than five units, ignore it. (This is 
		      * in particular to avoid having the release-event after
		      * a double-click causing a lasso throw-- we always end
		      * up with the construction object selected!) 
		      *)
		      if (Int.abs(x0- x)> 5) andalso (Int.abs(y0- y)> 5) then
			  (* valid throw: find selected items, delete lasso *)
			  let val selits= dropZonesInRect ddCanvas 
			      (mkRect(llc, mkCoord(x, y)))
			  in  (app DDitems.select selits;
			       selItems := (!selItems) @ selits;
			       ddebug ("Caught "^(StringUtil.concatWith ", " 
						  (map (CItemIdToString o 
							DDitems.selItemId) 
						   selits)))
			       )
			  end
		     else
			 ddebug("Invalid lasso throw: not far enough")
		    )
			 
		 | NONE => ())
	else
	    (case !enteredItem of
		 Entered it =>
		     if !canDrop then 
			  ((* first, do the drop operation. *)
			   if DDitems.drop it (map #1 (!grabItems)) then
			      (* non-destructive drop, reinstall item
			       * at original position *)
			       app (btwyc ddCanvas) (!grabItems)
			   else
			       (* destructive, argument items vanish *)
			       appIt ((recDelete ddCanvas) o DDitems.selItemId)
			                                         (!grabItems);
		               (* don't need to delete dropZone *)
			   (* generate leaving event for entered item *)
			   DDitems.leave it)
		      else
			 (* can't drop, reinstall at grab position 
			  * w/ original dropzone *)
			 app (btwyc ddCanvas) (!grabItems)		   
	       | NothingEntered =>
	             (* huvnae entered anything, so we huv tae move the items *)
		      app (moveGrabIt ddCanvas (subCoord (mkCoord(x,y))
						(!grabPos)))  (!grabItems)
	       | LeftCanvas =>
		     (* off the canvas, put items into clipboard 
		      * This is awkward -- we reinstall the item on the 
		      * DDcanvas, complete with dropZone, and have it de-
		      * leted by the callback function of the clipboard.
		      * Thus, only objects appearing elsewhere are de-
		      * leted from the DDcanvas. 
		      *)			  
		     let fun delIt it =
			 (delDropZone it;
			  recDelete ddCanvas (DDitems.selItemId it)
			  (* handle exceptions here ?! *)
			 )
		     in (app (btwyc ddCanvas) (!grabItems);
			 DDitems.CB.put (DDitems.item_list_abs(map #1(!grabItems)))ev 
			                (fn()=> appIt delIt (!grabItems))
			)
		     end;
	     (* In any case, reset variables and the cursor.
	      *)
	     enteredItem := NothingEntered;
	     grabItems := [];
	     canDrop := false;
	     addConf ddCanvas [Cursor(NoCursor)]
	     )

		
    and ddItemBindings canId item =
	[ BindEv(Enter, enterItem canId item),
	  BindEv(Leave, leaveItem canId item)
	]

    and place ddCanvas item = 
	let val cid  = DDitems.selItemId item
	    val wher = readCItemCoords ddCanvas cid
	    val nudz = moveRect (DDitems.selDropZone item) (hd wher)
			           (* (hd (readCItemCoords ddCanvas cid)) *)
	in
	    (ddebug ("place "^ (CItemIdToString cid)^", dropZone "^ (showRect nudz));
	     dropZones := (ddCanvas, item, nudz) :: !dropZones;
	     addTagBinding ddCanvas cid (ddItemBindings ddCanvas item) 
	    )
	end


    fun leaveCanvas _ =
	enteredItem := LeftCanvas

(* I forget why I thought this function was necessary :-) 
 * Anyway, it´s definitely harmful since some wish´s seem to generate an enter
 * event for the canvas widget when pressing the button over a canvas item
 * (see the comments above-- I can´t see no sense in this sense either)
 *
    fun enterCanvas _ =
	enteredItem := NothingEntered
 plus
        BindEv(ModButton(1, Enter),   enterCanvas)]; 
 below.
 *)

    (* Reset the drag&drop module -- i.e. don't reset it
     * to initial value, but reset the grabbed items etc. to some sane
     * values so we can continue. 
     * This function can be bound to an interrupt handler, and called 
     * if the drag&drop for some reason buggers up. Since it's very state-
     * based, and makes assumptions on the order in which events are generated
     * which may not hold on a particular wish, this may happen. *)
    fun reset ddCanvas = 
	((* reset currently grabbed items *)
	 app (btwyc ddCanvas) (!grabItems);
	 grabItems := [];
	 (* reset enteredItem *) 
	 case !enteredItem of Entered eit => DDitems.leave eit | _ => ();
	 enteredItem := NothingEntered;
	 (* deselect items *)
	 app DDitems.deselect (!selItems);
	 selItems := [];		 
	 canDrop := false;
	 addConf ddCanvas [Cursor NoCursor];
	 (* delete lasso *)
	 case !lasso of SOME(rid, _) => delCItem ddCanvas rid | NONE => (); 
         lasso := NONE	 
	)


    fun canvasBindings canId =
	[ BindEv(ButtonPress(SOME 1),   pressGrabButton canId),
	  BindEv(ButtonRelease(SOME 1), releaseGrabButton canId),
	  BindEv(ButtonPress(SOME 2),   pressSelButton canId),
	  BindEv(ModButton(1, Motion),  grabbedMotion canId),
	  BindEv(ModButton(1, Leave),   leaveCanvas)
	];


    fun init canvasId = 
	(* Raises an exception of passed a widget which isnae a canvas, or 
	 * a canvas wi' items on it. 
	 *)
	case getWidget canvasId
	of Canvas{widId=wid, citems=cids, ...} =>
	    (map ((SmlTk.delCItem wid) o SmlTk.selItemId) cids;
             (*fast remove of citems in DD *)
             initRefs();
             (* implicit remove of potential items in dragZone *)
	     addBind wid (canvasBindings wid);
	     (* dropZones := []; should be superfluous - initRefs!. bu *)
             ddebug("init " ^ (WidIdToString wid));
	     wid)
(*	|  Canvas{widId=wid, citems= x:: xs, ...} =>
	        raise DragAndDrop "init: called with non-empty canvas."
CHANGED - bu
*)
        |  w => raise DragAndDrop "init: argument not a canvas."
  	       
    fun delete ddCanvas item = 
	(case !enteredItem of
	     Entered it => if eq item it then 
		 (enteredItem := NothingEntered;
		  addConf ddCanvas [Cursor(NoCursor)]
		  )
			   else ()
	   | _ => ();
	 grabItems := List.filter (not o (eq item) o #1) (!grabItems);
	 selItems  := List.filter (not o (eq item)) (!selItems);
	 if (null (!grabItems)) 
	     then addConf ddCanvas [Cursor(NoCursor)] else ();
	 delDropZone item;
	 (* And delete the CItem: *)
	 recDelete ddCanvas (DDitems.selItemId item)
	)

    fun selectedItems ()= (!selItems) @
                          (map (fn (x,_,_) => x) (!grabItems))

    fun allItems ddCanvas = 
	map #2 (List.filter (fn (cnv, _, _)=> cnv= ddCanvas) (!dropZones))


(* --- Some implementation notes. ------------------------------- 

 - Although it would undoubtedly be better to export DDCanvas as an
 abstract datatype, this is not possible because GenGUI uses the fact
 that DDCanvas is a widget to install forward references to the
 exported functions from within the argument structure of D&D.

 - More than one d&d canvas: I'm note sure this works the way the 
 d&d module is implemented just now. One might have to do some more checks
 to ensure that grabbing/entering etc. does only effect items on the same
 canvas. (All of this would be very easy indeed if SML had dynamic modules. 
 Oh well.) 

 ---cxl. 

 *)
 
	
end


