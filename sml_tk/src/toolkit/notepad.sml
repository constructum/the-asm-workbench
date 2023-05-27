(* ***************************************************************************

   $Source: /repository/sml/sml_tk/src/toolkit/notepad.sml,v $

   A generic graphical user interface. 
  
   See <a href=file:../../doc/manual.html>the documentation</a> for more
   details.  

   "tests+examples/simpleinst.sml" contains a small example of how to
   use this package.
 
   $Date: 2001/03/30 13:39:46 $
   $Revision: 3.0 $

   Author: cxl, and a tiny bit bu (Last modification $Author: 2cxl $)

   (C) 1996, 1998, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)




functor Notepad(structure appl: NP_APPL_SIG ) (* :
  GENGUI_SIG *)(* where type object     = appl.object
                and type new_object = appl.new_object *)

= 

struct 
    
    local open SmlTk BasicUtil GlobalConfig in

    val default_printmode={mode = Print.Long,
                           printdepth=100,
                           height=NONE,
                           width=NONE}  (* the value is temporary *)
    fun name2string x = appl.string_of_name 
                            (appl.name_of x)
                                default_printmode


(* ************************************************************************ *
 * 
 * Parameters 
 *
 *)

    (* 
     * The trashcan
     *)
    val trashcanCId      = mkCItemId("trashcan") 

    fun trashcanCItem () =
	(* the trashcan does _not_ have an entry you can change *)
	CIcon{citemId=trashcanCId, coord=appl.Conf.trashcanCoord, 
              iconkind=Icons.selImage (appl.Conf.trashcanIcon()),
              configs=[Anchor NorthWest], bindings=[]}

    datatype item = obj of WinId* WidId * CItemId * Rect * appl.object
                  | trashcan of Rect


    (* the widget id of the canvas all the items are placed on *)
    val backdropId = mkWidgetId("backdrop")


   (*  
    * Assorted global references (ah, the joys of functional programming). 
    *)
    val placeObj = ref (fn (c:WidId)=> fn (i:item)=> ())
    val delObj   = ref (fn (c:WidId)=> fn (i:item)=> ())
    val overDZ   = ref (fn (c:WidId)=> fn (r:Rect)=> ([]:item list))
    (* point to the functions to place and delete items on the d/d canvas,
     * and to check dropzones as exported by the drag&drop module. 
     *)

    (* ``Subtypes'' -- a subtype is a type with a mode.  *)

    type subtype = appl.objtype * appl.mode
    
    val selSubtype = pair (appl.obj_type, appl.mode o appl.obj_type)



    exception GenGUI of string
		
    fun isTrashcan(trashcan _)    = true
      | isTrashcan _              = false

    (* selector functions *)

    fun selCanvas   (obj x)       = #2 x
      | selCanvas   (trashcan _)  = backdropId
	
    fun selDropZone (obj x)       = #4 x
      | selDropZone (trashcan dz) = dz

    fun selItemId   (obj x)       = #3 x
      | selItemId   (trashcan _)  = trashcanCId

    fun selObj      (obj x)       = #5 x

    fun itemCoords oo = hd (readCItemCoords (selCanvas oo) (selItemId oo))
	                handle Empty => mkCoord(0, 0)

    fun bitmapCId   cid = subCItemId(cid, "xIcon")
    fun widgetCId   cid = subCItemId(cid, "xWidId")
    fun popUpMenuId cid = subWidId(CItemIdToWidId cid, "xKuckuck")



    fun setObjImg  which ca cit oo = 
	                     addCItemConf ca (bitmapCId cit)
			           [Icon (which (not (appl.outline oo)) 
					        (appl.icon (appl.obj_type oo)))]

    fun setItemImg which (obj(_, ca, cit, _, oo)) = 
	                     setObjImg which ca cit oo
      | setItemImg which (trashcan _) =
                             addCItemConf backdropId trashcanCId
                                   [Icon ((which true) 
				          (appl.Conf.trashcanIcon()))]

    val hiliteIcon  = setItemImg (fn _ => Icons.selHiLite)
    val resetIcon   = setItemImg (fn no=> if no then Icons.selImage 
					  else Icons.selOutline)
    val outlineIcon = setItemImg (fn _ => Icons.selOutline)


    fun debugmsg msg = Debug.print 11 ("Notepad: "^msg)

    fun anchorToDir North     = mkCoord(0, ~1)
      | anchorToDir NorthEast = mkCoord(1, ~1)
      | anchorToDir East      = mkCoord(1, 0)
      | anchorToDir SouthEast = mkCoord(1, 1)
      | anchorToDir South     = mkCoord(0, 1)
      | anchorToDir SouthWest = mkCoord(~1,1)
      | anchorToDir West      = mkCoord(~1, 0)
      | anchorToDir NorthWest = mkCoord(~1, ~1)
      | anchorToDir Center    = mkCoord(0, 0)

    (*
     * Find a place to put the new object
     * 
     * Currently, this function just wanders off into the direction given until
     * it either finds a free space or wanders off the canvas. 
     * It would be nice if it would be a bit more clever and eg. if it at 
     * first can't find something in Direction NE, first try N, then E, and
     * then go further NE etc.
     *)

    fun getDropZone icn (x, y)= 
	((x, y), (x+Icons.selWidth icn, y+Icons.selHeight icn))
                        (* the drop zone is always in relation to
			 * the _first_ sub-item, here the bitmap *)

    fun findNicePlace cnv (nuOb, wh, shift) =
	let exception Off
	    (* width and height of canvas *)	    
	    val canrect = mkRect((0, 0), (getWidth cnv, getHeight cnv))
	    fun offCanvas (x, y) = not (inside (mkCoord (x, y)) canrect)
            (* get dropzone of new object *)
	    val dz      = getDropZone(appl.icon (appl.obj_type nuOb))
	    (* check for another dropzone *)
	    fun noOtherDZ r = null ((!overDZ) cnv r)
	    fun placeIt whr Center = whr
	      | placeIt whr sh     =
		    if (offCanvas whr) then raise Off
		    else if noOtherDZ (dz whr) then whr
			 else (debugmsg ("Can't place at "^
					 (showCoord [mkCoord whr]));
			       placeIt ((addCoord (mkCoord whr))
					(smultCoord (anchorToDir sh) 
					 appl.Conf.delta)) sh
			      )
	in
	    placeIt wh shift handle Off => wh
	end
 
    fun setObjIcon cnv cid no_out st =
	addCItemConf cnv (bitmapCId cid)
	[Icon ((if no_out then Icons.selImage else Icons.selOutline)
	       (appl.icon st))]

    and rename_action win frmid namid ob =
        appl.label_action{obj= ob, 
                          cc =fn txt=>(appl.rename txt ob;
                                      (* update the object *)
                                      setObjectName win frmid namid txt ob)
                                      (*update its visual appear. *)} 

    and monOpMenu win cnv frmid namid cid dz ob =
	let (* standard operations menue *)
	    val obt = appl.obj_type ob
	    fun opStdMItem (opn, name) =
		   MCommand [Text name, Command (fn () => opn ob)]
	    fun stdOps cnv cid dz 
		= (appl.std_ops obt) @
		  [(rename_action win frmid namid, "Rename"),
		   (fn ob=> (!(delObj) cnv (obj(win, cnv, cid, dz, ob));
		             appl.delete ob), "Delete")]
	    (* setting the mode
	     * ATTENTION: this piece of code _assumes_ that the icons of all
	     *            modes have exactly the same size
	     *)
	    fun setMode ob m  = (fn _=> (appl.set_mode (ob, m); 
					 setObjIcon cnv cid (not (appl.outline ob)) 
					 (appl.obj_type ob)),
				 appl.mode_name m)
	    val subtypeMenu = map (opStdMItem o (setMode ob)) (appl.modes obt)
	    (* customized extra menue *)
	    fun opMenuItem ob (opn, name) =
	           MCommand [Text name,
			     Command (fn () => opn (ob, 
						    hd (readCItemCoords cnv cid))
				                   (placeOnArea win cnv))]
	    val moreOpList = map (opMenuItem ob) (appl.mon_ops obt)
	    val menuList   = (map opStdMItem (stdOps cnv cid dz)) @
		                   (if ((length subtypeMenu)<= 1) then []
				    else MSeparator :: subtypeMenu) @
		                   (if (null moreOpList) then []
			            else MSeparator :: moreOpList)
        in
	    Popup {widId   = popUpMenuId cid,
		   mitems  = menuList,
		   configs = [Tearoff false]}
	end
    
    and popUpMonOpMenu cid (TkEvent(_,_,_,_, xr, yr)) =
	 popUpMenu (popUpMenuId cid) (SOME 0) (mkCoord(xr, yr))
	 
    and objectBindings win cnv cid dz ob =
         let fun wHere ()= hd(readCItemCoords cnv cid)
             fun rep_act wHere nuOb = ((!delObj)cnv(obj(win,cnv,cid,dz,ob));
                                       placeOnAreaAt win cnv wHere nuOb)
             fun out_act () = setObjImg (fn _ => Icons.selOutline) cnv cid ob
         in  [BindEv(Events.activate_event(),             
		     fn e => appl.object_action 
                                  {win = win, obj = ob,
                                   replace_object_action = rep_act (wHere()),
                                   outline_object_action = out_act}),  
	      BindEv(Events.object_menu_event(), 
		     fn e=> if not (appl.is_locked_object ob) 
                            then popUpMonOpMenu cid e 
                            else ())
	     ]
         end
   
    and setObjectName win frmid labelid name ob =
	let (* the name-printing should be done elsewhere once and for all - 
               some day in obj2objtree-fun . . . >>> *)
            val lab_len   = 10 (*ad hoc value !!!*)
            val lab_pm    ={mode = Print.Short,printdepth=100,
                            height=SOME (lab_len div 2), width=SOME lab_len} 
            fun blk txt   = if size(txt)>lab_len 
                            then substring(txt,0,lab_len) :: 
                                 blk(substring(txt,lab_len,size(txt)-lab_len))
                            else [txt]
            fun block txt = let val tt = blk txt 
                            in foldl(fn(a,b) => b ^"\n"^a)(hd tt)(tl tt) end

            fun height txt = ((size txt) div lab_len) + 1
         
            fun col_lab true  = Background (!(#background_sel Colors.Config))
               |col_lab false = Background (!(#background Colors.Config))

            fun hilite b _    = (addConf labelid[col_lab(b)])

            fun activate _    = (appl.label_action{obj= ob, 
                                      cc =fn txt=>
                                          (appl.rename txt ob;
                                           let  val txt = appl.string_of_name
                                                            (appl.name_of ob) 
                                                               lab_pm
                                                val TT = Text txt
                                                val CC = col_lab(false)
                                                val HH = Height(height txt)
                                      (* update the object *)
                                           in   addConf labelid [TT, CC, HH]
                                           end)
                                      (* update its visual appearance *)})

            fun label name = Label{widId=labelid, packings=[], 
                              bindings= [BindEv(Events.activate_event(),
                                                activate),
                                         BindEv(Enter, hilite true),
                                         BindEv(Leave, hilite false)
                                        ],
			      configs=  [Text name,col_lab false,
                                         Font appl.Conf.iconNameFont,
					 Width appl.Conf.iconNameWidth,
                                         Height (height name) 
                                        ]}
	in  (* yes, we do have to delete the widget and replace it because
	     * we want the packer to center the label within the (invisible)
	     * frame  *)
            appl.rename name ob;
            let val name = appl.string_of_name (appl.name_of ob) lab_pm
            in  debugmsg("Renaming "^(WidIdToString labelid)^" to "^name);
	        if (occursWidget labelid) then delWidget labelid else ();
	        addWidget win frmid (label name)
            end	    
	end

    and placeObjAsItem win cnv (x,y) nuOb =
	let val cid   = newCItemId()
	    val frmid = newWidgetId()
	    val namid = newWidgetId()
	    val icn   = appl.icon (appl.obj_type nuOb)
	    val selimg= if (not (appl.outline nuOb)) then Icons.selImage 
			else Icons.selOutline
	    val bm_w  = Icons.selWidth icn
	    val bm_h  = Icons.selHeight icn
	    val te_x  = (bm_w - appl.Conf.iconNameWidth) div 2
	    val dz    = mkRect(getDropZone icn (0, 0))
	    val nm    = appl.string_of_name 
                             (appl.name_of nuOb)
                             {mode = Print.Short, printdepth=100,
                              height=NONE, width=NONE} (* WHY ??? bu *)

	    (* the CItems representing the object *)
	    val bmCI  = CIcon{citemId= bitmapCId cid, 
			      coord= mkCoord(x, y), 
			      iconkind=selimg icn,
			      configs= [Anchor NorthWest], 
			      bindings= objectBindings win cnv cid dz nuOb}
	    val teCI  = CWidget{citemId=widgetCId cid, 
				coord= mkCoord(x+te_x, y+bm_h),
				widgets=
				   Pack [Frame{widId=frmid, 
					       widgets= Pack
 
                                        [(* Entry(entryWId cid, [Expand true],
					[Font appl.Conf.iconNameFont],
					textEntryBindings win nuOb cid), *)
				 	monOpMenu win cnv frmid namid cid 
                                                    dz nuOb],

					       packings=[], 
                                               configs= [Background (!(#background Colors.Config))],
					       bindings=[]}], 
				configs=[Width appl.Conf.iconNameWidth,
					 Anchor NorthWest (*,
                                         FillColor (!(#background Colors.Config)) *)],
				bindings= objectBindings win cnv cid dz nuOb}
	    val tagCI = CTag{citemId=cid, 
			     citemIds= [bitmapCId cid, widgetCId cid]} 
	in  (app (addCItem cnv) [bmCI, teCI, tagCI];
	     setObjectName win frmid namid nm nuOb;
	     obj(win, cnv, cid, dz, nuOb))
	end

    and placeOnAreaAt win cnv wHere nuOb = 
	   (!placeObj) cnv (placeObjAsItem win cnv wHere nuOb)

    and placeOnArea win cnv (nuOb, (wh, shift)) = 
           placeOnAreaAt win cnv (findNicePlace cnv (nuOb, wh, shift)) nuOb 

    
    structure DD=
	struct
	    type item = item
	    type il   = item list
		
	    val selItemId    = selItemId
	    val selDropZone  = selDropZone
		
(*	    fun isImmobile x = isOpen (selItemId x)  *)
            fun isImmobile x = not(isTrashcan x) andalso
                               appl.is_locked_object (selObj x) 
		
	    fun grab it = 
		if not appl.Conf.moveOpaque then
		    outlineIcon it
		else ()
		    
	    val release  = resetIcon
		
	    val select   = hiliteIcon 
		
	    val deselect = resetIcon
		
	    fun move it delta =
		if  appl.Conf.moveOpaque then 
		    moveCItem (selCanvas it) (selItemId it) delta
		else ()
		    
	    fun enter eIt entering =
		if (List.exists isTrashcan entering)  
		    then false
		else
		    case eIt of
			trashcan d =>  (hiliteIcon (trashcan d); true)
		      | obj(_, _, cit, dz, ob)=>
			    let val olt= appl.objlist_type (map selObj entering)
				val ot = appl.obj_type ob
			    in  if (appl.is_locked_object ob) 
				    then (* entered object is currently open in
					  * construction area-- no opns poss. *)
					false
				else case olt of
				    NONE    => false
				  | SOME lt =>
					(case appl.bin_ops (ot, lt) of
					     NONE    => false
					   | SOME f  => (hiliteIcon eIt; true))
			    end
			
	    val leave = resetIcon
		
		    
	    fun drop (trashcan _) trash =       
		(app appl.delete (map selObj trash); false)
	      | drop (obj(win, cnv, cid, dz, ob)) dropped =
		let val ot  = appl.obj_type ob
		    val olt = appl.objlist_type (map selObj dropped)
		in  if (appl.is_locked_object ob) then false
		        (* object dropped onto currently open in the con.area *)
		    else case olt of
			NONE    => raise GenGUI "Illegal 'drop'"
		      | SOME lt =>
			    (case appl.bin_ops(appl.obj_type ob, lt) of
				 NONE    => raise GenGUI "Illegal 'drop'"
			       | SOME f  => (f(ob, 
					       hd (readCItemCoords cnv cid),
					       (map selObj dropped),
					        fn nuob=> 
						   placeOnArea win cnv nuob); 
					     true)
			     )
		end
	    
            type item_list = item list
            fun  item_list_rep x = x
            fun  item_list_abs x = x	
	
	    structure CB = (* Clipboard(struct type obj= item end) *)
		struct
		    type obj= item_list
			
		    fun  put it ev cb = 
			appl.CB.put (appl.cb_objects_abs(fn ()=> map selObj it)) ev cb
		end
	end
    
    structure ObjectsDD= DragAndDrop(DD)

    type object     = appl.object
    type cb_objects = appl.cb_objects
    type new_object = appl.new_object
    type gui_state  = new_object list 

    (* the clipboard *)
    structure CB= appl.CB;


    (* redisplay all the icons *)
    fun redisplay_icons which =
	let fun setIcon (obj(_, cnv, cid, _, ob))= 
	           setObjIcon cnv cid (not (appl.outline ob)) (appl.obj_type ob)
	    fun filt (obj(_, _, _, _, ob))= which ob
	      | filt (trashcan _)         = false
	in app setIcon (List.filter filt (ObjectsDD.allItems backdropId))
	end


    fun enterArea win bd (ev as TkEvent(_,_, x, y, _, _)) =
	ignore(List.foldr (fn (ob, (x, y))=> 
			   (ObjectsDD.place bd 
			    (placeObjAsItem win bd (x,y) ob);
			    (x+ 5, y+ 5))) (x, y) ((appl.cb_objects_rep(CB.get ev))()))
	(* if there's more than one object, put the following ones slightly
	 * lower and to the right *)
	handle CB.Empty => ()


    fun areaBindings win bd =
	[BindEv(Enter, enterArea win bd),
         BindEv((* Events.object_menu_event() *)
                Shift(Double(ButtonPress(SOME 1))),  (* HACK ! *)
		     fn TkEvent(_,_, x, y, _, _) => 
                           (print"IN FUTURE: CreationMenu\n";
                            (fst(hd appl.create_actions)) {pos = (x,y),
                                                           tag = "folder"}))
        ]
 
    fun placeTrashcan bd = 
	let val tci = appl.Conf.trashcanIcon()
	    val tcw = Icons.selWidth tci
	    val tch = Icons.selHeight tci
	in  if (Icons.isNoIcon tci) orelse 
	       ((tcw= 0) andalso (tch= 0)) then ()  (* no icon-- no trashcan *)
	    else (addCItem bd (trashcanCItem());
		  ObjectsDD.place bd (trashcan ((0,0), (tcw, tch)))
		  )
	end
 

    fun initRefs () = 
	(placeObj := (fn cnv=> ObjectsDD.place cnv);
	 delObj   := (fn cnv=> ObjectsDD.delete cnv);
	 overDZ   := (fn cnv=> ObjectsDD.dropZonesInRect cnv)
	 ;debugmsg "Refs init'd."
	 )
	
    val backdropWinId = ref (newWinId())
	
    fun main_wid win =
	let val _ = backdropWinId := win
            val assArea= Canvas{widId=backdropId, 
				scrolltype=NoneScb, citems= [],
				packings= [Expand true, Fill X, Fill Y, 
					   Side Top],
				configs= [Width appl.Conf.width, 
					  Height appl.Conf.height, 
					  Relief Groove,
                                          Background (!(#background 
                                                        Colors.Config))], 
				bindings= areaBindings win backdropId}
	in  assArea
	end


    val initial_state = appl.init
	
	
    fun init state =
	let val bd= ObjectsDD.init backdropId
	in  (initRefs();
	     placeTrashcan bd;
	     app (placeOnArea (!backdropWinId) bd) state;
	     registerIntrListener (fn()=> ObjectsDD.reset backdropId);
             ()
	    )
	    handle ObjectsDD.DragAndDrop error => 
		                  raise GenGUI ("D/D error:"^error)
	end

    (* some thought needed here. If we ever want to place objects in
     * areas other than the backdrop, we need to pass this the WinId and WidId 
     * of the respective canvas. This becomes vital once we start exporting
     * configurations with more than one canvas -- i.e. folders. 
     *)

    fun intro nuOb = placeOnArea (!backdropWinId) backdropId nuOb

    fun elim ob =
        let fun ft x = (not(isTrashcan x) andalso 
                        appl.ord(selObj x,ob) = EQUAL)
        in  case (List.find ft (ObjectsDD.selectedItems ())) of
                 NONE    => (case List.find ft 
                                  (ObjectsDD.allItems backdropId) of
                              NONE => ()
                             |SOME it => (!delObj) (backdropId) (it))
               | SOME it => (!delObj) (backdropId) (it)
        end;

    fun state () = 
	let fun item2nuObj (obj(win, wid, cid, dz, ob))=
	                         SOME (ob, (hd (readCItemCoords wid cid)
				           handle Empty=> (0, 0), Center))
	      | item2nuObj (trashcan _) = NONE
	    
	in  let val bla = 
            List.mapPartial item2nuObj (ObjectsDD.allItems backdropId) @
	    (* This just forgets abouts the trashcan-- so it always appears
	     * in the same position on startup. One may or may
	     * not want to change that. 
	     *)
            List.mapPartial item2nuObj (ObjectsDD.selectedItems ())  
            (* do not forget the selected or grabbed items ! ! ! *)  
            in print ("Notepad.state:"^
                     (String.concat (List.map (name2string o fst) bla)) ^ ":\n"); bla
            end
  
	end
    end (* local *)
end




