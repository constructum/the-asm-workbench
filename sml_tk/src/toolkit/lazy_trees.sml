(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/lazy_trees.sml,v $

   Lazy Tree Lists

   $Date: 2001/03/30 13:39:45 $
   $Revision: 3.0 $
   Author: ludi (Last modification by $Author: 2cxl $)

   (C) 1999, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)

functor LazyTree(structure Obj : LAZY_TREE_OBJECTS) :
    sig
	type obj = Obj.obj
	exception Error of string

	datatype history_state =
	    hist_start | hist_middle | hist_end | hist_empty

	val tree_list :
	    {width              : int,
	     height             : int,
	     font               : SmlTk.Font,
	     selection_notifier : obj option -> unit} ->
	    {canvas    : obj -> SmlTk.Widget,
	     selection : unit -> obj option,
	     up        : unit -> unit,
	     position  : unit -> history_state,
	     back      : unit -> unit,
	     forward   : unit -> unit}
    end =
    struct
	open SmlTk

	type obj         = Obj.obj
	type EntryId     = string
	type path        = EntryId list
	datatype state_entry =
	    STATE_ENTRY of
	      (path *                   (*  1. path of entry ids             *)
	       Coord *                  (*  2. root coord of entry           *)
	       bool option *            (*  3. true/false if node is open/   *)
					(*     closed, NONE if leaf          *)
	       CItem list *             (*  4. CItems                        *)
	       CItem ref *              (*  5. line to upper entry           *)
	       (CItem list) ref *       (*  6. plus of minus citems if node  *)
	       obj *                    (*  7. referenced object             *)
	       WidId *                  (*  8. WidId of label                *)
	       CItemId *                (*  9. CItemId of Icon               *)
	       (state_entry list *      (* 10. subentries / old root y-coord *)
		int) option ref)        (*     if node has been open before  *)

	fun sel_entry_content(STATE_ENTRY e) = e

	exception Error of string

	datatype history_state =
	    hist_start | hist_middle | hist_end | hist_empty

	val dummy_event = TkEvent(0, "", 0, 0, 0, 0)

	fun tree_list {width, height, font, selection_notifier} =
	    let
		val widId = newWidgetId()

		val ENTCNT  = ref 0

		fun newEntryId() = (ENTCNT := !ENTCNT + 1;
				    "entry" ^ Int.toString(!ENTCNT))

		val STATE = ref [] : state_entry list ref

		val SELECTED =
		    ref NONE : (path * WidId * obj * CItemId) option ref

		val HISTORY         = ref [] : path list ref
		val HISTORY_POINTER = ref 0

		fun hist_append p =
		    ((if length(!HISTORY) = !HISTORY_POINTER then
			  HISTORY := !HISTORY @ [p]
		      else
			  HISTORY :=
			    List.take(!HISTORY, !HISTORY_POINTER) @ [p]);
		     HISTORY_POINTER := length(!HISTORY))

		fun sub_path p1 p2 =
		    List.take(p1, length p2) = p2 handle _ => false

		fun path_to p = List.take(p, length p - 1)

		fun enter id _ =
		    if (isSome(!SELECTED) andalso
			#2(valOf(!SELECTED)) = id) then ()
		    else addConf id [Background Grey, Foreground White]

		fun leave id _ =
		    if (isSome(!SELECTED) andalso
			#2(valOf(!SELECTED)) = id) then ()
		    else addConf id [Background White, Foreground Black]

		fun plus (x, y) bi =
		    [CLine {citemId  = newCItemId(),
			    coords   = [(x + 2, y), (x + 7, y)],
			    configs  = [],
			    bindings = bi},
		     CLine {citemId  = newCItemId(),
			    coords   = [(x + 4, y + 3),
					(x + 4, y - 2)],
			    configs  = [],
			    bindings = bi}]

		fun minus (x, y) bi =
		    [CLine {citemId  = newCItemId(),
			    coords   = [(x + 2, y), (x + 7, y)],
			    configs  = [],
			    bindings = bi}]

		fun sel_next ((e : state_entry) :: es) p =
		    if (path_to(#1(sel_entry_content e)) = p orelse
			sub_path (path_to(#1(sel_entry_content e))) p) then
			sel_next es p
		    else (e :: es)
		  | sel_next _ _    = []

		fun stretch ((e1 : state_entry) :: e1s)
		            ((e2 : state_entry) :: e2s) =
		    if (path_to(#1(sel_entry_content e1)) =
			path_to(#1(sel_entry_content e2))) then
			((#5(sel_entry_content e2)) :=
			 let
			     val oldcoords =
				 selItemCoords(!(#5(sel_entry_content e2)))
			     val newsndc   =
				 (#1(#2(sel_entry_content e1)) + 4,
				  #2(#2(sel_entry_content e1)) + 4)
			     val newcoords = hd oldcoords :: [newsndc]
			 in
			     updItemCoords (!(#5(sel_entry_content e2)))
			                   newcoords
			 end;
			 stretch
			   (sel_next e1s (path_to(#1(sel_entry_content e1))))
			   e2s)
		    else
			if (sub_path (#1(sel_entry_content e2))
			             (#1(sel_entry_content e1))) then
			    stretch (e1 :: e1s) e2s
			else
			    stretch
			      (sel_next e1s
			                (path_to(#1(sel_entry_content e1))))
			      (e2 :: e2s)
		  | stretch _ _                         = ()

		fun get_coord path ((e : state_entry) :: es) =
		    if #1(sel_entry_content e) = path then
			#2(sel_entry_content  e)
		    else get_coord path es

		fun selected path id ob cid b _ =
		    (if not(#1(valOf(!SELECTED)) = path) handle _ => true then
			 let
			     val (x, y)   = get_coord path (!STATE)
			 in
			     if isSome(!SELECTED) then
				 let
				     val sel      = valOf(!SELECTED)
				     val (sx, sy) = get_coord(#1 sel) (!STATE)
				     val sel_f    = selected (#1 sel) (#2 sel)
					                     (#3 sel) (#4 sel)
							     true
				 in
				     (addConf (#2(valOf(!SELECTED)))
				              [Relief Flat, Foreground Black,
					       Background White];
				      delCItem widId (#4 sel);
				      addCItem widId
				               (CIcon {citemId  = #4 sel,
						       coord    =
						         (sx + 12, sy),
						       iconkind =
						         Obj.icon(#3 sel),
						       configs  =
						         [Anchor West],
						       bindings =
						         [BindEv
							    (Enter,
							     enter(#2 sel)),
							  BindEv
							    (Leave,
							     leave(#2 sel)),
							  BindEv
							    (ButtonPress
							       (SOME 1),
							     sel_f)]}))
				 end
			     else ();
			     SELECTED := SOME(path, id, ob, cid);
			     addConf id [Relief Sunken, Foreground White,
					 Background Grey];
			     delCItem widId cid;
			     addCItem widId (CIcon {citemId  = cid,
						    coord    = (x + 12, y),
						    iconkind =
						      Obj.selected_icon ob,
						    configs  = [Anchor West],
						    bindings =
						      [BindEv(Enter, enter id),
						       BindEv(Leave, leave id),
						       BindEv
						         (ButtonPress(SOME 1),
							  selected path id ob
							           cid
								   true)]});
			     if b then hist_append path else ();
			     selection_notifier (SOME ob)
			 end
		     else ())

		fun reselect() =
		    if isSome(!SELECTED) then
			(let
			     val cid   = #4(valOf(!SELECTED))
			     val item  = getCItem widId cid
			     val coord = hd(selItemCoords item)
			     val bind  = selItemBind item
			     val ob    = #3(valOf(!SELECTED))
			 in
			     delCItem widId cid;
			     addCItem widId (CIcon {citemId  = cid,
						    coord    = coord,
						    iconkind =
						      Obj.selected_icon ob,
						    configs  = [Anchor West],
						    bindings = bind})
			 end;
			 addConf (#2(valOf(!SELECTED)))
			         [Background Grey, Foreground White,
				  Relief Sunken])
			 handle _ => (SELECTED := NONE;
				      selection_notifier NONE)
		    else ()

		fun single_entry p ob (x, y) root upper =
		    let
			val path = p @ [newEntryId()]
			val id   = newWidgetId()
			val cid  = newCItemId()
			val dy   =
			    case upper of
				NONE       => 11
			      | SOME true  => 16
			      | SOME false => 20
			val binds1 = [BindEv(Enter, enter id),
				      BindEv(Leave, leave id),
				      BindEv(ButtonPress(SOME 1),
					     selected path id ob cid true)]
		    in
			if Obj.is_leaf ob then
			    STATE_ENTRY
			      (path, (x, y), NONE,
			       [CLine {citemId  = newCItemId(),
				       coords   = [(x + 4, y), (x + 12, y)],
				       configs  = [Width 2],
				       bindings = []},
				CIcon {citemId  = cid,
				       coord    = (x + 12, y),
				       iconkind = Obj.icon ob,
				       configs  = [Anchor West],
				       bindings = binds1},
				CWidget {citemId  = newCItemId(),
					 coord    = (x + 32, y + 2),
					 widgets  =
				           Pack
					     [Label {widId    = id,
						     packings = [],
						     configs  =
						       [Text(Obj.sel_name ob),
							Background White,
							Font font],
						     bindings = binds1}],
					 configs  = [Anchor West],
					 bindings = []}],
			       ref (CLine {citemId  = newCItemId(),
					   coords   = [(x + 4, y),
						       (x + 4, y - dy)],
					   configs  = [Width 2],
					   bindings = []}),
			       ref [], ob, id, cid, ref NONE)
			else
			    STATE_ENTRY
			      (path, (x, y),
			       if root then SOME true else SOME false,
			       [CLine {citemId  = newCItemId(),
				       coords   = [(x + 8, y), (x + 12, y)],
				       configs  = [Width 2],
				       bindings = []},
				CRectangle {citemId  = newCItemId(),
					    coord1   = (x, y - 4),
					    coord2   = (x + 8, y + 4),
					    configs  = [],
					    bindings =
					      [BindEv(ButtonPress(SOME 1),
						      pressed_obj ob path)]},
				CIcon {citemId  = cid,
				       coord    = (x + 12, y),
				       iconkind = Obj.icon ob,
				       configs  = [Anchor West],
				       bindings = binds1},
				CWidget {citemId  = newCItemId(),
					 coord    = (x + 32, y + 2),
					 widgets  =
					   Pack
					     [Label {widId    = id,
						     packings = [],
						     configs  =
						       [Text(Obj.sel_name ob),
							Background White,
							Font font],
						     bindings = binds1}],
					 configs  = [Anchor West],
					 bindings = []}],
			       ref (CLine {citemId  = newCItemId(),
					   coords   = [(x + 4, y - 4),
						       (x + 4, y - dy)],
					   configs  = [Width 2],
					   bindings = []}),
			       ref (if root then
					minus (x, y)
					      [BindEv(ButtonPress(SOME 1),
						      pressed_obj ob path)]
				    else
					plus (x, y)
					     [BindEv(ButtonPress(SOME 1),
						     pressed_obj ob path)]),
			       ob, id, cid, ref NONE)
		    end

		and pressed_obj ob path _ =
		    let
			fun shift (STATE_ENTRY(path, coord, bopt, cits, cit,
					       cits2, obj, id, cid, subents)
				   :: es) delta =
			    let
				fun shift_coord (x, y) = (x, y + delta)

				fun shift_citem cit =
				    updItemCoords cit
				      (map shift_coord (selItemCoords cit))
			    in
				STATE_ENTRY(path, shift_coord coord, bopt,
					    map shift_citem cits,
					    ref(shift_citem (!cit)),
					    ref(map shift_citem (!cits2)),
					    obj, id, cid, subents)
				:: shift es delta
			    end
			  | shift [] _          = []

			val (state1, rest, oldst) =
			    let
				fun invert(STATE_ENTRY(path, coord, b, cits,
						       cit, cits2, obj, id,
						       cid, subents)) =
				    let
					val oldst = valOf b

					val bi = selItemBind(hd(!cits2))

					val newcits2 =
					    if oldst then ref (plus coord bi)
					    else ref (minus coord bi)
				    in
					app (delCItem widId)
					    (map selItemId (!cits2));
					app (addCItem widId) (!newcits2);
					(oldst,
					 STATE_ENTRY(path, coord,
						     SOME(not oldst), cits,
						     cit, newcits2, obj, id,
						     cid, subents))
				    end

				fun sep ((e : state_entry) :: es) s1 =
				    if #1(sel_entry_content e) = path then
					let
					    val (oldst, inverted) = invert e
					in
					    (s1 @ [inverted], es, oldst)
					end
				    else sep es (s1 @ [e])
			    in
				sep (!STATE) []
			    end
		    in
			if oldst then (* close *)
			    let
				fun sep ((e : state_entry) :: es) subs =
				    if (sub_path (#1(sel_entry_content e))
					         path) then
					sep es (subs @ [e])
				    else (subs, e :: es)
				  | sep [] subs                        =
				    (subs, [])

				val (todelete, oldstate2) = sep rest []

				val delta =
				    #2(#2(sel_entry_content (hd todelete))) -
				    #2(#2(sel_entry_content
					    (List.last todelete))) - 20

				val state2 = shift oldstate2 delta

				val maxy =
				    Int.max(if null state2 then
						#2(#2(sel_entry_content
						        (List.last state1)))
						+ 12
					    else #2(#2(sel_entry_content
						         (List.last state2)))
						+ 12,
					    height)
			    in
				#10(sel_entry_content(List.last state1)) :=
				  SOME(todelete,
				       #2(#2(sel_entry_content
					       (List.last state1))));
				stretch (rev state1) state2;
				app (delCItem widId)
				    (map selItemId
				         (List.concat
					    (map (#4 o sel_entry_content)
					         rest) @
					  map (! o (#5 o sel_entry_content))
					      rest @
					  List.concat
					    (map (! o (#6 o sel_entry_content))
					         rest)));
				app (addCItem widId)
				    (List.concat(map (#4 o sel_entry_content)
						     state2) @
				     map (! o (#5 o sel_entry_content))
				         state2 @
				     List.concat
				       (map (! o (#6 o sel_entry_content))
					    state2));
				STATE := state1 @ state2;
				addConf widId [ScrollRegion(0, 0, 0, maxy)];
				reselect()
			    end (* close *)
			else (* open *)
			    if (isSome(!(#10(sel_entry_content
					       (List.last state1))))) then
				let (* open with known content *)
				    val delta1 =
					#2(#2(sel_entry_content
					        (List.last state1))) -
					#2(valOf(!(#10(sel_entry_content
						         (List.last state1)))))

				    val state2 =
					shift (#1(valOf(!(#10(sel_entry_content
							        (List.last
							           state1))))))
					      delta1

				    val delta2 =
					#2(#2(sel_entry_content
					        (List.last state2))) -
					#2(#2(sel_entry_content
					        (List.last state1)))

				    val state3 = shift rest delta2

				    val maxy =
					Int.max
					  (if null state3 then
					       #2(#2(sel_entry_content
						     (List.last state2))) + 12
					   else
					       #2(#2(sel_entry_content
						     (List.last state3))) + 12,
					   height)
				in
				    stretch (rev state1) state3;
				    app (delCItem widId)
				        (map selItemId
					   (List.concat
					      (map (#4 o sel_entry_content)
					           rest) @
					    map (! o (#5 o sel_entry_content))
					        rest @
					    List.concat
					      (map
					         (! o (#6 o sel_entry_content))
					         rest)));
				    app (addCItem widId)
			              (List.concat
				         (map (#4 o sel_entry_content)
					      state2) @
					  map (! o (#5 o sel_entry_content))
					      state2 @
					  List.concat
					    (map (! o (#6 o sel_entry_content))
					         state2) @
					  List.concat
					    (map (#4 o sel_entry_content)
					         state3) @
					  map (! o (#5 o sel_entry_content))
					      state3 @
					  List.concat
					    (map (! o (#6 o sel_entry_content))
					         state3));
				    STATE := state1 @ state2 @ state3;
				    addConf widId
				            [ScrollRegion(0, 0, 0, maxy)];
				    reselect()
				end (* open with known content *)
			    else
				let (* open new *)
				    val (x, y) = #2(sel_entry_content
						    (List.last state1))
				    val (state2, delta) =
					subtree path ob (x + 17, y + 20)

				    val state3 = shift rest delta

				    val maxy =
					Int.max
					  (if null state3 then
					       #2(#2(sel_entry_content
						     (List.last state2))) + 12
					   else
					       #2(#2(sel_entry_content
						     (List.last state3))) + 12,
					   height)
				in
				    stretch (rev state1) state3;
				    app (delCItem widId)
				        (map selItemId
					   (List.concat
					      (map (#4 o sel_entry_content)
					           rest) @
					    map (! o (#5 o sel_entry_content))
					        rest @
					    List.concat
					      (map
					         (! o (#6 o sel_entry_content))
					         rest)));
				    app (addCItem widId)
			              (List.concat
				         (map (#4 o sel_entry_content)
					      state2) @
					  map (! o (#5 o sel_entry_content))
					      state2 @
					  List.concat
					    (map (! o (#6 o sel_entry_content))
					         state2) @
					  List.concat
					    (map (#4 o sel_entry_content)
					         state3) @
					  map (! o (#5 o sel_entry_content))
					      state3 @
					  List.concat
					    (map (! o (#6 o sel_entry_content))
					         state3));
				    STATE := state1 @ state2 @ state3;
				    addConf widId
				            [ScrollRegion(0, 0, 0, maxy)];
				    reselect()
				end (* open new *)
		    end (* val pressed_obj *)

		and subtree p ob (x, y) =
		    let
			fun subtree' (ob :: obs) (l, ny) upper =
			    let
				val ent =
				    single_entry p ob (x, ny) false upper

				val newupper =
				    SOME(isSome(#3(sel_entry_content ent)))
			    in
				subtree' obs (l @ [ent], ny + 20) newupper
			    end
			  | subtree' [] (l, ny) _              = (l, ny - y)
		    in
			subtree' (Obj.children ob) ([], y) NONE
		    end


(*--- navigation / history --------------------------------------------------*)

		fun selection() =
		    if isSome(!SELECTED) then SOME(#3(valOf(!SELECTED)))
		    else NONE

		fun up() =
		    if isSome(!SELECTED) then
			let
			    fun sep ((e : state_entry) :: es) =
				if (#1(sel_entry_content e) =
				    #1(valOf(!SELECTED))) then []
				else e :: sep es

			    fun seek ((e : state_entry) :: es) =
				if (length(#1(sel_entry_content e)) <
				    length(#1(valOf(!SELECTED)))) then
				    SOME e
				else seek es
			      | seek []                        = NONE

			    val to_select = seek(rev(sep(!STATE)))
			in
			    if isSome(to_select) then
				selected
				  (#1(sel_entry_content(valOf(to_select))))
				  (#8(sel_entry_content(valOf(to_select))))
				  (#7(sel_entry_content(valOf(to_select))))
				  (#9(sel_entry_content(valOf(to_select))))
				  true dummy_event
			    else ()
			end
		    else ()

		fun position() =
		    if null(!HISTORY) orelse length(!HISTORY) = 1 then
			hist_empty
		    else
			if length(!HISTORY) = !HISTORY_POINTER then hist_end
			else
			    if !HISTORY_POINTER = 1 then hist_start
			    else hist_middle

		fun seek_and_open ((e : state_entry) :: es) path =
		    if path = (#1(sel_entry_content e)) then e
		    else
			if (sub_path path (#1(sel_entry_content e))
			    andalso isSome(#3(sel_entry_content e))
			    andalso (not(valOf(#3(sel_entry_content e))))) then
			    (pressed_obj (#7(sel_entry_content e))
			                 (#1(sel_entry_content e))
					 dummy_event;
			     seek_and_open (!STATE) path)
			else  seek_and_open es path

		fun back() =
		    if (position() = hist_start orelse
			position() = hist_empty) then ()
		    else
			(HISTORY_POINTER := !HISTORY_POINTER - 1;
			 let
			     val p = List.nth(!HISTORY, !HISTORY_POINTER - 1)

			     val to_select = seek_and_open (!STATE) p
			 in
			     selected (#1(sel_entry_content(to_select)))
			              (#8(sel_entry_content(to_select)))
				      (#7(sel_entry_content(to_select)))
				      (#9(sel_entry_content(to_select)))
				      false dummy_event
			 end)

		fun forward() =
		    if (position() = hist_end orelse
			position() = hist_empty) then ()
		    else
			(HISTORY_POINTER := !HISTORY_POINTER + 1;
			 let
			     val p = List.nth(!HISTORY, !HISTORY_POINTER - 1)

			     val to_select = seek_and_open (!STATE) p
			 in
			     selected (#1(sel_entry_content(to_select)))
			              (#8(sel_entry_content(to_select)))
				      (#7(sel_entry_content(to_select)))
				      (#9(sel_entry_content(to_select)))
				      false dummy_event
			 end)

(*--- initialize ------------------------------------------------------------*)

		fun init ob =
		    let
			val root   = single_entry [] ob (10, 10) true NONE
			val rootId = hd(#1(sel_entry_content root))
			val ents   = root :: #1(subtree [rootId] ob (27, 30))
			val maxy   =
			    Int.max(#2(#2(sel_entry_content(List.last ents)))
				    + 12, height)
		    in
			SELECTED        := NONE;
			STATE           := ents;
			HISTORY         := [];
			HISTORY_POINTER := 0;
			(ScrollRegion (0, 0, 0, maxy),
			 List.concat(map (#4 o sel_entry_content) ents) @
			 map (! o (#5 o sel_entry_content)) ents @
			 List.concat(map (! o (#6 o sel_entry_content)) ents))
		    end

		fun check ob =
		    if Obj.is_leaf ob then
			(print "LazyTrees: tree with leaf object";
			 raise Error "LazyTrees: tree with leaf object")
		    else ob
	    in
		{canvas    =
		   fn ob =>
		   let
		       val (init_scroll, init_citems) = init ob
		   in
		       Canvas {widId      = widId,
			       scrolltype = RightScb,
			       citems     = init_citems,
			       packings   = [],
			       configs    = [Width width, Height height,
					     init_scroll,
					     Background White],
			       bindings   = []}
		   end,
	         selection = selection,
	         up        = up,
		 position  = position,
		 back      = back,
		 forward   = forward}
	    end (* val tree_list *)
    end (* functor LazyTrees *)
