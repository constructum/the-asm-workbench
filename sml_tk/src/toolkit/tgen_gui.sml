(* ***************************************************************************

   $Source: /repository/sml/sml_tk/src/toolkit/tgen_gui.sml,v $

   A generic graphical user interface,
   including a navigation pad, but still generic
   in the construction area. 
  
   See <a href=file:../../doc/manual.html>the documentation</a> for more
   details.  
   "tests+examples/tsimpleinst.sml" contains a small example of how to
   use this package.
 
   $Date: 2001/03/30 13:39:52 $
   $Revision: 3.0 $

   Author: bu (Last modification $Author: 2cxl $)

   (C) 2000, Albert Ludwigs Universität Freiburg

  ************************************************************************** *)



signature TGENGUI_SIG =
  sig
     include GENGUI_SIG;
     structure TreeObj : PTREE_OBJECT_CLASS;
     val create_folder : SmlTk.Coord -> unit
  end
     

functor TGenGUI(structure appl: APPL_SIG ) (* : TGENGUI_SIG *) = 

  struct 
    
    local open SmlTk BasicUtil in


    val default_printmode={mode       = Print.Long,
                           printdepth = 100,
                           height     = NONE,
                           width      = NONE}  (* the value is temporary *)

    fun debugmsg msg = Debug.print 11 ("GenGUI: "^msg)

    fun bool2string true  = "true"
       |bool2string false = "false"

(* ************************************************************************ *)
(*                                                                          *)
(* Pumping the data-model of APPL (the object_class) on object_class_trees  *)
(*                                                                          *)
(* ************************************************************************ *)

   structure M : OBJECT_CLASS = appl; (* Take just appl, reduce it to its
                                         Onjectclass, and bind it to M *)

(* I'm not that sure if folder names should remain strings.
   We must assure uniqueness here - and the string-solution
   would require that update-operations check this constraint
   as invariant. Alternative: unique key's... 
   I'll think about it *)

   structure N : FOLDERINFO = 
                 struct 
                     type node_info = string ref * (SmlTk.Coord * 
                                                    SmlTk.AnchorKind)
                     type subnode_info = SmlTk.Coord * SmlTk.AnchorKind
                     fun  ord_node ((x,_),(y,_)) = String.compare(!x,!y) 
                     fun  string_of_name_node (s,_) _ = !s
                     fun  rename_node s (t,_)   = (t:=s)
                     fun  reset_name_node (s,_) = (s:="...")
                 end
  
   structure TreeObj = obj2tree_obj(structure N = N and M = M);

   fun name2string x = TreeObj.string_of_name 
                          (TreeObj.path2name x)
                             default_printmode

   open TreeObj;       (* From now on, we'll have treeobjects as objects *) 

(* ************************************************************************ *)
(*                                                                          *)
(* TGengui-State                                                            *)
(*                                                                          *)
(* ************************************************************************ *)


   exception GenGUI of string

   (* the state comprises a focus and a list of (tree)-objects *)
   type gui_state = path * object list
   type new_object= object (*distinction no longer necessary *)

   val  root      = ([]:node_info list, NONE: Basic.object option)

   val  gui_state = ref(root,[]:object list)

   val  folder_id = ref(0)

   fun  select_object_from_guistate path = 
           select_from_path (snd(!gui_state)) path

   fun  eq x y = case ord(x,y) of 
                   EQUAL => true
                 | _     => false

   (* merging into state is useful to keep orders of objects whenever possible.
      Keeps the presentation og navi-board smooth - Notepad.state() yields
      the state in quite arbitrary orders . . . *)
   fun  merge S' S'' =
        let val b = (List.mapPartial (fn x => List.find (fn y => eq x y) S'') S') 
            val c = (List.filter (fn x => not(List.exists (fn y => eq x y) S')) S'')
        in  b @ c end;


   fun  update_object_in_guistate ([],NONE) objs =
           gui_state := (fst(!gui_state), merge(snd(!gui_state)) objs)
      | update_object_in_guistate path [obj] =
           let val (p,objs) = !gui_state
           in  gui_state:=(p,update_at_path objs path obj)
              (* handle InconsistPath => () *)
           end


   (* hooks, refreshManagement; ah, the joys of linear visibility *)

   val elim_ob_hook     = ref(NONE:(object -> unit)option)
   val intro_ob_hook    = ref(NONE:(object * (SmlTk.Coord * SmlTk.AnchorKind) -> unit)option)
   val refresh_lab_hook = ref(NONE:(unit->unit)option);
   val sync_hook        = ref(NONE:(bool->unit)option);
   val sync_back_hook   = ref(NONE:(unit->unit)option);
   val refocus_hook     = ref(NONE:(TreeObj.path->unit)option);

   (* used in order to delay sync's, for example for drag-Drop-Actions.
      For efficiency reasons. Could be made easily mor eager . . . *)
   val forceRefresh = ref false;


(* ************************************************************************ *)
(*                                                                          *)
(* The common state: the clipboard                                          *)
(*                                                                          *)
(* ************************************************************************ *)

(* The application clipboard-structure (appl.CB) is a flat clipboard - 
   it must be adapted to the above clipboard with tree-objects. *)

    structure CB_ca = appl.CB; (* external clipboard for 
                                  communication with ca *)

    type objectlist = unit -> object list 
    structure CB_nn = Clipboard(type obj = objectlist)         
                               (* internal clipboard for communication
                                  between navigation and notepad *)     


    fun treeobj2obj x = if isFolder x 
                        then List.concat(List.map treeobj2obj(snd(getFolder x)))
                        else [fst (getContent x)]

    structure CB =  (* sort of "joined clipboard" *)
        struct type obj= CB_nn.obj
               exception Empty = CB_nn.Empty
	       fun  put it ev cb = (CB_nn.put it ev cb;
			            CB_ca.put (appl.cb_objects_abs
                                                   (fn ()=> List.concat(List.map 
                                                         treeobj2obj (it())))) ev cb)
               fun  get ev = if CB_nn.isEmpty ev
                             then fn () => (List.map (fn x=>Content(x,((10,10),Center)))
                                                     (appl.cb_objects_rep
                                                             (CB_ca.get ev)())
                                            handle CB_ca.Empty => raise Empty)
                                  
                             else CB_nn.get ev

	       fun  copy ev =if CB_nn.isEmpty ev
                             then fn () => (List.map (fn x => Content(x,((10,10),
                                                                          Center)))
                                                      (appl.cb_objects_rep
                                                           (CB_ca.copy ev)())
                                            handle CB_ca.Empty => raise Empty)
                             else CB_nn.copy ev
 

	       fun  isEmpty ev = CB_nn.isEmpty ev andalso CB_ca.isEmpty ev
               
 	end


(* ************************************************************************ *)
(*                                                                          *)
(* Instantiate Navigation Board                                             *)
(*                                                                          *)
(* ************************************************************************ *)

   (* the navigationboard *)

   structure NaviBoardActions : TL_ACTION =
                 struct
                     type object       = TreeObj.object
                     type node_info    = TreeObj.node_info
                     type subnode_info = TreeObj.subnode_info
                     type path         = TreeObj.path

                     fun  content_label_action {path, was, cc} = 
                          UW.enterLine{title="enter label:",
                                       prompt="",default=was,width=30, 
                                       cc= fn s => (cc s; valOf(!refresh_lab_hook)())}

                     fun  objtree_change_notifier {changed_at:path} =
                            (debugmsg ( "general change notifier at :"^
                                        (name2string changed_at) ^ "\n");
                             valOf(!sync_back_hook)())



                     fun  focus_change_notifier {changed_at:path list} = 
                            (debugmsg ( "notifier activated at :" ^ 
                                     (String.concat 
                                     (map name2string changed_at)) ^
                                     "\n");
                             valOf(!refocus_hook)(hd(changed_at)))
                        
                     fun  open_close_notifier {is_open:bool, 
                                               changed_at:path list} = 
                            (debugmsg ( "open/close notifier activated at :" ^ 
                                     (String.concat 
                                     (map name2string changed_at)) ^ 
                                     " is_open:" ^
                                     (bool2string is_open) ^
                                     "\n");
                             (* sync just for subtree . . . >>> *)
                             if is_prefix(hd changed_at,fst(!gui_state))
                             then valOf(!sync_hook)(false)
                             else ())


                     fun  error_action          s = (UW.error "ERROR" )
                 end

    structure NaviBoard = 
                 TreeList(structure S = struct
                                          structure M  = TreeObj;
                                          structure A  = NaviBoardActions;
                                          type objlist = unit -> object list;
                                          structure CB = CB;
                                        end);

    val _ = (#width(NaviBoard.Config) := 120);

    fun navi_board win objs = NaviBoard.create_canvas objs


(* ************************************************************************ *)
(*                                                                          *)
(* Instantiate Notepad                                                      *)
(*                                                                          *)
(* ************************************************************************ *)


    (* the construction area frame widget id *)
    val caFrameId  = mkWidgetId("ca")
    (* the widget id of the canvas all the items are placed on *)

    (* flag indicating wether the construction area is currently open *)
    val caOpen     = ref (NONE: object option)

    (* and a function to check that *)
    fun isOpen ob = case (!caOpen) of 
                       NONE => false 
                     | SOME ob2 => case ord(ob,ob2) of
                                     EQUAL => true
                                   | _     => false


    fun fst (x,_) = x
    fun snd (_,x) = x
    fun K x y = x

    (* split : (node_info * object list -> 'a) * 
               (Data.object * subnode_info -> 'a)
               -> object -> 'a *)
    fun split (f,g) obj = 
        if isFolder obj then f(getFolder obj) else g(getContent obj)

    fun object2newobject ob = (ob,split(snd o fst,snd) ob)
    fun newobject2object (ob,pos) = split(fn((n,_),ol)=>Folder((n,pos),ol), 
                                          fn(ob,_)=>Content(ob,pos)) ob


    (* This could be done genericly : *)

    val label_action =(fn {obj,cc} => 
              (if isFolder obj then
                  UW.enterLine{title="Renaming folder", 
                               default="",
			       prompt="Please enter new name: ",
			       width= 20, 
                               cc=fn txt=> (rename_node txt(fst(getFolder obj));
                                            forceRefresh:=true; (* shut up optimize !!! *)
                                            NaviBoard.refresh_label())}
               else appl.label_action{obj=fst(getContent obj),
                                      cc=(fn txt =>(cc txt;
                                                    NaviBoard.refresh_label()))}))


    fun is_constructed ot = if isFolderType ot then false
                            else appl.is_constructed(getContentType ot)

    type mode = appl.mode option

    fun  mode ot = if isFolderType ot then NONE
                   else SOME(appl.mode(getContentType ot))

    fun  modes ot= if isFolderType ot then []
                   else map SOME (appl.modes (getContentType ot))

    (* much better ... *)
    fun objlist_type [] = NONE
       |objlist_type [a] = SOME(obj_type a)
       |objlist_type (a::R) = 
           if List.exists(fn oo => not(obj_type a = obj_type oo)) R then NONE
           else SOME(obj_type a)

    fun mode_name NONE = "Folder"
       |mode_name (SOME x) = appl.mode_name x

    fun set_mode (ob,m) = 
          split (K (), fn (ob',_) => appl.set_mode(ob',valOf m)) ob

    val outline = split (K false, appl.outline o fst)

    fun delete ob = 
        let fun rem ob obS = List.filter (fn x => not(ord(x,ob)=EQUAL)) obS
            val SS = rem ob (snd(!gui_state))
            val p = fst(!gui_state)
        in  split (K (), appl.delete o fst) ob; (* logical delete in application *)
            case p of
               ([],NONE) => (gui_state:=(p,SS);
                             NaviBoard.upd_guistate p (SS);
                             NaviBoard.refresh p)
            |  (M,NONE) =>  (let val (n,SS) = getFolder (select_from_path 
                                                            (snd(!gui_state)) (p))
                                 val nuFol = [Folder(n,rem ob SS)] 
                             in update_object_in_guistate p nuFol;
                                NaviBoard.upd_guistate p nuFol;
                                NaviBoard.refresh p
                             end)
        end;

     fun  create_folder (x,y) = 
          (let val ()  = folder_id:= (!folder_id) + 1;
               val fol = Folder((ref("Folder "^(Int.toString (!folder_id))),
                                   ((x,y),Center)), (* will be overwritten by 
                                                         placing . . . *)
                                   [])
           in  (valOf(!intro_ob_hook)) (object2newobject fol);  (* doesn't work well 
                                                         does not acticate 
                                                         "findNicePlace" for 
                                                         some reason *)
               (valOf(!sync_hook)) true
           end);

    fun show_folder x = 
        let val ((nm,_),ol) = getFolder x
            val tx = "This is a folder with "^
                      (Int.toString (length ol))^" subjects"
        in  UW.display{title= !nm, width= 40, height= 20,
		   text= mkAT tx, cc= fn _ => ()}
        end;

    fun stat_folder tO = show_folder tO (* preliminary HACK ! *)



    fun std_ops ot = 
	if isFolderType ot then [(show_folder, "Show"), 
		                 (stat_folder, "Info")]
	else map (fn (f,s) => (split(K(),f o fst),s)) 
	         (appl.std_ops (getContentType ot))

    fun mon_ops ot = 
        if isFolderType(ot) 
        then [] (* so far *)
        else let fun conv(h)(ob,c)(f)= h(fst(getContent ob),c)
                                        (fn(x,y)=>(f(Content(x,y),y)))
             in map (fn(h,s)=>(conv h,s))(appl.mon_ops (getContentType ot))
             end;



    fun bin_ops (ot1,ot2) = 
        if isFolderType(ot1) 
        then (* moving >>> *)
             (let fun H(ob,coord,obS,f) = (app (valOf(!elim_ob_hook)) obS;
                                           (* <<< delete items from Notepad *)
                                           valOf(!sync_hook)(true); 
                                           (* <<< sync notepad with state and redisplay *)
                                           (* <<< extremely EXPENSIVE. Could be replaced 
                                              by something that just deletes CITEMS -- 
                                              commenting out results just in WARNINGS like:
                                              WARNING: Exception CITEM: item: anocid115xIcon 
                                              not found *)

                                           (* update substate in gui_state und nv-board *)
                                           let val pp = concat_path((fst (!gui_state)),
                                                                    (name_of ob))
                                               val (n,SS) = getFolder(select_from_path 
                                                                         (snd(!gui_state))
                                                                         (pp))
                                           in update_object_in_guistate pp [Folder(n,SS@obS)];
                                              NaviBoard.upd_guistate pp [Folder(n,SS@obS)];
                                              NaviBoard.refresh pp;
                                              (* replace tree-object on note-pad - 
                                                 otherwise correspondence of all three 
                                                 states violated (data-invariant) and
                                                 sync will not work in future >>> *)
                                              (valOf(!elim_ob_hook)) ob;
                                              (valOf(!intro_ob_hook)) 
                                                  (Folder(n,SS@obS),(coord,Center))
                                           end 
                                          )
              in SOME H end) 
        else (let fun loc_refresh (p as ([],NONE)) ob = (let val SS =snd(!gui_state) 
                                                         in gui_state:=(p,SS@[ob]);
                                                            NaviBoard.upd_guistate p (SS@[ob]);
                                                            NaviBoard.refresh p
                                                         end)
                     |loc_refresh (p as (M,NONE)) ob = (let val (n,SS) = getFolder
                                                                         (select_from_path 
                                                                         (snd(!gui_state))
                                                                         (p))
                                                            val nuFol = [Folder(n,SS@[ob])] 
                                                        in update_object_in_guistate p nuFol;
                                                           NaviBoard.upd_guistate p nuFol;
                                                           NaviBoard.refresh p
                                                        end)
                                                         
                  val pred = split(K NONE, SOME o fst)
                                          (* filtering folder objects *)
                  fun H(h)(ob,c,obS,f) = h(fst(getContent ob),c,
                                           List.mapPartial pred obS,
                                           (fn(x,y)=>(loc_refresh (fst(!gui_state)) 
                                                                  (Content(x,y));
                                                      f(Content(x,y),y))))
              in if isFolderType(ot2) 
                 then NONE(* dragging folders on basic objs: not considered useful! *)
                 else case appl.bin_ops(getContentType ot1,getContentType ot2) of
                        SOME f => SOME(H f)
                       |NONE   => NONE
              end)


    fun openConArea {win,obj,replace_object_action,outline_object_action} =
	let
	    (* id of the window holding the con/area widgets *)
	    val cawin = if appl.Conf.oneWindow then win 
			else newWinId()
	    (* bindings for the con area while open *)
	    fun caEnter wsp ev =
		let val dropobs = appl.cb_objects_rep(appl.CB.copy ev)()
		    val oot     = appl.objlist_type dropobs
		in  case oot of 
		    SOME ot => appl.area_ops ot wsp dropobs
		  | NONE => () 
		end handle appl.CB.Empty => ()
	    fun caBindings wsp = 
		[BindEv(Enter, caEnter wsp)]
	    (* bindings for the con/area while closed *)
	    val caClosedBindings =
		[BindEv(Enter, K0)]
	    fun closeConArea nuOb =
		(caOpen := NONE;
		 replace_object_action nuOb;
		 if appl.Conf.oneWindow then 
		     app (delWidget o selWidgetId) 
		         (selWidgets (getWidget caFrameId))
		 else 
		     closeWindow cawin
		)
	in
	    if (is_constructed (obj_type obj)) 
		andalso not (Option.isSome (!caOpen)) then
		let 
		    (* get the con/area widgets from the application: *)
                    val (bobj,_) = getContent obj (* Folder are never 
                                                     constrcution objects...*)
		    val (wsp, wwidgs, init) = appl.area_open(cawin, bobj, 
							     closeConArea)
		    (* add con/area bindings to widgets: *)
		    val wwidgs = map (fn w=> updWidgetBind w 
				               ((caBindings wsp)@
						(selWidgetBind w)))
                        			 wwidgs			 
		in 
		    (outline_object_action ();
		     (* set flag *)
		     caOpen := SOME obj;
		     if appl.Conf.oneWindow then
			 (app (addWidget win caFrameId) wwidgs;
			  addBind caFrameId (caBindings wsp);
			  init()
			 )
		     else openWindow(mkWindow{
			   winId=cawin, 
			   config= [WinTitle (appl.Conf.caTitle 
					      (appl.string_of_name
                                                      (appl.name_of bobj)
                                                      (default_printmode))),
				    WinGeometry (SOME(appl.Conf.caWidth,
						      appl.Conf.caHeight),
						 appl.Conf.caXY),
				    WinGroup win], 
			   widgets= Pack wwidgs, 
			   bindings = [], init=init})
		     )
		end	    
	    else
		debugmsg "Not a primary object, or ConArea already open."
     end
 
     fun object_action{obj,win,
                       outline_object_action,
                       replace_object_action} =
         let fun name2pathApp (p,M) a = (a@p,M)
         in  (if isFolderType (obj_type obj) 
              then (valOf(!refocus_hook)
                         (name2pathApp(name_of obj) 
                         (fst(fst(!gui_state)))))
              else (openConArea  {obj=obj, win=win,
                       outline_object_action = outline_object_action,
                       replace_object_action = fn x => 
                                                 replace_object_action
                                                   (Content(x,((10,10),Center)))}))
         end

    
     structure notepadappl = 
         struct 
            open TreeObj;

            type mode = mode
            val  mode = mode
            val  modes= modes
            val  mode_name = mode_name
            val  set_mode  = set_mode
            val  outline   = outline
            val  std_ops   = std_ops
            val  mon_ops   = mon_ops
            val  bin_ops   = bin_ops  
            val  delete    = delete

            type new_object = object * (SmlTk.Coord * SmlTk.AnchorKind)

            val objlist_type = objlist_type

            val is_constructed = is_constructed
            
            val object_action = object_action
                
            val label_action  = label_action

            val create_actions = [(fn {pos=(x,y),tag=str} => create_folder(x,y),"folder")] 
            
            val is_locked_object = isOpen
            fun init () = map (fn x=> (Content x,snd x)) (appl.init()) 

            structure Conf = appl.Conf

            type objectlist = unit -> object list

            structure CB = CB;

         end

    structure Notepad = Notepad(structure appl = notepadappl);



(* ************************************************************************ *)
(*                                                                          *)
(* sync'ing and refresh                                                     *)
(*                                                                          *)
(* ************************************************************************ *)



     (* sync - routines = synchronize distributed state + refresh. *)
     (* implemented unidirectional for efficiency reasons *)
     (* here: sync notepad-content with gui_state, and gui_state with *)
     (* navi-board-state. *)
     fun sync_notepad_with_guistate refreshNB = 
         let val path  = fst(!gui_state)
             val () = debugmsg("sync: path >" ^ (name2string path) ^ "<\n");
             
         in  (case path of 
             (* sync at root position *)
               ([],NONE) => (update_object_in_guistate path 
                                   (map newobject2object(Notepad.state()));
                             NaviBoard.upd_guistate path (snd(!gui_state));
                             debugmsg "sync root refresh\n")
             (* sync somewhere deep inside . . .*)
             | (_,NONE)  =>  let val (n,S') = getFolder
                                            (select_object_from_guistate path)
                                 val S'' = map newobject2object(Notepad.state())
                                 val fol = Folder(n,merge S' S'')
                             in  update_object_in_guistate path [fol];
                                 NaviBoard.upd_guistate path [fol];
                                 debugmsg "sync inside refresh\n"
                             end);
             if refreshNB then NaviBoard.refresh path else ()
         end

     fun sync_naviboard_into_guistate () = 
         let val (ap,st) = !gui_state
             val ()      = gui_state := (ap,NaviBoard.get_guistate())
             val newobs  = case ap of
                             ([],NONE) => snd(!gui_state)
                           | (_, NONE) => snd(getFolder(
                                                   select_object_from_guistate
                                                   ap))
         in  Notepad.init (map object2newobject newobs)
         end;
         (* necessary for sync'ing name-changes and moves from
            the naviboard with the notepad (done since names are shared)
            and rearrangements within the naviboard. Thats not implemented 
            so far. . . *) 
                              
   (*        
     fun  create_folder (x,y) = 
          (let val ()  = folder_id:= (!folder_id) + 1;
               val fol = Folder((ref("Folder "^(Int.toString (!folder_id))),
                                   ((x,y),Center)), (* will be overwritten by 
                                                         placing . . . *)
                                   [])
           in  Notepad.intro (object2newobject fol);  (* doesn't work well 
                                                         does not acticate 
                                                         "findNicePlace" for 
                                                         some reason *)
               sync_notepad_with_guistate true
           end);
    *)                      




     fun refocus_notepad (path as (m,NONE))  = 
         let val (act_path,act_state) = !gui_state
             val () = debugmsg("focus: act_path " ^ (name2string act_path) ^ 
                               "\n           path "^(name2string path)^"\n");
             fun doit ()=(sync_notepad_with_guistate true;
                          gui_state := (path, snd(!gui_state));
                          let val newobs = 
                                    case path of 
                                      ([],NONE) => snd(!gui_state)
                                     |(_, NONE) => snd(getFolder(
                                                   select_object_from_guistate
                                                   path))
                          in  Notepad.init (map object2newobject newobs)
                          end)
         in  case ord_path(path,act_path) of
               EQUAL => (if !forceRefresh then doit() else ())
             | _     => doit();
             forceRefresh:=false
         end 
        |refocus_notepad (m,SOME _ ) = refocus_notepad (m,NONE)


(* ************************************************************************ *)
(*                                                                          *)
(* export                                                                   *)
(*                                                                          *)
(* ************************************************************************ *)
   
  
    fun main_wid win =
	let val assArea = Frame{widId= newWidgetId(),
                                 widgets=Pack[Notepad.main_wid win],
                                 packings=[Expand true,Fill X,Fill Y,
                                           Side Right],
				 configs = [],
			         bindings= []}

            val navi    =  Frame{widId= newWidgetId(),
                                 widgets=Pack[navi_board win []],
                                 packings=[Expand true,Fill X,Fill Y,
                                           Side Left],
				 configs = [],bindings= []}
            val assAreaNNavi =  Frame{widId= newWidgetId(),
		                      widgets= Pack[assArea,navi],
				      packings= [Expand true, Fill X, Fill Y, 
					         Side Top],
				      configs = [],
				      bindings= []}
	in if appl.Conf.oneWindow then
	        Frame{widId= newWidgetId(),
		      widgets=Pack [assAreaNNavi,
		    	            Frame{widId=caFrameId, 
		 		          widgets=Pack [], 
				          packings=[Fill X, Side Bottom],
				          configs= [Height appl.Conf.caHeight],
				          bindings=[]}
			          ], 
		      packings= [], bindings= [], configs= []}
	   else  assAreaNNavi
	end
    


     (*  val init  : gui_state-> unit
	 (* call that as init action of main window *) *)
     fun init ((s,NONE), objs) = 
         let fun  distinct eq [] = true
                 |distinct eq (a::R) = (not(List.exists (eq a) R))
                                       andalso (distinct eq R)
                                       (* too naiv. should be recursively *)
             fun eq x y =  case ord(x,y) of EQUAL => true | _ => false
             val () = ( elim_ob_hook     := SOME Notepad.elim;
                        intro_ob_hook    := SOME Notepad.intro;
                        refresh_lab_hook := SOME (fn _ => Notepad.init (
                                                                  Notepad.state())); 
                                                 (* does a complete refresh; a more
                                                    local refresh of labels would increase
                                                    EFFICIENCY here *)
                        sync_hook        := SOME sync_notepad_with_guistate;
                        sync_back_hook   := SOME sync_naviboard_into_guistate;
                        refocus_hook     := SOME refocus_notepad);
         in  if distinct eq objs 
             then(gui_state:=((s,NONE), objs);
                  NaviBoard.upd_guistate (s,NONE) objs;
                  NaviBoard.refresh (s,NONE);
                  if null s (* focus at root *)
                     then (Notepad.init (map object2newobject objs))
                     else (refocus_notepad (s,NONE));
                  caOpen   := NONE;
                  appl.area_init()) 
             else raise GenGUI "TGenGUI init: objects must be distinct"
         end
       | init ((s,SOME _), objs) = 
         raise GenGUI "TGenGUI init: path must point to folder"
      

     (* val state : unit-> gui_state *)

     (* This is the initial state which only has those objects as given
      * by the application's init() function (see above). 
      *)
     fun state () =
         let val _  = sync_notepad_with_guistate true
         in  (!gui_state) end

     (* introduce (not "create" really) a new object into the
      * manipulation area
      *     val intro : new_object -> unit *)
     fun intro obj = Notepad.intro (object2newobject obj)
         (* NOT YET: probably sync with state and naviboard necessary !!!*)

     (* val initial_state : unit-> gui_state *)
     fun initial_state () = (([],NONE), map Content (appl.init()))
     (* builds a (flat) gui_state just from appl.init *)
     (* Quite old-fashioned, I guess. *)


     (* Resynchronize all icons, e.g. if objects have changed their mode.
      * (Unfortunately, we cinnae change icons of single objects, since
      *  we can't identify objects...)
      *
      * val redisplay_icons : (object-> bool)-> unit *)

     val redisplay_icons = Notepad.redisplay_icons


     end
end




