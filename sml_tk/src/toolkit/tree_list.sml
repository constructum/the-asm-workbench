(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/tree_list.sml,v $
 
   A hierarchical Listbox. Enables browsing and selection
   in a tree-like data structure.

   $Date: 2001/03/30 13:39:53 $
   $Revision: 3.0 $
   Author: bu (Last modification by $Author: 2cxl $)

   (C) 1999, Albert Ludwigs Universität Freiburg
 
  ************************************************************************** *)
 


signature TL_ACTION =
   sig 
       type   object       (* SML-necessity since no HO-functors *)
       eqtype node_info    (* SML-necessity since no HO-functors *)
       type   subnode_info (* SML-necessity since no HO-functors *)
       type   path         (* SML-necessity since no HO-functors *)

       val content_label_action : 
           {path:path, was: string, cc: string -> unit} -> unit
           (* fired whenever a content label is activated. 
            * Should be a modal action. *)

       val focus_change_notifier : 
           {changed_at:path list} -> unit
           (* fired whenever a folder label or a folder icon is modified; 
            * should be used if TreeList is nonmodally coupled over  
            * gui_state with a notepad. *)

       val objtree_change_notifier :
           {changed_at: path} -> unit
           (* fired whenever the tree-structure (gui_state) has been modified -
            * e.g. as a consequence of an internal drag-drop.
            * Used for rehresh`s of other views. *)

       val open_close_notifier : 
           {is_open:bool,changed_at:path list} -> unit
           (* fired whenever a folder label or a folder icon is opened; 
            * can be used if internal tree is incrementally . *)

       val error_action          : string -> unit
           (* fired whenever illegal drag-drop-operations are attempted. 
            * Should be a modal action. *)
   end


signature JOIN = (* only there fore stupid SML-reasons *)
   sig
      structure  M  : PTREE_OBJECT_CLASS
      structure  A  : TL_ACTION;
      structure  CB : CLIPBOARD;
      sharing    type A.object       = M.object; 
      sharing    type CB.obj         = M.cb_objects; 
      sharing    type A.node_info    = M.node_info;
      sharing    type A.subnode_info = M.subnode_info;
      sharing    type A.path         = M.path
   end

functor TreeList (structure  S : JOIN) :
   sig
       datatype scale   = micro | mini | demo
       type config      = {hight     : int ref,             (* default 300 *)
                           width     : int ref,             (* default 400 *)
                           scrolltype: SmlTk.ScrollType ref,(* default NoneScb*)
                           no_icons  : bool ref,            (* no icons used;
                                                             * default false *)
                           std_icons : bool ref,            (* use icons speci-
                                                             * fied in M or use 
                                                             * std-icons; 
                                                             * default true *)
                           scale_factor: scale ref          (* scales display,
                                                             * default micro *)
                          }
       val Config       : config

       val create_canvas: S.M.object list -> SmlTk.Widget

       val upd_guistate : S.M.path -> S.M.object list -> unit
       val get_guistate : unit -> S.M.object list
       val refresh      : S.M.path -> unit
       val refresh_label: unit -> unit

       val get_selected : unit -> S.M.object list
       val set_selected : S.M.path list -> unit
   end =



struct 

  open S SmlTk GlobalConfig;

(* *********************************************************************** *)
(*                                                                         *)
(* Configuration ...                                                       *)
(*                                                                         *)
(* *********************************************************************** *)

                                     
  datatype scale = micro | mini | demo       (* according to icons/filer/* *)
 
  type config   = {hight        : int ref,             (* default 300 *)
                   width        : int ref,             (* default 400 *)
                   scrolltype   : SmlTk.ScrollType ref,(* default NoneScb *)
                   no_icons     : bool ref,            (* no icons used;
                                                          default false *)
                   std_icons    : bool ref,            (* default true *)
                   scale_factor : scale ref            (* scales display,
                                                          default 1 *)  
                  }

  val  Config   = {hight        = ref (300),
                   width        = ref (400),
                   scrolltype   = ref (SmlTk.RightScb),
                   no_icons     = ref false,
                   std_icons    = ref true,
                   scale_factor = ref micro
                  }

  fun debugmsg msg = Debug.print 11 ("TreeList: "^msg)
  val default_printmode = {mode = Print.Long,
                           printdepth=100,
                           height=NONE,
                           width=NONE}  (* the value is temporary *)

  fun name2string x = M.string_of_name (M.path2name x) default_printmode


  fun scale_toString micro = "micro"
    | scale_toString mini  = "mini"
    | scale_toString demo  = "demo"


(* Some Display Parameters *)

(* ****************************************************************

    <  in2  >    -
            *    ^
            *    | <hi
    <in1 >****** -   -
          *    *     ^
          * ** ****  |
          *    *     | <box_height
          ******     _
    <in3          >
          <    >box_width
   **************************************************************** *)
    

  val box_height   = 9
  val box_width    = 8
  val box_w_middle = 4
  val box_h_middle = 5
  val icon_width   = 9 (* in realitaet 12 !!! *)
  val in1          = 4
  val in2          = 9
  val in3          = 12
  val hi           = 9

  (* the crosshair for dragging an item *)
  val dragCursor = Cursor(XCursor(mkCursorName("fleur"), NONE))

  fun height n = mkCoord(0,n * (hi+box_height))

(* *********************************************************************** *)
(*                                                                         *)
(* The internal object-tree                                                *)
(*                                                                         *)
(* *********************************************************************** *)

(* The internal object tree contains not only the pure data-structure
   with labels, icons and object items, but also a decent abstraction of
   the state of the canvas, i.e. which folders are displayed
   open or closed ("is_open"), which ones are selected ("is_selct"), etc.

   For efficiency reasons, even more information is stored:
   - namely hooks to redisplay functions for local labels
   - and the CItems used in order to move substrees efficiently.
     (not yet implemented)

   However, there no real good reason for the fact, that I dicided to
   implement obj_tree in its own in this structure here instead of providing
   a new instance of obj2tree_obj-fun. Better patternmatch, and efficiency,
   maybe. But the price is code duplicity for critical functions like update.
*)

  type leaf_type =   {lab       : M.Basic.object * M.subnode_info,
                      path      : M.path,
                      icon      : IconKind option, 
                      cids      : CItemId * CItemId * CItemId * CItemId,
                      is_selct  : bool ref,
                      rd_hook   : (SimpleAction option) ref
                     }

  type 'obj_tree folder_type = 
                     {lab       : M.node_info,
                      path      : M.path,
                      subtrees  : 'obj_tree list, 
                      icon      : IconKind option, 
                      cids      : CItemId * CItemId * CItemId *
                                  CItemId * CItemId * CItemId * CItemId,
                      is_open   : bool ref,
                      is_selct  : bool ref,
                      rd_hook   : (SimpleAction option) ref}

  datatype obj_tree = leaf of   leaf_type
                    | folder of obj_tree folder_type

  fun get_folder (folder x) = x
  fun get_leaf   (leaf x)   = x

  fun fst (x,y) = x
  fun snd (x,y) = y

  fun convertFT ({lab,path, icon,cids,is_selct,rd_hook,...} 
                : 'obj_tree folder_type) = 
      let val (x1,x2,x3,_,_,_,_) = cids in
              {lab=lab, path=path,icon = icon, 
               cids=(x1,x2,x3),
               is_selct = is_selct,
               rd_hook  = rd_hook}
      end


  fun length []             = 0
     |length ((leaf _)::R)  = 1 + length R
     |length ((folder{is_open, subtrees,...})::R) = 
             1 + (if !is_open then length subtrees else 0) + (length R) 

  fun relabel (path,_) obs = 
      let fun rel p [] = []
          |   rel p (a as (leaf{lab,path,icon,cids,
                     is_selct,rd_hook}) :: R) = 
                     leaf{lab=lab, path=M.path_abs(List.rev p,snd(M.path_rep path)), 
                          icon = icon,cids = cids, is_selct=is_selct,
                          rd_hook = rd_hook}
                     :: (rel p R)
          |   rel p (a as (aob as(folder{lab,path,subtrees,icon,cids,
                                         is_open,is_selct,rd_hook})) :: R) = 
                     folder{lab      = lab, path = M.path_abs(List.rev p,NONE),
                            subtrees = rel (lab::p) subtrees,
                            icon     = icon, cids = cids,
                            is_open  = is_open,is_selct = is_selct,
                            rd_hook  = ref NONE} 
                     :: (rel p R) 
      in  rel (List.rev path) obs end

  exception WrongUpdate;

  fun get_subtrees (folder{subtrees,...}) = subtrees
     |get_subtrees _ = raise WrongUpdate

  fun update clean path ob [] = []
     |update clean path ob x =
      let   fun upd path [] = []
                (* search for leaf on leaf >>> *)
           |    upd ([],SOME x) ((aob as (leaf{lab,...}))::R) = 
                    (case M.Basic.ord(x,fst lab) of
                      EQUAL => (clean aob; ob::R)
                    | _     => aob :: (upd ([],SOME x) R))
                (* search for leaf on fold >>> *)
           |    upd  ([],SOME x) (A::R) = A::upd ([],SOME x) R 
                (* replace folder by folder content >>> *)
           |    upd  ([x],NONE)((aob as(folder{lab,path,subtrees,icon,cids,
                                               is_open,is_selct,rd_hook}))::R)= 
                    (case M.ord_node(x,lab) of
                      EQUAL => (app clean subtrees; 
                                folder{lab=lab,path=path,
                                       subtrees=get_subtrees ob(*!!!*),
                                       icon=icon,cids=cids,is_open=is_open,
                                       is_selct=is_selct,rd_hook=rd_hook}::R)
                    | _     => aob :: (upd ([x],NONE) R))
                (* search for folder on leaf >>> *)
           |    upd  ([x],NONE) (A::R) = A::upd ([x],NONE) R   
                (* descending in folder >>> *)
           |    upd  (x::R,H)((aob as(folder{lab,path,subtrees,icon,cids,
                                             is_open,is_selct,rd_hook}))::R') =
                    (case M.ord_node(x,lab) of
                      EQUAL => (folder{lab=lab,path=path,
                                subtrees=upd (R,H) subtrees,
                                icon=icon, cids=cids,is_open=is_open,
                                is_selct=is_selct,rd_hook=rd_hook} :: R')
                    | _     => aob :: (upd (x::R,H) R')) 
           |    upd (x::R,H) (A::R') = A::upd (x::R,H) R'

      in upd (M.path_rep path) x end;

  exception WrongInsert;

  fun insert path obs [] = raise WrongInsert
     |insert path obs x =
      let   fun ins path [] = []
                (* search for leaf on leaf >>> *)
           |    ins ([],SOME x) ((aob as (leaf{lab,...}))::R) =
                    (case M.Basic.ord(x,fst lab) of
                      EQUAL => (aob :: (relabel (M.path_rep path) obs)@R)
                    | _     =>  aob :: (ins ([],SOME x) R))
                (* search for leaf on fold >>> *)
           |    ins  ([],SOME x) (A::R) = A::ins ([],SOME x) R
                (* replace folder by folder content >>> *)
           |    ins  ([x],NONE)((aob as(folder{lab,path,subtrees,icon,cids,
                                               is_open,is_selct,rd_hook}))::R)=
                    (case M.ord_node(x,lab) of
                      EQUAL => (aob::(relabel (M.path_rep path) obs)@R)
                    | _     => aob :: (ins ([x],NONE) R))
                (* search for folder on leaf >>> *)
           |    ins  ([x],NONE) (A::R) = A::ins ([x],NONE) R
                (* descending in folder >>> *)
           |    ins  (x::R,H)((aob as(folder{lab,path,subtrees,icon,cids,
                                             is_open,is_selct,rd_hook}))::R') =
                    (case M.ord_node(x,lab) of
                      EQUAL => (folder{lab=lab,path=path,
                                subtrees=ins (R,H) subtrees,
                                icon=icon, cids=cids,is_open=is_open,
                                is_selct=is_selct,rd_hook=rd_hook} :: R')
                    | _     => aob :: (ins (x::R,H) R'))
           |    ins (x::R,H) (A::R') = A::ins (x::R,H) R'

      in ins (M.path_rep path) x end;

  fun is_open_at _ [] = false
     |is_open_at (a::R) ((leaf _)::R') = is_open_at (a::R) (R')
     |is_open_at [a] ((folder{lab,is_open,subtrees,...})::R') =
         if !is_open then   
            case M.ord_node(a,lab) of
              EQUAL => true
            | _     => false
         else is_open_at [a] (R')
     |is_open_at (a::R) ((folder{lab,is_open,subtrees,...})::R') =
         if !is_open then   
            case M.ord_node(a,lab) of
              EQUAL => is_open_at (R) (subtrees)
            | _     => false
         else is_open_at (a::R) (R') 


  fun cidsOf [] = []
     |cidsOf((leaf {cids=(A,B,C,D),...})::R)  = A::B::C::D::(cidsOf R)
     |cidsOf((folder{cids=(A,B,C,D,E',F,G),subtrees,...})::R)  = 
             A::B::C::D::E'::F::G::(cidsOf R)@(cidsOf subtrees)

(* *********************************************************************** *)
(*                                                                         *)
(* Conversion ...                                                          *)
(*                                                                         *)
(* *********************************************************************** *)

  fun gen_cids1 () = (newCItemId(),newCItemId(),newCItemId(),newCItemId())
  fun gen_cids2 () = (newCItemId(),newCItemId(),newCItemId(),
                      newCItemId(),newCItemId(),newCItemId(),newCItemId())


  val texticon_id  = newImageId()
  val foldericon_id= newImageId()


  fun text_icon  ()= FileImage((SmlTk.getLibPath())^
                                "/icons/treelist/"^ 
                                (scale_toString (!(#scale_factor Config))) ^
                                "/text.gif", 
                                texticon_id);
  fun folder_icon()= FileImage((SmlTk.getLibPath())^
                                "/icons/treelist/"^
                                (scale_toString (!(#scale_factor Config))) ^
                                "/folder.gif", 
                                foldericon_id);

  fun obj2obj_tree0 p obj = 
      if M.isFolder obj then 
            let val (h,s)       = M.getFolder obj
                val p           = h::p
            in  folder{lab      = h,
                       path     = M.path_abs(List.rev p,NONE),
                       subtrees = map (obj2obj_tree0 p) s,
                       icon     = if   !(#no_icons Config) then NONE
                                  else SOME(folder_icon ()), 
                       cids     = gen_cids2 (),
                       is_open  = ref false,is_selct = ref false, 
                       rd_hook  = ref NONE}
            end           
      else leaf{lab  = M.getContent obj, 
                path = M.path_abs(List.rev p,SOME(fst(M.getContent obj))),
                icon = if !(#no_icons Config) then NONE 
                       else if !(#std_icons Config) then SOME(text_icon ())
                       else SOME(Icons.selMicroline(M.icon(M.obj_type obj))), 
                cids = gen_cids1 (), is_selct=ref false,rd_hook = ref NONE}


  val obj2obj_tree = obj2obj_tree0 [];

  fun obj_tree2obj (folder{lab,subtrees,...}) = 
                   M.Folder(lab,map obj_tree2obj subtrees)
     |obj_tree2obj (leaf{lab,...}) = M.Content lab


(* *********************************************************************** *)
(*                                                                         *)
(* Display related tree-traversal ...                                      *)
(*                                                                         *)
(* *********************************************************************** *)

  fun clearSel [] = ()
     |clearSel((leaf {is_selct,rd_hook,...})::R)  = 
              (if !is_selct then (is_selct:=false; valOf(!rd_hook)())
               else ();
               clearSel R)
     |clearSel((folder{is_selct,rd_hook,subtrees,...})::R)  = 
              (if !is_selct then (is_selct:=false; valOf(!rd_hook)())
               else ();
               clearSel subtrees;
               clearSel R)

  fun deselect [] = false
     |deselect ((leaf {is_selct,...})::R)  = 
                (let val h = !is_selct
                     val r = deselect R
                 in  is_selct:=false; h orelse r end)
     |deselect ((folder{is_selct,subtrees,...})::R) = 
                (let val h  = !is_selct
                     val r  = deselect R
                     val r' = deselect subtrees
                 in  is_selct:=false; h orelse r orelse r' end)

  fun setSelRange A tree =
      let val mark_mode = ref(0) (* 0 = init, 1 = fill-to-mode,
                                    2 = fill-from-mode, 3 = delete-mode *)
          fun mlr [] = ()
             |mlr ((leaf {cids=(_,_,C,_),is_selct,rd_hook,...})::R)  =
                  (case !mark_mode of
                    0 =>(if A = C then (mark_mode:=1;
                                        is_selct:=true;
                                        valOf(!rd_hook)())
                         else if !is_selct then mark_mode:=2 else ())
                  | 1 =>(if A = C then (mark_mode:=3;
                                        is_selct:=true;
                                        valOf(!rd_hook)())
                         else if !is_selct then mark_mode:=3 
                              else (is_selct:=true;
                                    valOf(!rd_hook)()))
                  | 2 =>(if A = C then (mark_mode:=3;
                                        is_selct:=true;
                                        valOf(!rd_hook)())
                         else if !is_selct then () 
                              else (is_selct:=true; valOf(!rd_hook)()))
                  | 3 =>(if not (!is_selct) then () 
                         else (is_selct:=false; valOf(!rd_hook)()));
                   mlr R)
             |mlr((folder{cids=(_,_,_,_,_,C,_),is_open,is_selct,
                          rd_hook,subtrees,...})::R)  =
                  (case !mark_mode of
                    0 =>(if A = C then (mark_mode:=1;
                                        is_selct:=true;
                                        valOf(!rd_hook)())
                         else if !is_selct then mark_mode:=2 else ())
                  | 1 =>(if A = C then (mark_mode:=3;
                                        is_selct:=true;
                                        valOf(!rd_hook)())
                         else if !is_selct then mark_mode:=3 
                              else (is_selct:=true;
                                    valOf(!rd_hook)()))
                  | 2 =>(if A = C then (mark_mode:=3;
                                        is_selct:=true;
                                        valOf(!rd_hook)())
                         else if !is_selct then () 
                              else (is_selct:=true; valOf(!rd_hook)()))
                  | 3 =>(if not(!is_selct) then () 
                         else (is_selct:=false; valOf(!rd_hook)()));
                   if !is_open then mlr subtrees else ();
                   mlr R)
      in mlr tree end;


    

(* *********************************************************************** *)
(*                                                                         *)
(* State ...                                                               *)
(*                                                                         *)
(* *********************************************************************** *)

                                     

  val gui_state = ref ([]: obj_tree list)
  val globalDragDropBindings = ref([]:Binding list)
  val refresh_hook = ref(NONE: (S.M.path -> unit) option)
                                    
(*
  fun get_selected0 () =  
      let fun get_sel [] = []
             |get_sel((leaf {is_selct,lab,path,...})::R)  = 
                     (if !is_selct then [(path, M.Content lab)] 
                      else []) @ (get_sel R)
             |get_sel((folder{is_selct,subtrees,lab,path,...})::R)  = 
                     (if !is_selct 
                      then [(path,M.Folder(lab,map obj_tree2obj subtrees))] 
                      else get_sel subtrees) 
                     @ (get_sel R)
      in  get_sel(!gui_state) end;

 *)
  fun get_selected0 () =
      let fun get_sel [] = []
             |get_sel((a as leaf {is_selct,lab,path,...})::R)  =
                     (if !is_selct then [(path, a)]
                      else []) @ (get_sel R)
             |get_sel((a as folder{is_selct,subtrees,lab,path,...})::R)  =
                     (if !is_selct
                      then [(path,a)]
                      else get_sel subtrees)
                     @ (get_sel R)
      in  get_sel(!gui_state) end;


  fun get_selected() = map (obj_tree2obj o snd) (get_selected0())

  fun rem_selected [] = []
     |rem_selected ((a as (leaf {is_selct,lab,path,...}))::R) =
                   (if !is_selct then [] else [a]) @ (rem_selected R)
     |rem_selected ((a as (folder{lab,path,subtrees,icon,cids,
                          is_open,is_selct,rd_hook}))::R) = 
                     (if !is_selct
                      then []
                      else [folder{lab=lab,path=path,icon=icon,cids=cids, 
                                   subtrees = rem_selected subtrees,
                                   is_open=is_open,is_selct=is_selct,
                                   rd_hook = rd_hook}])
                     @ (rem_selected R)

  fun set_selected _  = ()   (* NOT YET IMPLEMETED *)


  fun get_guistate () =  map obj_tree2obj (!gui_state);
  
(* *********************************************************************** *)
(*                                                                         *)
(* drag-drop-control  ...                                                  *)
(*                                                                         *)
(* *********************************************************************** *)

fun debugmsg x = print x
fun debugmsg x = ()

  datatype dragmodetype = Internal (* from tl-canvas to tl-canvas *)
                        | External (* from tl-canvas to extern *)
                        | Import   (* from extern to tl-canvas *)
  val      dragmode     = ref(NONE:dragmodetype option) (* NONE : don't know *)

  fun pressGrabButton   path canId (Ev:TkEvent) = 
             (dragmode := NONE;
              debugmsg "drag:";
              debugmsg (name2string path);
               debugmsg "\n";
             addConf canId [dragCursor])

  fun releaseGrabButton path canId (TkEvent(_, _, x, y, _, _)) = 
             (debugmsg ("release:");
              debugmsg (name2string path);
              case !dragmode of 
                NONE           => debugmsg ": none \n"
              | SOME(Import)   => debugmsg ": import \n"
              | SOME(Internal) => debugmsg ": internal \n"
              | SOME(External) => debugmsg ": external \n";
              (* dragmode := NONE; *)
              addConf canId [Cursor(NoCursor)])

  fun grabbedMotion canId _ = 
             (dragmode := SOME(Internal);
              debugmsg "motion \n")

  fun leaveCanvas canId Ev = 
             (case !dragmode of 
                SOME(Internal) => dragmode := SOME(External)
              | _              => dragmode := NONE; 
              debugmsg "leave: \n";
              let val objs = get_selected()
                  fun remove objs = (print "   export objects \n")
              in  CB.put (M.cb_objects_abs(fn() => objs)) Ev 
                         (fn() => remove (objs))  end
             )  

  fun import_objects to_path objs Ev = 
      (print "   import objects \n";
       if CB.isEmpty Ev then ()
       else let val objs = (M.cb_objects_rep(CB.get Ev))()
                fun doit x = ()
            in  doit objs end)

  fun move_objects path intern canvasId =
      (print "   move objects \n";
       (* internal objects have been dragged into can Id.
          This resulted in storing the objects in the
          internal_release_buffer. *)
       let val objs    = map snd (get_selected0())
           val state'  = rem_selected (!gui_state)
           val state'' = insert path objs state'
       in (* first: delete everything from screen (including 
             stuff not in nustate) *)
          app (fn x => (delCItem canvasId x 
                     handle CITEM _ => ()))
              (cidsOf (!gui_state));
          gui_state:=state'';
          valOf(!refresh_hook) (M.path_abs([],NONE));
          (* <<< removes again, but does not hurt <<< *)
          A.objtree_change_notifier {changed_at=M.path_abs([],NONE)}
          (* HACK! more precise: least common prefix of all paths . . . *)
       end)


  (* enterCanvas atpath called_internally inCanvasId ... *)
  fun enterCanvas path true canId Ev = 
             (debugmsg ("enter:1:");
              debugmsg (name2string path);
              case !dragmode of 
                SOME(External) => (print (":ext \n"))
              | SOME(Import)   => (print (":imp \n");
                                   import_objects path true Ev)
              | SOME(Internal) => (print (":int \n");
                                   move_objects path true canId)
              | _              => print (":no \n");
             dragmode := NONE)                
     |enterCanvas path false canId Ev = 
             (debugmsg ("enter:2:");
              debugmsg (name2string path);
              case !dragmode of 
                SOME(External) => print (":ext \n")
              | SOME(Internal) => (print (":int \n");
                                   move_objects path false canId)
              | SOME(Import)   => (print (":imp \n");
                                   import_objects path false Ev)
              | _              => print (":no \n");
              dragmode := NONE)



  fun pressSelButton canId _ = ()


(* *********************************************************************** *)
(*                                                                         *)
(* object-tree - drawing ...                                               *)
(*                                                                         *)
(* *********************************************************************** *)

  val canvasId = newWidgetId()

  fun Cline (cid, c, cl, bl) = SmlTk.CLine{citemId=cid, coords=c, 
                                           configs=cl, bindings=bl};

  (* mk_label : generates a editable label for folders and basicobjects.
     Lots of functionality for selection is provided - 
     requiring additional information of the global tree, the surrounding
     canvas-widget, the CItemId for delete-management. *)    
  fun mk_label is_selected rd_hook (gttxt,updtxt) pos A wid path = 
      (* in order not to redraw the whole tree in case of a selection,
         local redraw functions are provided here and stored in the 
         obj_tree via the hooks. Thus, selection with global effects
         can be implemented via evaluating the local redraw functions
         on demand. This complicates the story a bit.
       *)
      let val labelId  = newWidgetId()

          fun col_lab b= if b 
                         then Background (!(#background_sel Colors.Config))
                         else Background (!(#background Colors.Config))

          fun relief_lab b = if b 
                             then Relief Sunken 
                             else Relief Flat
 
          fun redraw _ = (addConf labelId[col_lab(!is_selected),
                                          relief_lab(!is_selected),
                                          Text(gttxt())])

          fun hilite b _ = if !is_selected then ()
                           else (addConf labelId[col_lab(b)])

          fun sel_action _ = (clearSel (!gui_state);
                              is_selected:=true; 
                              valOf(!rd_hook)());

          fun sel_range_action _ = (setSelRange A (!gui_state));

          fun sel_group_elem_action _ = (is_selected:= not (!is_selected);
                                         valOf(!rd_hook)())

          fun activate _ = (updtxt(valOf(!rd_hook)); redraw ())
 
          fun lab b = Label{widId=labelId,
			    packings=[],
                            bindings=[BindEv(Events.sel_elem_event(),
                                             fn XX => (sel_action XX;
                                                       (* drag-code >>> *)
                                                       pressGrabButton 
                                                          path wid XX)),
                            (*CONFLICT with:
                                      BindEv(Events.drag_event(), 
                                             pressGrabButton path wid), *)                                 (* Problem : This conflict-resolution above
                               assumes a particular configuration
                               of sel_elem_event() and drag_event() *)
	  BindEv(Events.drop_event(), releaseGrabButton path wid),
	  BindEv(Events.dd_motion_event(),grabbedMotion wid),                               
                                      BindEv(Events.sel_range_event(),
                                             sel_range_action),
                                      BindEv(Events.sel_group_elem_event(),
                                             sel_group_elem_action),
                                      BindEv(Events.activate_event(),
                                             activate),

                                      BindEv(Enter, fn XX=>(hilite true XX;
                                                            enterCanvas path
                                                              false wid XX)),
                                      BindEv(Leave, hilite false)
                                     ],
			    configs=[Text (gttxt ()),
                                     col_lab b,relief_lab b,
                                     Font (SmlTk.SansSerif [SmlTk.Small])]

			   }
          val m = mkCoord(icon_width + 4,0)
          fun mkClab b = CWidget{citemId=A, coord=addCoord pos m, 
                                 widgets=Pack [lab b],
                                 configs=[Anchor West], 
                                 bindings=[]}
      in  rd_hook:=SOME (redraw);
          mkClab (!is_selected)
      end
      
  (* mini-box: clickable symbol for folders; activation may result in opening
     the folder by displaying the subtree. The symbol is drawn - not a gif.
       b: open/close status; pos: top-left start position of the drawing,
       A,B,C,D : CItemId's stored here for systematic release,
       cmd: command for activation, path: info for activation and debugging *)
  fun mini_box b pos (A,B,C,D) cmd path =
      let fun cm _ = (A.open_close_notifier {is_open=(!b),changed_at=[path]};
                      (* caution ! this may change the gui_state ! *)
                      cmd())
          val bi = BindEv(Events.activate_event(),cm) in
         [CRectangle{citemId=A, coord1=pos, 
                     coord2=addCoord pos (mkCoord(box_width,box_height)),
                     configs=[FillColor White, Outline Black], bindings=[bi]},
          Cline(B,[addCoord pos (mkCoord(2,box_h_middle)),
                   addCoord pos (mkCoord(box_width-1,box_h_middle))],[],[bi]),
          Cline(C,[addCoord pos (mkCoord(box_width,box_h_middle)),
                   addCoord pos (mkCoord(in3,box_h_middle))],[],[bi])] @
         (if !b then []
          else [Cline(D,[addCoord pos (mkCoord(box_w_middle,2)),
                         addCoord pos (mkCoord(box_w_middle,box_width))],
                      [],[bi])])
      end


  (* icon_piece: clickable symbol for folders and basicobjects; 
     activation may result in firing the activation continuation. 
     The symbol is a gif - either user-defined (i available), or standard.
       p: top-left start position of the drawing,
       path: info for activation and debugging *)
  fun icon_piece (SOME i) CitemId p path hi wid =
      let fun activate _ = A.focus_change_notifier{changed_at = [path]} 
      in 
          [CIcon {citemId  = CitemId,
                  coord    = addCoord p (addCoord(in3+1,box_h_middle) hi),
                  iconkind = i,
                  configs  = [Anchor West],
                  bindings = [(* BindEv(Events.sel_elem_event(),sel_action),
                              BindEv(Events.sel_range_event(),sel_range_action),
                              BindEv(Events.sel_group_elem_event(),
                                            sel_group_elem_action), *)
                              BindEv(Events.drag_event(), 
                                          pressGrabButton path wid),
	                      BindEv(Events.drop_event(), 
                                          releaseGrabButton path wid),
                              BindEv(Events.activate_event(), activate),
	                      BindEv(Enter, enterCanvas path false wid) 
                             ]
                }]
       end
     | icon_piece (NONE) CitemId p path hi wid = []


  (* folder_line: line in tree consisting of box, icon, and label (for folders).
     All information from the context tree must be passed to the drawing 
     functions of these subitems. 
     The offset off produces a shift of the line level and a suitably prologued 
     front vertex. *)    
  fun folder_line is_open is_slct rdh icon p path 
                  off lab (A,B,C,D,E',F,G) cmd wid = 
      let val p'  = addCoord p (mkCoord(in1,0))
          val hi  = addCoord (mkCoord(0,hi)) (height off)
          val li  = Cline(A, [p',addCoord p' hi],[],[])
          val p'' = addCoord p hi
          val p'''= addCoord p (addCoord(mkCoord(in3,box_h_middle)) hi)
      in  li :: mini_box is_open p'' (B,C,D,E') cmd path @ 
          icon_piece icon G p path hi wid @ 
          [mk_label is_slct rdh lab p''' F wid path] 
      end 

  (* object_line: line in tree consisting of front vertrex, icon, and label 
     (for basicobjects).
     All information from the context tree must be passed to the drawing 
     functions of these subitems.  
     The offset off produces a shift of the line level and a suitably 
     prolongued front vertex.*)    
  fun object_line is_open is_slct rdh icon p path off lab (A,B,C,D) wid = 
      let val p'    = addCoord p (mkCoord(in1,0))
          val hi    = addCoord (mkCoord(0,hi)) (height off)
          val hi_tot= addCoord hi (mkCoord(0,box_height))
          val p''   = addCoord (addCoord p' hi) 
                               (mkCoord(0,box_h_middle))
          val p'''  = if is_open then addCoord p' hi_tot else p''
          val p'''' = addCoord p (addCoord (mkCoord(in3,box_h_middle)) hi)
      in  [Cline(A, [p',p'''],[],[]),
           Cline(B, [p'',addCoord p'' (mkCoord(in3-in1,0))],[],[])] @
           icon_piece icon D p path hi wid @
          [mk_label is_slct rdh lab p'''' C wid path]
      end;
      
  fun diag1 maxcl p lab C =
      let val s = M.Basic.string_of_name (M.Basic.name_of lab) maxcl
          fun upd1 s lab C= (fn s' => if s=s' then () 
                                      else (M.Basic.rename s' lab; C())) 
      in  A.content_label_action{path=p, was=s, cc=upd1 s lab C} end

  fun diag2 maxcl p lab C = 
      let val s = M.string_of_name_node lab maxcl
          fun upd2 s lab C= (fn s' => if s=s' then () 
                                      else (M.rename_node s' lab; C())) 
      in  A.content_label_action {path=p, was=s, cc=upd2 s lab C} end

  (* placing a tree into a canvas - with all jingles . . . *)
  fun place_tree pos wid tree = 
      let val cl         = {mode=Print.Short,printdepth=1, 
                            height=NONE,width=NONE}          (* HACK ! *)
          fun str1  lab  = (fn () => M.Basic.string_of_name 
                                        (M.Basic.name_of lab) cl)
          fun str2  lab  = (fn () => M.string_of_name_node lab cl)
          fun shift n p  = addCoord p (height n) 
          fun indent  p  = addCoord p (mkCoord (in3 + (icon_width div 2) - 1,0))
          fun beh1 p lab = (str1 lab, diag1 cl p lab)
          fun beh2 p lab = (str2 lab, diag2 cl p lab)
          fun openCont p lt is_open_ref _ = 
                     (is_open_ref := not(!is_open_ref);
                      (#is_selct (get_folder lt)) := deselect [lt];
                      refresh (M.path_rep p))
          (* the core of the display algorithm: *)
          fun pt p off ([]) = []
             |pt p off ((leaf{lab,icon,cids,is_selct,rd_hook,path,...})::R)  = 
                      (object_line (not(null R)) is_selct rd_hook icon p path
                                    off (beh1 path (fst lab)) cids wid) @ 
                      (pt (shift (1+off) p) 0 R) 
             |pt p off ((lt as folder{icon,lab,cids,is_selct,
                                      rd_hook,is_open,subtrees,path,...})::R)= 
                     if !is_open then 
                          (folder_line is_open is_selct rd_hook icon p path
                                       off (beh2 path lab) 
                                       cids (openCont path lt is_open) wid) @
                       	  (pt (indent (shift 1 p)) (0) subtrees) @
                          (pt (shift (1+off) p) (length subtrees) R)
                     else (folder_line is_open is_selct rd_hook icon p path
                                       off (beh2 path lab) 
                                       cids (openCont path lt is_open) wid) @
                          (pt (shift (1+off) p) 0 R)

     in  debugmsg "place_tree ... "; pt pos 0 tree end
       
  and refresh ([],_)      = 
      (app (fn x => (delCItem canvasId x
                     handle CITEM _ => ())) 
           (cidsOf (!gui_state));
       (* gui_state := map (obj2obj_tree o obj_tree2obj) (!gui_state); *)
       app (addCItem canvasId) 
           (place_tree (mkCoord(10,15)) canvasId (!gui_state))
      )
    |refresh (p,NONE) =  refresh ([],NONE);  
       (* correct, but inefficient. This refresh is used for internal
          use - i.e. redisplay for open-close-actions. *)

   fun refreshO ([],X') = refresh ([],X')
      |refreshO (p,NONE) = if is_open_at p (!gui_state)
                                            (* very simple heuristic 
                                               to keep it smooth *)
                         then (print "refresh full\n"; refresh ([],NONE)) 
                         else (print "refresh optimized\n");

   val refresh = (fn p => refresh  (M.path_rep p))
   val refreshO= (fn p => refreshO (M.path_rep p))


   fun refresh_label () = 
       let fun rl [] = ()
           |   rl ((leaf {rd_hook,...})::R) = (valOf(!rd_hook)(); rl R)
           |   rl ((folder{rd_hook,is_open,subtrees,...})::R) =
                  (valOf(!rd_hook)();
                   if !is_open then rl subtrees else ();
                   rl R);
       in rl (!gui_state) end;
  

      
   fun redisplay () = 
      (app (fn x => (delCItem canvasId x 
                     handle CITEM _ => ())) 
           (cidsOf (!gui_state)); 
       (* <<< better: scratch everything from complete canvas ...*)
       gui_state := map (obj2obj_tree o obj_tree2obj) (!gui_state);
       app (addCItem canvasId) 
           (place_tree (mkCoord(10,15)) canvasId (!gui_state))
      )

(* *********************************************************************** *)
(*                                                                         *)
(* update access to gui_state ...                                          *)
(*                                                                         *)
(* *********************************************************************** *)

  (* merge maintains the internal data-structure, as long as there
     are no differences to the analogous external structure.
     Maintaining means open/Close, cids, etc. Path is patched. *)
  fun merge p R [] = R
     |merge p [] R = map (obj2obj_tree0 p) R
     |merge p ((aob as (leaf{lab,...}))::R) (a::R') = 
            if M.isFolder a then ((obj2obj_tree0 p a) :: (merge p R R'))
            else (case M.Basic.ord(fst(M.getContent a),fst lab) of
                    EQUAL => aob :: (merge p R R')
                  | _ => (obj2obj_tree0 p a) :: (merge p R R'))
     |merge p ((aob as(folder{lab,path,subtrees,icon,cids,
                             is_open,is_selct,rd_hook}))::R) (a::R') =
            if M.isFolder a then 
                 let val (n,R'') = M.getFolder a
                 in  (case M.ord_node(n,lab) of
                        EQUAL => ((folder{lab=lab,path=M.path_abs(List.rev p,NONE),
                                       subtrees = merge (n::p) subtrees R'',
                                       icon=icon,cids=cids,is_open=is_open,
                                       is_selct=is_selct,rd_hook=rd_hook}) 
                                 :: (merge p R R'))
                      | _ => (obj2obj_tree0 p a) :: (merge p R R'))
                 end
            else (obj2obj_tree0 p a) :: (merge p R R')
 





  fun upd_guistate (p as ([],NONE)) obs =
                   (app (fn x => (delCItem canvasId x
                         handle CITEM _ => ())) 
                        (cidsOf (!gui_state));
                    gui_state:= map obj2obj_tree obs)  
     |upd_guistate (p as (m,_)) [ob] =
                    let fun clean dob = app(fn x => (delCItem canvasId x
                                                     handle CITEM _ => ())) 
                                           (cidsOf [dob]) 
                    in  gui_state:=update clean (M.path_abs p) 
                                           (obj2obj_tree0 (tl(rev m)) ob) 
                                           (!gui_state)
                    end

  val upd_guistate = fn p => fn obs =>  upd_guistate (M.path_rep p) obs  


  fun canvasBindings canId =  
	[ BindEv(Events.drag_event(), fn _ => (clearSel (!gui_state);
                                               dragmode:=NONE;
                                               refresh_label ())),
	  BindEv(Events.drop_event(), releaseGrabButton (M.path_abs([],NONE)) canId),
	  BindEv(Events.dd_motion_event(),grabbedMotion canId),
	  BindEv(Events.dd_leave_event(), leaveCanvas canId),
	  (* BindEv(Events.dd_enter_event(),enterCanvas([],NONE)true canId), *)
          (* <<< seems to have no effect . . . *)
	  (* BindEv(Enter, enterCanvas ([],NONE) false canId) *)
	  BindEv(Leave, leaveCanvas canId) 
	];

(* Events.drag_event()      = ButtonPress   (SOME 1) 
   Events.drop_event()      = ButtonRelease (SOME 1) 
   Events.dd_motion_event() = ModButton     (1,Motion) 
   Events.dd_leave_event()  = ModButton     (1,Leave)
   Events.dd_enter_event()  = ModButton     (1,Enter)   
 *)

  fun create_canvas obj =
      let val () = (gui_state := map obj2obj_tree obj;
                    globalDragDropBindings := canvasBindings canvasId;
                    refresh_hook:=SOME(refresh))
      in  Canvas{widId   = canvasId, scrolltype = !(#scrolltype Config),
	         citems  = (place_tree(mkCoord(10,15))canvasId(!gui_state)),
	 	 packings= [Side Top, Fill X, Expand true],
		 configs = [Height (!(#hight Config)), 
                            Width (!(#width Config)), 
                            Relief Groove, 
                            Background (!(#background Colors.Config))],
		 bindings= !globalDragDropBindings}
      end;
                
val refresh = refreshO (* This optimized refresh is exported ... *)


end

