(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/tests+examples/tree_list_ex.sml,v $
 
   Test of hierarchical Listbox. 

   $Date: 2001/03/30 13:40:06 $
   $Revision: 3.0 $
   Author: bu (Last modification by $Author: 2cxl $)

   (C) 1999, Albert Ludwigs Universität Freiburg
 
  ************************************************************************** *)



structure LittleTreeList : 
   sig val go : unit -> string
   end  =
struct
   open SmlTk 

(* *********************************************************************** *)
(*                                                                         *)
(* Data Construction                                                       *)
(*                                                                         *)
(* *********************************************************************** *)

   structure M : OBJECT_CLASS =
                 struct
                     type name = string ref
                     type object = (int * name)
                     datatype objtype = INT
                     fun  ord ((x,_),(x',_)) = Int.compare(x,x');
                     fun  name_of(_,y) = y
                     fun  rename s (x,y) = (y:=s)
                     fun  reset_name (x,y)=(y:="stdname: "^(Int.toString x)) 
                     fun  string_of_name s t = !s
                     fun  obj_type _ = INT
                     fun  icon _    = (print"W\n";raise Empty)
                     type cb_objects = unit -> object list
                     fun  cb_objects_abs x = x
                     fun  cb_objects_rep x = x
                 end

   structure N : FOLDERINFO = 
                 struct 
                     type node_info = string ref
                     type subnode_info = unit
                     fun  ord_node (x,y) = String.compare(!x,!y) 
                     fun  string_of_name_node s _ = !s
                     fun  rename_node s t   = (t:=s)
                     fun  reset_name_node s = (s:="...")
                 end
 
   structure TreeObj = obj2tree_obj(structure N = N and M = M);

(* *********************************************************************** *)
(*                                                                         *)
(* Provoding additional TreeList-Behaviour                                 *)
(* (Renaming dialogues etc.)  and intantiating TreeList                    *)
(*                                                                         *)
(* *********************************************************************** *)


   structure TreeListActions : TL_ACTION =
                 struct
                     type object        = TreeObj.object
                     type node_info    = TreeObj.node_info
                     type subnode_info = TreeObj.subnode_info
                     type path         = TreeObj.path

                     fun  content_label_action {path, was, cc} = 
                          UW.enterLine{title="enter label:",
                                       prompt="",default=was,
                                       width=15, cc=cc}
                     fun toStr x = TreeObj.string_of_name 
                                       (TreeObj.path2name x)
                                            {mode = Print.Long,
                                             printdepth=100,
                                             height=NONE,
                                             width=NONE}

                     fun  objtree_change_notifier {changed_at:path} = 
                            (print ( "general change notifier at :"^
                                     (toStr changed_at) ^ "\n"))

                     fun  focus_change_notifier {changed_at:path list} = 
                            (print ( "notifier activated at :" ^ 
                                     (String.concat 
                                     (map toStr changed_at)) ^
                                     "\n"))
                        
                     fun  open_close_notifier {is_open:bool, 
                                               changed_at:path list} = 
                           (print ( "open/close notifier activated at :" ^ 
                                     (String.concat 
                                     (map toStr changed_at)) ^
                                     "\n"))


                     fun  error_action          s = (UW.error "ERROR" )
                 end

   structure CB = Clipboard(type obj = unit -> TreeObj.object list)

   structure TreeList = 
                 TreeList(structure S = struct
                                          structure M  = TreeObj;
                                          structure A  = TreeListActions;
                                          type objlist = unit -> 
                                                         TreeObj.object list;
                                          structure CB = CB;
                                        end);

(* *********************************************************************** *)
(*                                                                         *)
(* Wrapping TreeList in a window                                           *)
(*                                                                         *)
(* *********************************************************************** *)

   fun quitButton win= Button{widId=newWidgetId(),
			      packings=[Side Top, Fill X, Expand true],
			      configs=[Text "Quit", 
                                       Command (fn ()=> closeWindow win)],
			      bindings=[]}
      
  

  fun testwin test = 
      let val winid = newWinId()
      in  mkWindow{winId  =winid, 
	           config =[WinTitle "Little Folder Tree",
	                     WinAspect(4,3,4,3),
	                     WinMinSize(200,200),
	                     WinMaxSize(500,400)], 
	           widgets=Pack [TreeList.create_canvas test,
                                 quitButton winid], 
		   bindings = [],
                   init=(fn ()=> ())}
      end
        

(* *********************************************************************** *)
(*                                                                         *)
(* Building a hierarchical object "test"                                   *)
(*                                                                         *)
(* *********************************************************************** *)

   local open TreeObj in
         
   (* local name generation management *)
         val ctr = ref(0)
         fun mk_obj s = ((!ctr, ref(s)),())
         fun mk_fol s = ref(s)
         fun emb s = ((),s)

   val test = [Content(mk_obj"blub"),
                       Folder(mk_fol"bla",
                             [Content(mk_obj"fgh"),
                              Folder(mk_fol"g8tgku",[]),
                              Folder(mk_fol"rtfu",[])
                             ]),
                      Folder(mk_fol"blerg",[])
              ]
   end;

(* *********************************************************************** *)
(*                                                                         *)
(* Run the sucker !                                                        *)
(*                                                                         *)
(* *********************************************************************** *)

  fun go () = startTclExn [ testwin test ];

end;
