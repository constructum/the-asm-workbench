(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/obj2tree_obj-fun.sml,v $
 
   Unified Object Interface
 
   $Date: 2001/03/30 13:39:48 $
   $Revision: 3.0 $
   Author: bu (Last modification by $Author: 2cxl $)

   (C) 1999, Albert-Ludwigs-Uni Freiburg
 
  ************************************************************************** *)

(* This functor produces out of an OBJECT_CLASS-structure 
   a (standard) TREE_OBJECT_CLASS-structure, i.e. in particular
   also another OBJECT_CLASS-structure. It provides
   standard notions for paths and path-operations on the
   generated TREE_OBJECT's. 

   These operations require that the application will assure
   uniqueness of object-names and node-info's.
   There are two fundamentally different way's to assure this:
   - update and rename must assure this via postconditions
   - internally in node-infos and names, unique key's are
     generated.
*)

functor obj2tree_obj (structure N:FOLDERINFO and 
                                M:OBJECT_CLASS) : PTREE_OBJECT_CLASS
(* where nodeinfo = N.nodeinfo and basic_object = M.object and 
   mlabel = M.label*)
(* We have: obj_to_treeobj(A,B) : OBJECT_CLASS  *)
=
struct

      open Print;

      structure Basic = M
   (*   type mname = M.name *)
      type node_info = N.node_info
      type subnode_info = N.subnode_info

      datatype object = Content of Basic.object * subnode_info
                       |Folder  of N.node_info * (object)list

      type cb_objects = unit -> object list
      fun  cb_objects_abs x = x
      fun  cb_objects_rep x = x

      fun isFolder   (Folder x) = true
         |isFolder   _          = false

      fun getContent (Content x) = x
      fun getFolder  (Folder x) = x

      type path = N.node_info list * M.object option
      fun  path_rep x = x
      fun  path_abs x = x
      type name = path

      fun  name2path x = x 
      fun  path2name x = x  

      fun  path_of (P,_) = P

      fun  base_of (_,B) = valOf B

      datatype  objtype = LeafT of M.objtype 
                         |FolderT

      fun fst (x,_) = x
      fun snd (_,y) = y

      fun lex_ord ord ([],[]) = EQUAL
         |lex_ord ord ([],S)  = LESS
         |lex_ord ord (S,[])  = GREATER
         |lex_ord ord (a::S,a'::S')  = 
            (case ord(a,a') of
               LESS => LESS
            | GREATER => GREATER
            | EQUAL => lex_ord ord (S,S'));
         
      fun ord (Content a,Content b)  = M.ord (fst a,fst b)
         |ord (Content a,Folder b)   = LESS
         |ord (Folder b,Content a)   = GREATER
         |ord (Folder (a,S),Folder (b,S')) = 
           case N.ord_node (a,b) of
              LESS => LESS
            | GREATER => GREATER
            | EQUAL => lex_ord ord (S,S');
   
      fun name_of (Content (a,_)) = ([], SOME(a))
         |name_of (Folder (a,S)) = ([a],NONE)



      fun rename s (Content a) = M.rename s (fst a)
         |rename s (Folder (a,S)) = N.rename_node s a

      fun reset_name (Content a) = M.reset_name (fst a)
         |reset_name (Folder (a,S)) = N.reset_name_node a

      val separator = "/"

      (* treatment of printdepth and margin incorrect !!! *)
      fun mgs m = (fn (a,b) => separator ^ (N.string_of_name_node a m) ^ b)

      fun string_of_path p (m as {mode,printdepth,height,width}:Print.format)= 
          let val m' = {mode=mode, printdepth=0, (* print from scratch *)
                        height= SOME 1, (* no internal breaking *)
                        width = NONE:int option   (* unbounded *) }

              fun string_of ([], SOME(l)) = M.string_of_name (M.name_of l) m'
                 |string_of (R, SOME(l))  = (foldr (mgs m') "" R) ^ separator ^
                                            (M.string_of_name (M.name_of l) m')
                 |string_of ([], NONE)    = "" (* ??? *)
                 |string_of ([a], NONE)   = (N.string_of_name_node a m')
                                            ^(case mode of 
                                                Short => ""
                                              | _     => "")
                 |string_of (a::R, NONE)  = (foldr (mgs m') 
                                               (N.string_of_name_node a m')(R))
                                            ^ (case mode of 
                                                Short => ""
                                              | _     => "")
             
              fun blk len txt   = if size(txt)>len 
                                  then substring(txt,0,len) :: 
                                       blk len (substring(txt,len,
                                                 size(txt)-len))
                                  else [txt]
              fun block len txt = let val tt = blk len txt 
                                  in  foldl(fn(a,b) => b ^"\n"^a)
                                           (hd tt)(tl tt) 
                                  end

              fun ht len txt = ((size txt) div len) + 1

          in  case (height,width) of
                (NONE,NONE)    => string_of p    (* printing unbounded *)
              | (SOME k,NONE)  => string_of p    (* unrealistic case   *)
              | (NONE,SOME k)  => block k (string_of p)
                                               (* printing unbounded height *)
              | (SOME l,SOME k)=> if ht k (string_of p) <= l
                                  then block k (string_of p) 
                                  else substring(string_of p,0,k-3) ^"..."  
                                  (* printing shortened - somewhat panicking *)
              
              end

      val string_of_name = string_of_path


      fun obj_type (Content x) = LeafT(M.obj_type (fst x))
         |obj_type (Folder (a,_)) = FolderT

      fun isFolderType FolderT = true 
         |isFolderType _       = false

      fun getContentType (LeafT x) = x
      fun ContentType (x) = LeafT x

      fun folder_icon ()= Icons.getIcon
                               ((SmlTk.getLibPath())^
                                 "/icons/gengui/",  
                                                   (* HACK !!! This is not
                                                      config-dependent *)
                                 "folder.gif");

      fun icon (LeafT x) = M.icon x
         |icon (FolderT) = folder_icon()(* INEFFICIENT ! ! ! leads to
                                           reload of the icon whenever
                                           drag on folder dragged... *)

      val string_of_name_node = N.string_of_name_node
      val rename_node = N.rename_node
      val reset_name_node = N.reset_name_node
      val ord_node = N.ord_node

      fun get_path p a b  = 
          (case ord(a,b) of
            EQUAL => [(rev p,if isFolder b then NONE 
                             else SOME (fst(getContent b)))]
           |_     => (if not (isFolder a) then []
                      else let val (n,S) = getFolder a
                               fun f a = get_path (n::p) a b
                           in  List.concat(map f S) end)
                    
          )
 
      val get_path  = get_path [];

      fun ord_path ((p,M),(p',M')) = 
          case lex_ord ord_node (p,p') of
                  EQUAL => (case M of
                              NONE => (case M' of 
                                           NONE => EQUAL
                                         | SOME _ => LESS)
                            | SOME X => (case M' of 
                                           NONE => GREATER
                                         | SOME X' => M.ord(X,X')))
               |  X => X


      fun  is_prefix (([],NONE),(p,M'))         = true
         | is_prefix (([],SOME X),([],SOME(X')))= (M.ord(X,X') = EQUAL)
         | is_prefix (([],SOME X),_)            = false
         | is_prefix ((a::p,M),([],M'))         = false
         | is_prefix ((a::p,M),(a'::p',M'))     = 
                    case ord_node (a,a') of
                       EQUAL => is_prefix ((p,M),(p',M'))
                    |  X     => false
 

      fun concat_path((p1,NONE),(p2,R)) = (p1@p2,R)
  
      exception InconsistPath;

      (* one could imagine a more liberal version (which is
         even easier to implement), that yields the *list of objects*
         if path-id's are not unique. I chose this more strict
         version for efficiency reasons *)
      fun select_from_path (obj::R) ([a],NONE) = 
                if isFolder obj then 
                   case ord_node(a,fst(getFolder obj)) of
                     EQUAL => obj
                   | _ => select_from_path R ([a],NONE)
                else select_from_path R ([a],NONE)
         |select_from_path (obj::R) ([],SOME a)=
                if isFolder obj then select_from_path R ([],SOME a)
                else(case M.ord(a,fst(getContent obj)) of
                       EQUAL => obj
                     | _     => select_from_path R ([],SOME a))
         |select_from_path (obj::R) (a::R',r)= 
                if not (isFolder obj) then select_from_path R (a::R',r)
                else let val (n,S) = getFolder obj
                     in  case ord_node(a,n) of
                           EQUAL => select_from_path S (R',r) 
                         | _     => select_from_path R (a::R',r)
                     end
         |select_from_path _ _ = raise InconsistPath
                           
                                                  
     fun update objs path X = 
      (* update is very liberal - it allows none or multiple
         replacements in cases of ambiguities *)
      (* potential improvements: other mapPartial (only first replacement)
                                 working inherently on lists ... *)
          let fun upd path [] = raise InconsistPath
                 |upd ([],SOME x) (Content(bob,h)::R) = 
                      (* search for leaf on leaf *)
                      (case M.ord(x,bob) of
                        EQUAL => X@R
                       | _    => (Content(bob,h) :: upd ([],SOME x) R))
                 |upd ([],SOME x) (A::R) = A :: upd ([],SOME x) R
                      (* search for leaf on folder *)
                 |upd ([x],NONE) (Folder(n,S)::R) =
                      (case ord_node(x,n) of
                         EQUAL => X@R
                       | _     => (Folder(n,S):: upd ([x],NONE) R))
                 |upd ([x],NONE) (A::R) = A :: upd ([x],NONE) R
                 |upd (x::R,H) (Folder(n,S)::R') = 
                      (case ord_node(x,n) of
                         EQUAL => Folder(n,upd (R,H) S) :: R'
                       | _     => Folder(n,S) :: upd (x::R,H) R')
                 |upd (x::R,H) (A::R') = A :: upd (x::R,H) (R')

          in  upd path objs end;


      fun remove_at_path objs path = update objs path []

      fun update_at_path objs path repojb = update objs path [repojb]
   
end;
   
(*  fields (fn x = #"/")  translate ... *)
          
     

     

      
     
