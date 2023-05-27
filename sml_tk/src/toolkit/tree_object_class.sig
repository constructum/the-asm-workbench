(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/tree_object_class.sig,v $
 
   Unified Object Interface
 
   $Date: 2001/03/30 13:39:55 $
   $Revision: 3.0 $
   Author: bu (Last modification by $Author: 2cxl $)

   (C) 1999, Albert-Ludwigs-Universität Freiburg
 
  ************************************************************************** *)

(* The entity FOLDERINFO contains just the information that
   makes abstractly the "skeleton" or just the "node" of
   a folder, but not its content. This node_info must comprise:
   - an ordering 
   - an (implicit) name
   - rename and reset-name facilities 
   - additional info's that may be attached directly to subcomponents
     of a folder (i.e. positions, layout, ...) 
*)

signature FOLDERINFO =
sig
   type   node_info
   type   subnode_info
   val    string_of_name_node : node_info -> Print.format -> string
   val    ord_node            : node_info * node_info -> order
   val    rename_node         : string -> node_info -> unit
   val    reset_name_node     : node_info -> unit 
end

(* SPECIFICATION :
   - ord_node is a linear ordering
   - (rename_node s ni;
      string_of_name_node ni f) = "nice s"
   - (rename_node s ni;
      reset_name_node ni;
      string_of_name_node ni f) = 
     (reset_name_node ni;
      string_of_name_node ni f)
 *)
      


(* The entity TREE_OBJECT_CLASS is just a subclass of 
   OBJECT_CLASS. It is enriched by FOLDER_INFO, SUBNODE_INFO and functions,
   that exploit (or enforce) the tree-like structure of TREE_OBJECT_CLASS-
   elements - i.e. terms of type obj. 
   Additionally, they provide the concept path on folders and
   path-related operations.
*)

signature TREE_OBJECT_CLASS =
sig
   include OBJECT_CLASS;
   include FOLDERINFO;
   structure Basic : OBJECT_CLASS;
   val  getContent       : object -> Basic.object * subnode_info
   val  getFolder        : object -> node_info * object list
   val  isFolder         : object -> bool
   val  Content          : Basic.object * subnode_info -> object
   val  Folder           : node_info * object list -> object
   val  isFolderType     : objtype -> bool
   val  getContentType   : objtype -> Basic.objtype
   val  ContentType      : Basic.objtype -> objtype
end

(* SPECIFICATION :
   - getContent(Content m) = m
   - getFolder(Folder m) = m
   - Content(getContent o) = o
   - Folder(getFolder o) = o
   - tree_objects are generated over Content and Folder
   - isFolder(Folder m) = true, isFolder(Content m) = false
   - getContentType(ContentType ot) = ot
   - ContentType(getContentType ot) = ot
 *)
      


signature PTREE_OBJECT_CLASS =
sig
   include TREE_OBJECT_CLASS;

   (* necessary for SML97 - compilience : synonyms no longer allowed . . . *)
   type path
   val  path_rep         : path -> node_info list * Basic.object option
   val  path_abs         : node_info list * Basic.object option -> path
   (* path and name are identical in PTREE_OBJECT_CLASS. Unfortunately,
    * this can't be said explicitly in SML. Therefore, we establish an
    * isomorphism. *)

   val  ord_path         : path * path -> order
   val  is_prefix        : path * path -> bool
   val  concat_path      : path * path -> path

   val  name2path        : name -> path  
   val  path2name        : path -> name  

   (*   The foll. opns may fail if paths do not exist or are not unique   *)
   (*   NOTE : this implies that node_info and obj must be unique if      *)
   (*   these operations are expected to work properly *)
   val  get_path         : object -> object -> path list   
                           (* get_path a b produces path 
                              of sub-object b in object a *)
   exception InconsistPath;

   val  select_from_path : object list -> path -> object         

   val  remove_at_path   : object list -> path -> object list
                           (* removes_at_path a   produces object 
                              from a with subobject at p removed *)

   val  update_at_path   : object list -> path -> object -> object list

end


(* SPECIFICATION :
   - to be done
   select_from_path, remove_at_path and update_at_path use inherently
   the uniqueness of paths and may deliver arbitrary result if it
   is not assured. May raise InconsistPath if path not accessible.
   It is allowed to replace objects byb olders and vice versa -
   the application has to assure uniqueness !!!
 *)
   
