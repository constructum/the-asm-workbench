(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/object_class.sig,v $
 
   Unified Object Interface
 
   $Date: 2001/03/30 13:39:49 $
   $Revision: 3.0 $
   Author: bu (Last modification by $Author: 2cxl $)

   (C) 1999, Albert-Ludwigs-Uni Freiburg
 
  ************************************************************************** *)

(* The signature OBJECT_CLASS (vulgo: the object-class OBJECT-CLASS)
   in the sense of the toolkit has:
   - a type of objects object
   - an ordering on object
   - a unique abstract name for each object
   - a (formatable) string representation thereof
   - a "user-name", i.e. an explicitly
     via side-effect set or reset string hiding the
     string representation of the name.
   - an object-type obj_type for each object
   - an icon for each obj-type

   OBJECT-CLASS is fundamental for several larger
   SmlTk-toolkit components like TreeList or GenGui.
   Moreover, there is a functor obj2tree_obj,
   that extends OBJECT-CLASS to TREE-OBJECT-CLASS,
   where TREE-OBJECT-CLASS is a strict extension
   of OBJECT-CLASS (such that V o obj2tree_obj is in
   a sense an endofunctor on  OBJECT-CLASS . . .
   This facilitates to have a uniform interface
   and common lifting facilities for toolkit-
   components with respect to objects.

   SPECIFICATION:
   - ord is a linear ordering on names of objects
   - name_of(o) must be unique in all system states
     (* this fact is only used in tree_object_classes, 
        more precisely: select_from_path,remove_at_path,update_at_path *)
   - name_of(rename(s,o)) = name_of(o)  (* rename is actually a relabelling *)
   - name_of(reset_name(s,o)) = name_of(o)
   - obj_type(rename(s,o)) = obj_type(o)
   - obj_type(reset_name(s,o)) = obj_type(o)
   - icon(rename(s,o)) = obj_type(o)
   - icon(reset_name(s,o)) = obj_type(o)

   - string_of_name n f should be "as nice as possible".

   - (rename s o;
      string_of_name(name_of o) f) = "nice s"
   - (rename s o;
      reset_name o;
      string_of_name o f) = 
     (reset_name_node o;
      string_of_name_node o f)


*)


signature OBJECT_CLASS =
sig
   type object
   eqtype objtype 
   type name                                     (* think of it as : id *)

   val  ord            : object * object -> order(* based on name *) 
   val  name_of        : object -> name          (* think of it as : id_of *)
   val  string_of_name : name ->   Print.format ->
                                   string        (* annotated text ? *)
   val  rename         : string ->               (* annotated text ? *)
                                   object -> unit(* side effect *)
   val  reset_name     : object -> unit             (* side effect *)
   val  obj_type       : object -> objtype
   val  icon           : objtype -> Icons.icon


   (* for lousy reasons (SML97-compliance), it is necessary to add the
      following lines, that introduce a "clipboardable" version 
      of objects. This is not necessary in njml, but poly and moscow. *)

   type cb_objects
   val  cb_objects_abs : (unit -> object list) -> cb_objects
   val  cb_objects_rep : cb_objects -> (unit -> object list) 
end

