(* ***************************************************************************

   $Source: /repository/sml/sml_tk/src/toolkit/appl.sig,v $

   Signature for "applications" of generic graphical user interface. 

   APPL_SIG is the signature for an application with which to 
   instantiate gen_gui. It comes in seberal variants and degrees of 
   completion.

   See <a href=file:../../doc/manual.html>the documentation</a> for more
   details.  tests+examples/simpleinst.sml contains a small example
   of how to use this package.
 
   $Date: 2001/03/30 13:39:37 $
   $Revision: 3.0 $
   Author: cxl (Last modification by $Author: 2cxl $)

   (C) 1996, 1998, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)




(*

 Signature for a simple application with which the generic GUI can
 be instantiated. 

 *)

signature NP0_APPL_SIG =
  sig
 
    include OBJECT_CLASS

    (* New objects are objects together with an annotation where
     * they should appear.  This is a coordinate followed by an Anchor
     * which gives the direction in which GenGUI tries to place the
     * object if another object is in the way. 
     * new_object will correspond directly to Contents in TreeObjects. 
     *)
    type new_object = object * (SmlTk.Coord* SmlTk.AnchorKind)

    (* Now comes the GenGUI-specific OBJECT_CLASS extensions: 
       Typing, modes, is_constructed, outline. *)
    (* Typing *)

    val objlist_type : object list -> objtype Option.option

    val is_constructed : objtype-> bool  
                        (* objects of this type are construction objects *)


    (* "Modes" are states for objects. They are changed with the object's pop-up
     * menu, which displays the mode by the mode_name given below. 
     * Every object's mode can be set within the range given by its type 
     * (function modes below) by set_mode. 
     * Every object's mode can be set within the range given by its type (function
     * modes below) by set_mode. 
     *)
    eqtype mode
 
    val mode      : objtype       -> mode   (* New ! mode is attached to objtype
                                               for structuring reasons . . . *)
    val modes     : objtype       -> mode list
    val mode_name : mode          -> string
    val set_mode  : object * mode -> unit

    (* These objects are displayed with an "outline" icon, to indicate
     * some out-of-date condition. Note that they can still receive
     * drag-and-drop operations.
     *)
    val outline      : object-> bool 

    (*
     * Nullary objects are constants, or in other words, objects
     * existiting a priori. 
     * The init function returns a list of all these objects; it will 
     * only be called once, on startup.
     *)
    val init   : unit -> new_object list

    (* Unary operations *)

    (* standard actions, called ops for historic reasons *)
    val std_ops       : objtype-> ((object -> unit) * string) list  
                                           (* better signature ? *)
    val create_actions: (({pos : SmlTk.Coord, tag : string} -> unit) 
                         * string) list
    val label_action  : {obj : object,
                         cc : string -> unit}-> unit    
    val delete        : object -> unit
    
    
    (* further object type specific operations: for a type t, monOps t
     * is a list of pairs (f, s), where f is a unary operation, and s
     * is a string, the name under which it appears in the pop-up
     * menu. f has the functionality 
     *     object* SmlTk.Coord-> (newObject-> unit)-> unit;
     * where the first argument is the object itself, together with its present
     * location, and the second argument is a continuation you can use
     * to create new objects.  
     *) 
    val mon_ops : objtype -> 
	         ((object * SmlTk.Coord -> 
		   (new_object ->  unit) -> unit) * string) list

    (*
     *
     * binary operations 
     *
     * aka.the drag&drop-action-table
     *)
    val bin_ops :  objtype * objtype -> (object * SmlTk.Coord * 
				 	 object list * 
					 (new_object-> unit) -> unit) 
	                                                        Option.option

    (* --- Substructures -------------------------------------------------- *)


     (* The clipboard will allow the exchange of items between 
      * the drag&drop area and other application-specific 
      * widgets-- eg. a chooser. 
      * It gets passed closures of objects, so we create
      * an object only if it is really taken out of the clipboard 
      *)
     
     structure CB : CLIPBOARD
     sharing type CB.obj = cb_objects
	 

   (* --- Configuration -- see above --- *)

    structure Conf : GENGUI_CONF
 

end; 


signature NP_APPL_SIG =
  sig
 
    include NP0_APPL_SIG

    val object_action   : {win : SmlTk.WinId,
                           obj : object,
                           replace_object_action : object -> unit,
                           outline_object_action : unit -> unit}
                          -> unit

    val is_locked_object: object -> bool (* locking manipulations - 
                                          * e.g. opened construction objects *)

end;

signature APPL_SIG =
  sig
 
    include NP0_APPL_SIG

    (* --- The Construction Area ----------------------------------------- *) 

    type ca

    (* This data type represents the Construction Area's
     * state. It might eg. probably contain the area's
     * widget's widget id.
     *)
	
    (* This should be the respective row of the drag&drop table in
     * binaryOps above. Has to be here explicitly, since it will change
     * the whole area rather than just the object. Further, objects may
     * behave differently while being open.
     *)	
    val  area_ops  : objtype-> ca-> object list-> unit
	
    (* open an object to be worked on the construction area
     * The old object is deleted from the manipulation area.
     *
     * One (or even more?) new objects may appear on the notepad
     * when the construction finishes, they are introduced with 
     * the second argument.
     *
     * The result is a tuple, consisting of a data structure
     * ws as above, a list of widgets representing the 
     * area on the screen and an init function to be called after 
     * the widget has been placed and that would not be necessary
     * if we could instantiate text widgets properly. 
     *)
    val area_open  : SmlTk.WinId* object* (object -> unit) -> 
                                      (ca* SmlTk.Widget list* (unit-> unit))
    val area_init  : unit-> unit
		(* initializations that need to be done only once.
		 * !!! Caution, this is called when the area isn't open.
		 *)



  end









