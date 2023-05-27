(* ***************************************************************************

   $Source: /repository/sml/sml_tk/src/toolkit/drag_and_drop.sig,v $

   Signatures for drag_and_drop.

   DD_ITEMS is the signature for the drag&drop-items, and 
   DRAG_DROP_SIG is the export signature.
 

   $Date: 2001/03/30 13:39:38 $
   $Revision: 3.0 $
   Authour: cxl (Last modification $Author: 2cxl $)

   (C) 1996, 1998, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)
 
signature DDITEMS = 
    
sig
    type item
    
    val selItemId    : item-> SmlTk.CItemId

    val selDropZone  : item-> SmlTk.Rect
 
    val isImmobile   : item-> bool

    val grab     : item-> unit
    val move     : item-> SmlTk.Coord-> unit
    val release  : item-> unit

    val enter    : item -> item list -> bool
    val leave    : item -> unit

    val select   : item -> unit
    val deselect : item-> unit

    val drop  : item -> item list -> bool (* false <==> drop is destructive,
					   *            dropped items vanish
					   *)

(*

  two "semantic" points to take note of:
  - after a drop, a leave is generated for the item which has been dropped on;
  - a leave is generated _only_ if the preceding enter has returned true,
    otherwise we assume the visited item doesn't want to know

*)

   type item_list (* = item list *)
   val  item_list_rep : item_list -> item list
   val  item_list_abs : item list -> item_list 
	
   structure CB : CLIPBOARD_W                  
                  
   sharing type CB.obj = item_list 

end


signature DRAG_DROP_SIG = 

sig
    type item
    type DDCanvas
	
    exception DragAndDrop of string
    
    (* initialize area *)
    val init   : SmlTk.WidId-> DDCanvas
	
    (* place a new object on d&d canvas *)
    val place  : DDCanvas-> item -> unit

    (* delete an object from the d&d canvas *)
    val delete : DDCanvas-> item -> unit

    (* return all items the dropzone of which is at given point *)
    val overDropZone  : DDCanvas->SmlTk.Coord-> item list
    (* .... or inside a given rectangle *)
    val dropZonesInRect : DDCanvas-> SmlTk.Rect-> item list

    (* selected items (including grabbed items) *)
    val selectedItems : unit-> item list

    (* get all items on a d&d canvas (except selectedItems) *)
    val allItems      : DDCanvas-> item list

    (* reset to a sane state *)
    val reset         : DDCanvas-> unit

end


(*
functor DragAndDrop(DDitems: DDITEMS) : 
    sig
	include DRAG_DROP_SIG
        sharing type item = DDitems.item
    end = ?
 *)






