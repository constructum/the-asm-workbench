(* ***************************************************************************

   $Source: /repository/sml/sml_tk/src/toolkit/gen_gui.sig,v $

   Signature for the generic graphical user interface. 

   GENGUI_SIG is the export signature of GenGUI

   See <a href=file:../../doc/manual.html>the documentation</a> for more
   details.  tests+examples/simpleinst.sml contains a small example
   of how to use this package.
 
   $Date: 2001/03/30 13:39:42 $
   $Revision: 3.0 $
   Author: cxl (Last modification by $Author: 2cxl $)

   (C) 1996, 1998, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)



(* 
 
 Configuration for the gen_gui-- modfiying its visual appearance.
 
 *)

signature GENGUI0_CONF =

    sig
	(* This is the width and height of the notepad area *)
	val width         : int
	val height        : int       
	    
	(* The background colour of the notepad and construction area *)
(* moved to GlobalConfig !!! bu  
	val background    : SmlTk.Color
 *)
	    
	(* The font and the width of the box, in pixels,
	 * used to display the icons *)
	val iconNameFont  : SmlTk.Font
	val iconNameWidth : int
	    
	(* if opaqueMove is true, then the whole item will move if it
	 * is grabbed; ows. only change cursor to indicate an object is
	 * being moved. *)
	val moveOpaque    : bool

	(* The minimum distance between two objects' dropzones when placing
	 * new objects
	 *)
	val delta         : int
    end	    


signature GENGUI_CONF = 
    sig
	include GENGUI0_CONF

	(* if oneWindow is true, the construction area will appear as 
	 * a widget within the lower part of the window (which will be 
	 * large enough to hold it. Actually, this should be called _one
	 * widget_, since it means that both construction and assembly
	 * come within one frame); otherwise, the construction area 
	 * will appear as a separate window 
	 *)
	val oneWindow     : bool
	    
	(* The height and width of the construction area, and the
	 * position of the window. The X/Y position and the caTitle options
	 * determine the placing and title of the construction area window,
	 * and only take effect if oneWindow is false. 
	 *)
	val caHeight      : int
	val caWidth       : int
	val caXY          : (int* int) Option.option
	val caTitle       : string-> string

	(* The icon used to display the trashcan, and its initial position *)
	(* NB. Can only construct sml_tk icons at runtime *)
	val trashcanIcon  : unit-> Icons.icon
	val trashcanCoord : SmlTk.Coord  
    end


(*

 Export signature for the simple generic GUI. 

 *)

signature GENGUI_SIG =  
  sig
     type object
     type new_object

     (* the state of the gui *)
     type gui_state

     val intro : new_object -> unit 
	 (* introduce (not "create" really) a new object into the
	  * manipulation area
	  *)

     val elim  : object -> unit
         (* remove (not "delete" really) an (selected!) object from the 
	  * manipulation area; causes *not* appl.delete
	  *)


     (* The GenGUI main widget. You MUST use the init function below
      * to initialize this widget. (Note GenGUI doesn't check this 
      * itself.)
      *)
     val main_wid : SmlTk.WinId -> SmlTk.Widget 

     
     (* In the following, init takes a gui_state and returns a function
      * which has to be used as the init function of the main window,
      * as it sets up the GenGUI. 
      *
      * state returns the current gui_state suitable as an argument to init. 
      *)

     val init  : gui_state-> unit
	 (* call that as init action of main window *)

     val state : unit-> gui_state

     (* This is the initial state which only has those objects as given
      * by the application's init() function (see above). 
      *)
     val initial_state : unit-> gui_state

     (* Resynchronize all icons, e.g. if objects have changed their mode.
      * (Unfortunately, we cinnae change icons of single objects, since
      *  we can't identify objects...)
      *)
     val redisplay_icons : (object-> bool)-> unit 

     exception GenGUI of string 
	 (* something went wrong-- this execption indicates a
	  * critical error on part of the gen_gui. This may either
	  * be a genuine bug (although due to the state-of-the-art
	  * software technology used to implement gen_gui, this is un-
	  * likely), or wrong usage of GenGUI. 
	  * The exception is critical in the sense that it's all right
	  * to just panic after it has been raised. Alternatively, catch
	  * it, ignore it and hope for the best. 
	  *)

     (* The clipboard is just reexported, to allow external components
      * (e.g. the filer) to create objects.
      *)
     
     type cb_objects  (* = (unit -> object list) ; sharing in SML97 
                         can be so tedious !!! *)
     val  cb_objects_abs : (unit -> object list) -> cb_objects
     val  cb_objects_rep : cb_objects -> (unit -> object list) 
     structure CB : CLIPBOARD
     sharing type CB.obj = cb_objects

  end


(* 
functor GenGUI(structure appl: APPL_SIG ) :
  sig 
      include GENGUI_SIG 
      sharing type object     = appl.object 
	  and type new_object = appl.new_object
  end

= ? 

*)










