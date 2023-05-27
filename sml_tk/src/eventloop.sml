(* ***************************************************************************

   $Source: /repository/sml/sml_tk/src/eventloop.sml,v $

   sml_tk event handler.

   This module implements the sml_tk event handling mechanism -- i.e. 
   the bit which listens to something coming from Tcl, figures out which
   binding this corresponds to, and calls the corresponding SML function.

   Further, there are the two functions, startTcl and startTclExn, which
   launch the application loop and the GUI.

   $Date: 2001/03/30 13:39:10 $
   $Revision: 3.0 $

   Author: bu/stefan (Last modification $Author: 2cxl $)

   (C) 1996-99, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

structure Eventloop : EVENTLOOP =
struct

open BasicUtil BasicTypes GuiState



(***********************************************************************
 *
 * sml_tk's main event handler.
 *
 * This is the 'raw' event handler. Below, we wrap the interrupt handler
 * around this, so diverging event handling functions can be interrupted
 *)

fun do_interpret_event s =
    let val kind::win::path::ss = StringUtil.words s
    in
	case kind of
          "Command"  => WidgetTree.selectCommandPath (win, path) () 
	| "MCommand" => let val mitpath = Config.readCascPath (hd ss)
			in WidgetTree.selectMCommandMPath (win, path) mitpath () 
	                end
        | "SCommand" => let
	 	           val newval = valOf(Real.fromString (hd (ss)))
	                in
		           WidgetTree.selectSCommandPath (win, path) newval
	                end
	| "Destroy"  => (let
			   val key  = path (* no path in Window Bindings *)
			   val ev_v = hd (ss)
			   val tkev = TkEvent.unparse ev_v
			   val wind = (SOME (GuiState.getWindowGUI win))
			              handle WINDOWS t => NONE
	                in (case wind of
		            SOME wind=>(Window.selectBindKeyPath win key)tkev
		          | NONE      => ();
		            if GuiState.isInitWin win
		               then (Com.exitTcl();
		                     Window.deleteAllGUI)
		               else Window.deleteGUI win)
	                end
	                handle WINDOWS t => ())
	| "WinBinding" =>
			(let
		  	   val key  = path (* no path in Window Bindings *)
			   val ev_v = hd (ss)
			   val tkev = TkEvent.unparse ev_v
			   val wind = (SOME (GuiState.getWindowGUI win))
			               handle WINDOWS t => NONE
	                 in
		           case wind of
		              SOME wind => (Window.selectBindKeyPath win key) tkev
		            | NONE      => Debug.print 1 "got NONEX-WBinding\n"
	                 end
	                 handle WINDOWS t => Debug.warning ("Exception WINDOWS: "^t))
	| "WBinding" => (let 
			   val key::ev_v::_  = ss
			   val tkev = TkEvent.unparse ev_v
			   val wid  = (SOME (WidgetTree.getWidgetGUIPath (win,path)))
			              handle WINDOWS t => NONE
					   | WIDGET t  => NONE
		         in case wid of
		           SOME wid => (WidgetTree.selectBindKeyPath (win, path) key) tkev
		         | NONE     => Debug.print 1 "got NONEX-WBinding\n"
	                 end
	                 handle WIDGET      t => Debug.warning ("Exception WIDGET: "^t)
		              | CItem.CITEM t => Debug.warning ("Exception CITEM: "^t))
	| "CBinding" => (let
			   val cid::key::ev_v::_  = ss
			   val tkev = TkEvent.unparse ev_v
			   val wid  = (SOME (WidgetTree.getWidgetGUIPath (win,path)))
			              handle WINDOWS t => NONE
				  	   | WIDGET t  => NONE
	 	         in case wid of
			      SOME wid => (CItem.getBindingByName wid cid key) tkev
			    | NONE     => Debug.print 1 ("got NONEX-CBinding\n")
		         end
		         handle CItem.CITEM t => Debug.warning("Exception CITEM: "^t)
			      | WIDGET      t => Debug.warning("Exception WIDGET: "^t))
	| "TBinding" => (let 
			    val tn::key::ev_v::_   = ss
			    val tkev = TkEvent.unparse ev_v
			    val wid  = (SOME (WidgetTree.getWidgetGUIPath (win,path)))
		          		     handle WINDOWS t => NONE
					  	  | WIDGET t  => NONE
	    		in case wid of 
		             SOME wid => (Annotation.getBindingByName wid tn key) tkev
		           | NONE     => Debug.print 1 ("got NONEX-TBinding\n")
	                end
	                handle CItem.CITEM t => Debug.warning("Exception CITEM: "^t)
		             | WIDGET      t => Debug.warning("Exception WIDGET: "^t))
	| "VValue"   => Debug.print 1 ("Eventloop.interpret_event: someone missed VValue")
	| "ERROR"    => (Debug.print 1 ("Eventloop.interpret_event: got Tcl Error: \"" ^
			               (StringUtil.concatWith " " (win::path::ss))^"\"");
	                 raise TCL_ERROR ("Eventloop.interpret_event: got Tcl Error: \"" ^
			                 (StringUtil.concatWith" "(win::path::ss))^"\""))
	| _          => Debug.warning ("Tcl junk sent to SmlTk: " ^ s) 
    end
(*
    handle e => Debug.warning ("Event.interpret_event: exception " ^ (exnName e) ^
			       " raised (and ignored) with event: "^ s) 
*)

(***********************************************************************
 *
 * Interrupt Handling 
 *
 * I refer the honourable gentleman to the answer I gave earlier.
 *)

   val lcnt = ref 0

   datatype intr_listener = mkIL of int

   val listeners = ref [(0, fn()=> TextIO.print "[sml_tk] Interrupt.\n")]

   (* Register an interrupt listener, i.e. a function f:unit-> unit to be
    * called when an interrupt occurs. Use sparingly if ever. *)
   fun registerIntrListener h= 
       let val id= inc lcnt 
       in  (mkIL id) before (listeners := (!listeners)@[(id, h)])
       end

   (* deregister this listener: don't call us, we'll call you *)
   fun deregisterIntrListener (mkIL id)= 
       listeners := List.filter (fn(lid, _)=> not (lid= id)) (!listeners)

   (* call all the interrupt listeners *)
   fun get_listeners s = 
       List.foldl (op o) (fn x=> x) (map #2 (!listeners)) s

   (* The 'real' event handler is this one *)
   fun interpret_event s =
       SysDep.interruptable do_interpret_event get_listeners s

(***********************************************************************
 *
 * The main application loop
 * 
 * For the wish, we sometimes need to read Tcl values (e.g. 
 * readCoords, getVal) and while we do so, other Tcl bindings may fire. 
 * In this case, these Tcl answers are stored in the COM_state, and 
 * are processed separately by readAnswerFromTcl below.
 *)

fun appLoop _ = 
    while (ComState.wishActive()) do
	(Com.readAnswerFromTcl interpret_event; 
	 interpret_event (Com.getLine()))

(***********************************************************************
 *
 * Launching the application loop.
 *
 *)


fun startTcl ws =
    (Com.initTcl();
     app Window.openW ws; appLoop())

fun startTclExn ws = 
    (startTcl ws; "") 
    handle WIDGET      t => "WIDGET: "^t
	 | CItem.CITEM t => "CITEM: "^t
	 | WINDOWS     t => "WINDOWS: "^t
	 | CONFIG      t => "CONFIG: "^t
	 | BasicTypes.TCL_ERROR t => "TCL_ERROR: "^t
	 | Annotation.ANNOTATION  t => "ANNOTATION: "^t

end


