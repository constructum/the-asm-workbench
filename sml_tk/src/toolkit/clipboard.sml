(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/clipboard.sml,v $
 
   The clipboard is used to exchange objects between different drag&drop
   canvases or other manipulation areas. 
 
   $Date: 2001/03/30 13:39:37 $
   $Revision: 3.0 $
   Author: cxl (Last modification by $Author: 2cxl $)

   (C) 1996, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)


(* In the following TkEvents are used to certify events, to make sure
 * that only matching pairs of objects are put/got. That ensures that
 * an object can only be got from the clibboard if it it has been put
 * there with the same mouse event-- in other words, you don't drop
 * your object somewhere, take the mouse around the screen for an
 * extended tour, and finally two hours later end up in another window
 * with still something in the clipboard.  The second argument to put
 * is a "callback" function which is executed once the object has been
 * successfully taken out of the clipboard with the get function; an 
 * example here would be to  delete the object in the old window once
 * it has appeared elswhere. The "copy" function does the same as get,
 * except it _doesn't_ call the callback function, allowing eg. the copying
 * of the object in the clipboard. Of course, the object disappears from
 * the clipboard.
 *
 * You can imagine a more generic version of this parameterized with 
 * type stamp eq : stamp-> stamp-> bool but then what would be the use
 * of that and it would make debugging mair complicated an' aw.
 *
 * There are two additional subsignatures of CLIPBOARD given, which you can
 * use to coerce to clipboard to being read-only or write-only (i.e. you 
 * can only get things or put things)
 *)


signature CLIPBOARD =
    sig
	type obj

        exception Empty

	val  get  : SmlTk.TkEvent-> obj
	val  copy : SmlTk.TkEvent-> obj
        val  put  : obj-> SmlTk.TkEvent -> (unit-> unit)-> unit

	val  isEmpty: SmlTk.TkEvent -> bool

(*      axiom isEmpty e <==> exists o. o= get e  
        axiom get e      ==> isEmpty f
           --  i.e. even an unsuccessful get will empty the clipboard
 *)	  

    end

signature CLIPBOARD_R = (* read-only access to the clipboard *)
    sig
	type obj

        exception Empty

	val  get: SmlTk.TkEvent-> obj
	val  copy : SmlTk.TkEvent-> obj
	val  isEmpty: SmlTk.TkEvent -> bool

    end

signature CLIPBOARD_W = (* write-only access to the clipboard *)
    sig
	type obj
        val  put: obj-> SmlTk.TkEvent -> (unit-> unit)-> unit
    end


functor Clipboard(obj: sig type obj end) : 
    CLIPBOARD (* where type obj= obj.obj *) =

struct

    open SmlTk
	
    type obj = obj.obj

    exception Empty
  
    val cb = ref NONE : (obj* TkEvent* (unit-> unit)) option ref


    fun  eq (TkEvent(_,_,_,_, x1, y1)) (TkEvent(_,_,_,_, x2, y2)) = 
	 (x1= x2) andalso (y1= y2)

    fun mkstr e = "("^(Int.toString (SmlTk.selXRootPos e))^","^
	              (Int.toString (SmlTk.selYRootPos e))^")"

	
    fun isEmpty queryst = 
	case !cb of 
	    NONE              => true 
	  | SOME(_, putst, _) => not (eq putst queryst)
       
    fun getit callback queryst = 
      (Debug.print 10 ("Clipboard.getit "^(mkstr queryst));
	case !cb of
	    NONE => (Debug.print 10 "Clipboard.getit unsuccessful (mt)"; raise Empty)
          | SOME (cont, putstamp, callb) 
	      => if eq putstamp queryst then 
		  (cb := NONE;
		   if callback then callb() else ();
		   Debug.print 10 "Clipboard.getit succesful after callback";
		   cont
		   )
		 else (cb := NONE;
		       Debug.print 10 "Clipboard.getit unsuccessful (no match)"; 
		       raise Empty)
      )

    val get  = getit true
    val copy = getit false

    fun put obj stamp callb =  (cb:= SOME(obj,  stamp, callb);
				Debug.print 10 ("Object put with stamp "^
						(mkstr stamp))
			       )
					     	   
end





(* 
 * A dummy clipboard.
 * 
 * Use this to instantiate a clipboard-structure in a functor when 
 * you don't really want to use a clipboard
 *)

structure DummyCB = Clipboard(struct type obj= unit end)


