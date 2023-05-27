(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/enter_windows.sml,v $
 
   Windows to enter substitutions or related data structures (like
   substitutions).
 
   $Date: 2001/03/30 13:39:39 $
   $Revision: 3.0 $
   Author: bu/kol/cxl (Last modification by $Author: 2cxl $)

   (C) 1997-99, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)
 


signature SUBSTWIN =
sig 

    (* a substitution is a list [(p_i,str_i)] of pairs of strings,
     * where p_i is the "parameter names" and str_i its value. 
     *
     * In the following, new creates a new list of substitutions,
     * ie. str_i are (initially) empty, whereas edit takes an existing
    * substitution. The "title" below is the window title.
     *)

    val new : {title  : string,
	       width  : int,
	       params : string list,
	       cc     : (string * string) list -> unit}-> unit

    val edit : {title : string,
		width : int,
		subst : (string * string) list,
		cc    : (string* string) list-> unit}-> unit
end 


structure SubstWin: SUBSTWIN

= 


struct

    open SmlTk 

    (* These lines, copied from util_win here, should go into some general
     * configuration thingy *)

    val msgFont      = Normalfont []
    val msgWidth     = 40
    val buttonRelief = Raised
    val buttonWidth  = 5
    val buttonFont   = SansSerif []	    
    val enterTextFont = Typewriter []
	
    fun upto(from, to) = if to< from then [] else from::upto(from+1,to)

    fun doSubst(width, sep, wintitle, subst, cc) =

	let (* Width of variable entry boxes: max. length of a var. name +2 *)
	    val varWidth = (foldr
		            (fn ((a, _), m) => if (size a)> m then (size a)
					      else m) 0 subst)+ 2

	    (* some widget ids *) 
	    fun lhsWidId(w, n) = subWidId(w, "substLhs"^Int.toString n)
	    fun rhsWidId(w, n) = subWidId(w, "substRhs"^Int.toString n)
	    fun substFrmId w   = subWidId(w, "substFrm")
	    fun addButtonId w  = subWidId(w, "substBttn")
	    fun clsButtonId w  = subWidId(w, "substCls")

	    fun zipIt s = ListPair.zip (upto (1, length s), s)
		
	    (* entry box for one substitution *)
	    fun oneEntry w (n, (par, str)) = 
		Frame {widId=newWidgetId(), 
		      widgets= Pack [Entry{widId=lhsWidId(w, n),
			     packings=[Side Left],
			     configs=[Width varWidth, Font enterTextFont], bindings=[]},
		       Label{widId=newWidgetId(), packings=[Side Left], 
		             configs=[Text sep],bindings=[]},
		       Entry{widId=rhsWidId(w, n),
			     packings=[Side Right], 
			     configs=[Width width, Font enterTextFont],bindings=[]}
		       ], packings=[Side Top], configs=[], bindings=[]}

	    (* frame with all substitutions. Needs to be one frame so we
	     * can add new subst-entry boxes *)
	    fun allSubsts w substs =
		Frame{widId=(substFrmId w), 
		      widgets=Pack(map (oneEntry w) (zipIt subst)),
		      packings=[Side Top], configs=[], bindings=[]}

	    (* close window, read values, continue with cc *)
	    fun closeSubst(win, wid, n) =
		let fun getSub n = (readTextAll(lhsWidId(wid, n)), 
				    readTextAll(rhsWidId(wid, n)))
    		    val subst = List.filter (fn (p, _)=> not(p=""))
			                    (map getSub (upto (1, n)))
		in  closeWindow win; 
		    cc subst
		end

	    (* add another entry box. Note reconfiguration of the commands
	     * bound to the add-button and the close-button 
	     *)
	    fun addSubst(win, wid, n) =
		(addConf (addButtonId wid)
		         [Command (mkSimpleAction (fn()=> addSubst(win, wid, n+1)))];
		 addConf (clsButtonId wid)
		         [Command (mkSimpleAction (fn()=> closeSubst(win, wid, n+1)))];
		 addWidget win (substFrmId wid) (oneEntry wid (n+1, ("", "")))
		)

	    fun substFrame(win, wid) =
		Frame{widId=newWidgetId(),
		      widgets=Pack [allSubsts wid subst,
			Button{widId=(addButtonId wid),
			   packings=[Side Top, Fill X],
			      configs=[Width varWidth, Text "Add Parameter", Font buttonFont,
			       Command (mkSimpleAction(fn()=> 
				addSubst(win, wid, length subst)))],
			      bindings=[]}],
		      packings=[Side Top, Fill X],configs=[],bindings=[]}

	    fun buttonFrm(win, wid) =
		Frame{widId=newWidgetId(),
		      widgets=Pack [Button{widId=(clsButtonId wid),
			      packings=[Side Right], 
			      configs=[Text "OK", Width buttonWidth, 
			       Font buttonFont, 
			       Command (mkSimpleAction(fn()=> closeSubst(win, wid,
							  length subst)))],
			      bindings=[]},
		       Button{widId=newWidgetId(),
			      packings=[Side Left],
			      configs=[Text "Cancel", Width buttonWidth, 
			       Font buttonFont,
			       Command (mkSimpleAction(fn()=> closeWindow win))],
			      bindings=[]}],packings= [Fill X, Side Bottom], 
			      configs=[],bindings=[]}

	    (* initializiation function *)
	    fun fillSubst wid (n, (p, str)) = 
		(insertTextEnd (lhsWidId(wid, n)) p;
		 insertTextEnd (rhsWidId(wid, n)) str)

	    val win = newWinId()
	    val wid = newWidgetId()	  

	in  
	    openWindow(mkWindow{winId    = win,
				config   = [WinTitle wintitle],
				widgets  = Pack [substFrame(win, wid),
					         buttonFrm(win, wid)],
                                bindings = [],
				init     = (fn ()=> app (fillSubst wid) (zipIt subst))})
	end


    fun new{title, params, width, cc} = 
	doSubst(width,  " |-> ", title, map (fn str=> (str, "")) params, cc)
		 
    fun edit{title, subst, width, cc} =
	doSubst(width,  " |-> ", title, subst, cc)

end
