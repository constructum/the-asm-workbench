(* ***************************************************************************

   $Source: /repository/sml/sml_tk/src/com.sml,v $

   Basic communication layer: sending & receiving,
   sending commands and receiving events, main loop and control. 

   This module implements the sml_tk event handling mechanism -- i.e. 
   the bit which listens to something coming from Tcl, figures out which
   binding this corresponds to, and calls the corresponding SML function.

   Below, we have two main functions, interpret_event: string-> unit
   which takes a string returned by the which and figures out what to
   do with it, and appLoop: unit-> unit which is the main event loop,
   which listens to the pipes to all currently running applications,
   reads their answer, dispatches their handling, and most importantly
   loops (hence the name).

   (Probably, these two functions should not be in the same module).
  
   $Date: 2001/03/30 13:39:05 $
   $Revision: 3.0 $

   Author: bu/stefan (Last modification $Author: 2cxl $)

   (C) 1996-99, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

structure Com : COM
= 
struct

    open BasicTypes BasicUtil ComState GuiState

(***********************************************************************
 *
 * WRITING AND READING
 *
 *)

(* getLine() strings can only be used for texts that are certain not *)
(* contain \n. Otherwise, getlineM() (M for multiple) has to be used.*)
(* On the other side, an appropriate writeM is provided. *)


fun doProtIn t =
    case getWishProt() of
	SOME prot =>(TextIO.output(prot, "<== " ^ t ^"\n");
		     TextIO.flushOut prot;
	             t)
	  | NONE => t

fun getLine ()  = 
    let
	val t = ComState.getEvent()
	(* strip off concluding "\n" *) 
	val t = substring(t,0,(size t) -1)
    in 	
	doProtIn t
    end
  
fun getLineM () =
    let
	fun getls () = 
	    let
                val t = ComState.getEvent()
	    in    		
		if t = "EOM\n" then "" else t ^ getls () 
	    end
    in
	doProtIn (getls())
    end


fun putLine ps = 
    (case getWishProt() of
	 SOME prot =>(TextIO.output(prot, "==> " ^ ps ^"\n");
		      TextIO.flushOut prot)
       | NONE => ();
     ComState.eval ps)


(***********************************************************************
 *
 * SENDING COMMANDS
 *
 *)


fun putTclCmd cmd =
    let
	val emsg = fn s => (StringUtil.concatWith " " s)
	fun getAnswer aws =
	    let
		val a    = getLine() 
		val ss   = StringUtil.words a
		val _    = Debug.print 1 ("Com.putTclCmd: got \""^a^"\"");
		val kind = hd ss
	    in
		if (kind = "CMDOK" orelse kind = "ERROR" ) then
		    (a,aws)
		else
		    getAnswer(aws@[a])
	    end 

	val _         = putLine ("WriteCmd \"CMDOK\" {"^ cmd ^ "}")
	val (a,binds) = getAnswer []

	val gaws      = ComState.getTclAnswersGUI()
	val _         = ComState.updTclAnswersGUI(gaws@binds)

	val _         = if not (length binds = 0) then
	                   Debug.print 1 "Missed Binding"
		        else ()
    in
	case (hd (StringUtil.words a)) of
	    "CMDOK" => ()
	  | "ERROR" => Debug.warning ("Com.putCmd: got Tcl Error: \""^ a ^"\"")
	  | s       => Debug.warning ("Com.putCmd: got unexpected answer: \""^ s ^"\"")
    end handle Empty => Debug.warning ("Com.putCmd: no answer")


fun readTclVal req =
    let
	val concatSp = StringUtil.concatWith " "

	fun getAnswer aws =
	    let
		val a    = getLine() 
		val ss   = StringUtil.words a
		val kind = hd ss
		val _    = Debug.print 1 ("Com.readTclVal: got \""^a^"\"");
	    in
		if (kind = "VValue" ) then
		    (concatSp(tl(ss)), aws)
		else
		    getAnswer(aws@[a]) 
	    end

	val _         = putLine ("WriteSec \"VValue\" {"^ req ^ "}") 
	val (a,binds) = getAnswer []

	val gaws = ComState.getTclAnswersGUI()
	val _    = ComState.updTclAnswersGUI(gaws@binds)
    in
	a
    end

fun readAnswerFromTcl interpret_answer =
    (case (ComState.getTclAnswersGUI ()) of
         []        => ()
       | (ta::tal) => (ComState.updTclAnswersGUI(tal);
                       interpret_answer ta;
                       readAnswerFromTcl interpret_answer))

(* val forceTcl2doOneEvent = ComState.doOneEvent_nowait *)


(* "communicate" *)

val commToTcl   = "Write"
val commToTcl'  = "SWrite"
val writeToTcl  = "Write"
val writeMToTcl = "WriteM"



(***********************************************************************
 *
 * MAIN CONTROL
 *
 * Setting up the communication.
 *
 *)

fun resetTcl() = (GuiState.initGuiState();
		  ComState.initComState())

fun initTcl() = 
    (ComState.initWish();
     putLine ((getTclInit()) ^ prelude_tcl))

fun exitTcl()  = 
    (putLine "destroy .";
     closeWish();
     initGuiState();
     initComState())


end
