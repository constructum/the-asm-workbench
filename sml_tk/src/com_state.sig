(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/com_state.sig,v $
 
   Basic Data Structures for sml_tk
  
   $Date: 2001/03/30 13:39:05 $
   $Revision: 3.0 $
   Author: bu & behrends

   (C) 1998, ALU Freiburg
 
  ************************************************************************** *)


signature COMSTATE = 
 sig

    (* The prelude to be sent to the wish after starting it *)
    val prelude_tcl : string

    (* Visible components of the com-state, and how to change them *)
    val getLogfilename : unit -> string option
    val getLibPath : unit -> string
    val getTclInit : unit -> string
    val getWishPath : unit -> string
    val getTclAnswersGUI : unit -> BasicTypes.TclAnswer list

    val updLogfilename : string option -> unit
    val updLibPath : string -> unit
    val updTclInit : string -> unit
    val updWishPath : string -> unit
    val updTclAnswersGUI:BasicTypes.TclAnswer list -> unit

    (* set up the wish -- used to be called initCom *)
    val initWish         : unit-> unit

    (* get the stream of the current logfile, if open *)
    val getWishProt     : unit-> TextIO.outstream Option.option

    (* true as long as the eventloop is active and the wish is running *)
    val wishActive      : unit-> bool

    (* get one event from the wish, and send some string to the wish *)
    val getEvent  : unit -> string 
    val eval      : string -> unit
   
    (* close down the wish, and more importantly, close the in/outstreams *)
    val closeWish        : unit-> unit

    (* initialize the com state *)
    val initComState     : unit -> unit

(*  Not needed any more -- I don't think --
    val doOneEvent       : unit -> int
    val doOneEvent_nowait: unit -> int *)


(*  val reset_tcl_interp : unit -> unit -- dito -- is done in initComState *)

  end

