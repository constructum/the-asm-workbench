(* ***************************************************************************

   $Source: /repository/sml/sml_tk/src/com.sig,v $

   Basic communication routines. 

   This module implements the basic communication between the 
   wish and SML. 

   $Date: 2001/03/30 13:39:04 $
   $Revision: 3.0 $

   Author: bu/stefan (Last modification $Author: 2cxl $)

   (C) 1996-99, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

signature COM =
    sig
        (* global names for communication primitives *)
	val commToTcl   : string
	val commToTcl'  : string
	val writeToTcl  : string
	val writeMToTcl : string 

        (* setting up the wish *)
	val initTcl     : unit -> unit
	val exitTcl     : unit -> unit
	val resetTcl    : unit -> unit 

        (* basic sending and receiving to/from the wish *)
	val getLine     : unit -> string
	val getLineM    : unit -> string
	val putLine     : string -> unit


        (* sending and receiving entities to/from the wish *)
	val putTclCmd           : string -> unit
	val readTclVal          : string -> string
        val readAnswerFromTcl   : (string -> unit) -> unit

    end

