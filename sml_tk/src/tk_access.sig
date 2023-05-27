signature ACCESS =
  sig

   val init     : unit -> int
   (* Clears EventCmd-Buffer, Creates instance of TclInterpreter and 
      makes the setup for its communication with SML *)

   val reset    : unit -> unit
   (* destroy's instance of interpreter (if any) *)

   val eval  : string -> unit
   (* sends string to instance of TclInterpreter and forces its evaluation 
      via Tcl_Eval. May produce EventCmd's in EventCmd-Buffer *)

   val doOneEvent : unit -> int
   (*  Processes one Event in Tk via calling Tk_DoOneEvent(0). This
       may produce EventCmd's in EventCmd-Buffer. If no event occured in
       the interface, doOneEvent will wait until the first non-empty event. 
       Postcond: EventCmd is nonempty.*)

   val doOneEvent_nowait : unit -> int 
   (*  Processes one Event in Tk via calling Tk_DoOneEvent(TCL_DONT_WAIT). This
       may produce EventCmd's in EventCmd-Buffer. If no event occured in the
       interface, doOneEvent_nowait will return. 
       doOneEvent_nowait may return because no event occured (return value 0)
       or because an empty-event occured (return value 1, EventCmd-Buffer 
       unchanged.*)

   val getEvent : unit -> string
   (* reads one Entry from EventCmd-Buffer. Produces "" if empty. *)


  end;
