structure Access : ACCESS = 
struct
 
   val init : unit -> int = 
       Unsafe.CInterface.c_function "TKSML" "init_tcl";
   (*  Clears EventCmd-Buffer, Creates TclInterpreter and makes the setup
       for its communication with SML *)

   val reset : unit -> unit = 
       Unsafe.CInterface.c_function "TKSML" "reset_tcl";
   (*  destroy's interpreter (if any) *)

   val eval : string -> unit = 
       Unsafe.CInterface.c_function "TKSML" "eval_tcl";
   (*  sends string to instance of TclInterpreter and forces its evaluation 
       via Tcl_Eval. May produce EventCmd's in EventCmd-Buffer *)

   val doOneEvent : unit -> int = 
       Unsafe.CInterface.c_function "TKSML" "run_tcl";
   (*  Processes one Event in Tk via calling Tk_DoOneEvent(0). This
       may produce EventCmd's in EventCmd-Buffer. If no event occured in
       the interface, doOneEvent will wait until the first non-empty event. 
       Postcond: EventCmd is nonempty.*)

   val doOneEvent_nowait : unit -> int = 
       Unsafe.CInterface.c_function "TKSML" "run_tcl_nowait";
   (*  Processes one Event in Tk via calling Tk_DoOneEvent(TCL_DONT_WAIT). This
       may produce EventCmd's in EventCmd-Buffer. If no event occured in the
       interface, doOneEvent_nowait will return. 
       doOneEvent_nowait may return because no event occured (return value 0)
       or because an empty-event occured (return value 1, EventCmd-Buffer 
       unchanged.*)

   val getEvent : unit -> string = 
       Unsafe.CInterface.c_function "TKSML" "get_event";
   (*  reads one Entry from EventCmd-Buffer - produces "" if there is none. *)



end;
