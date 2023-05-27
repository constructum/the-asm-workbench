signature ERROR =
sig
  type 'a ERROR =
  { module   : string,
    function : string, 
    problem  : 'a, 
    message  : 'a -> string,
    cause    : exn option }

  exception Exit
  val debug   :bool ref
  val reraise :bool ref
  val message :'a ERROR -> string
  val report  :'a ERROR -> 'b
end


structure Error :ERROR =
struct
  type 'a ERROR =
  { module   : string,
    function : string, 
    problem  : 'a, 
    message  : 'a -> string,
    cause    : exn option }

  val debug   = ref true
  val reraise = ref true

  exception Exit

  fun message ({ problem, message, cause, module, function } :'a ERROR) =
    message problem

  fun report ({ problem, message, cause, module, function } :'a ERROR) =
    let open TextIO
    in if (!debug) then output (stdErr, String_.replace "In $1.$2:\n" [ module, function ]) else ();
       output (stdErr, message problem);
       case cause of
         SOME ex => if (!reraise) then raise ex else (print "raise Exit\n"; raise Exit)
       | NONE    => raise Exit
    end
end
