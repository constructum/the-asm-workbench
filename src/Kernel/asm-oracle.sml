(*
## "asm-oracle.sml", G. Del Castillo, Jul 1998 - Apr 2001
##
##   
*)


structure ASM_Oracle =
struct
  structure Parse  = ASM_Parser

  datatype PROBLEM =
    Impossible of string
  | NoOracleProcess of string
  | ParseError
  | TypeError of exn
  | EvalError
  | WrongType of ASM_Type.TYPE * ASM_Type.TYPE

  exception Error of PROBLEM Error.ERROR

  val replace = String_.replace

  fun message (problem) =
    case problem of
      Impossible s =>
	replace "Impossible! This should not happen! (Probably a bug) [$1]" [s]
    | NoOracleProcess s => 
	replace "Oracle process ('$1') not found or unreadable" [ s ]
    | ParseError => 
	replace "Parse error in oracle response" []
    | TypeError (ASM_Check.Error cont) => 
	replace "Type-check error in oracle response:\n$1" [ Error.message cont ]
    | TypeError _ =>
	"Type-check error in oracle response"
    | EvalError => 
	replace "Evaluation error in oracle response (probably a bug)" []
    | WrongType (T1, T2) => 
	replace "Oracle response has wrong type ($1 instead of $2)"
		  (* (ASM_Value.toString x) @ *) (map ASM_Type.toString [ T1, T2 ])


  fun error (fct :string) (what :PROBLEM) =
    raise Error { module = "ASM_Oracle", function = fct, problem = what, message = message, cause = NONE }

  fun impossible (fct :string) (s :string) =
    error fct (Impossible s)


  val to_oracle   = ref TextIO.stdOut
  val from_oracle = ref TextIO.stdIn


  val timeout = ref 1000
  exception TimeOut
  fun timeout_msg () =
  ( "Oracle timeout error.\n\
    \  Please try restarting the oracle and/or\n\
    \  increasing the timeout value (current value = "
    ^ (Int.toString (!timeout)) ^ ")\n" )
  fun with_timeout t f =
    let val old_timeout = !timeout
    in timeout := t;
       f () handle ex => (timeout := old_timeout; raise ex);
       timeout := old_timeout
    end

  fun wait_until condition =
    let open Timer
        val start = startRealTimer ()
	fun repeat () =
	  if condition () then ()
	  else let val m = Real.floor (Time.toReal (checkRealTimer start))
	       in if m >= (!timeout) then raise TimeOut else repeat ()
	       end
    in repeat ()
    end


  fun no_oracle () =
    to_oracle := TextIO.openOut "/dev/null"

  val oracle_process_started = ref false

  fun start_oracle oracle_name =
    let val _ = if not (File.exists oracle_name)
                then error "start_oracle" (NoOracleProcess oracle_name)
		else ()
        val oracle_proc = Unix.execute (oracle_name, [ ])
			  handle IO.Io _ => error "start_oracle" (NoOracleProcess oracle_name)
        val (p_out, p_in) = Unix.streamsOf oracle_proc
        val _ = (oracle_process_started := true)
    in from_oracle := p_out;
       to_oracle   := p_in
       (* possibly an initialization sequence for the oracle process *)
    end


  fun send_oracle_command (cmd, contents) =
    let open TextIO
    in output (!to_oracle, cmd ^ " " ^ contents ^ "\n\n");
       flushOut (!to_oracle)
    end



  (* note: the response of the oracle is ended by two newlines or EOF *)
  fun read_oracle_response () =
    let open TextIO
        fun read () =
	( case input1 (!from_oracle) of
            NONE => []
          | SOME #"\n" =>
            ( case input1 (!from_oracle) of
                NONE       => [ "\n" ]
              | SOME #"\n" => [ "\n" ]
              | SOME c     => "\n" :: (Char.toString c) :: (read ()) )
          | SOME c => (Char.toString c) :: (read ()) )
        val result = String.concat (read ())
    in (* output (stdErr, result ^ "\n"); *)
       result
    end


  fun quit_oracle () =
    let open TextIO
    in if (!oracle_process_started) then output (!to_oracle, "@QUIT\n\n") else ();
       flushOut (!to_oracle);
       closeOut (!to_oracle) handle IO.Io _ => ();
       closeIn (!from_oracle) handle IO.Io _ => ();
       oracle_process_started := false
    end


  val show_value = ASM_Value.toString
  val STRING	 = ASM_Value.STRING


  val retry_limit = ref 5



  val parseTerm :(string -> ASM_AST.TERM) ref =
    ref ( fn _ => impossible "ASM_Oracle.parseTerm" "still undefined [bug]" )

  val checkTerm :(ASM_AST.TERM -> ASM_AST.TYPE) ref =
    ref ( fn _ => impossible "ASM_Oracle.checkTerm" "still undefined [bug]" )

  val typealias :(unit -> ASM_Type.GET_TYPEALIAS) ref =
    ref ( fn _ => impossible "ASM_Oracle.typealias" "still undefined [bug]" )

  val evalTerm :(ASM_AST.TERM -> ASM_Value.VALUE) ref =
    ref ( fn _ => impossible "ASM_Oracle.evalTerm" "still undefined [bug]" )


  fun consult_oracle_process (expected_type :ASM_Type.TYPE)
                             (function_name, argument) =
    let open TextIO
        fun err what = error "consult_oracle_process" what
        val query_contents = (show_value (STRING function_name)) ^ " " ^ (show_value argument)
	fun fetch_result () =
	  let (*val _ = (output (stdErr, "[ reading, "); flushOut stdErr)*)
	      val s = read_oracle_response ()
	      (*val _ = (output (stdErr, "parsing: '"^(s)^"', "); flushOut stdErr)*)
	      val t = (!parseTerm) (s)  handle _ => err ParseError
	      (*val _ = (output (stdErr, "type-checking, "); flushOut stdErr)*)
	      val actual_type = ( ((!checkTerm) t)
				  handle ex => err (TypeError ex) )
	      val _ = (ASM_Type.unify ((!typealias)()) (expected_type, actual_type))
		      handle ASM_Type.IncompatibleType _ => err (WrongType (actual_type, expected_type))
	      (*val _ = (output (stdErr, "evaluating "); flushOut stdErr)*)
	      val result = ((!evalTerm) t)  handle _ => err (EvalError)
	      (*val _ = (output (stdErr, "]\n"); flushOut stdErr)*)
          in result
	  end
	fun err_msg ex =
	  case ex of
	    Error what => Error.message what
	  | _ => raise ex
	fun retry ex n =
	  if n > 0
	  then ( send_oracle_command ("@RETRY", show_value (STRING (err_msg ex)) ^ " " ^ query_contents);
	         fetch_result () ) handle ex => retry ex (n-1)
	  else ASM_Value.UNDEF
	fun query () =
	( send_oracle_command ("@QUERY", query_contents);
	  fetch_result () ) handle ex => retry ex (!retry_limit)
	val result = query ()
    in (* output (stdErr, "result = " ^ (show_value result) ^ "\n");
	  flushOut stdErr; *)
       result
    end
end
