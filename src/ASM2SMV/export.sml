(*
use "load_all";
*)

structure ASM2SMV_Export =
struct

fun ASM2SMV_Usage () =
( print
    "Usage: asm2smv [OPTION]... -files FILE...\n\
    \                           -program RULE\n\
    \                           [-invariants FILE...]\n\n\
    \Options:\n\
    \  -asm0             transform program into ASM0 form\n\
    \  -smv              translate program and properties to SMV\n\
    \\n\
    \SMV code generation options:\n\
    \  -no_cc            do not generate consistency conditions\n\
    \  -no_rc            do not generate range conditions\n\
    \\n\n\
    \Please report bugs to giusp@uni-paderborn.de\n" )


fun ASM2SMV_Init () =
( ASM.reset () )


fun printErr s = TextIO.output (TextIO.stdErr, s)

local
  exception Exit
  fun args_to_next_option L =
    let fun F (L1, []) = (L1, [])
          | F (L1, L2 as (x :: xs)) =
             case (explode x) of
               #"-" :: _ => (L1, L2)
             | _         => F (L1 @ [ x ], xs)
    in F ([], L)
    end

  fun load_files filenames =
    let open TextIO
        fun message s = (output (stdErr, s); flushOut stdErr)
        fun load_file s =
        ( message (" " ^ s);
          (ASM.loadFile' s)
            handle ex => (output (stdErr, "\n\n" ^ (ASM.errorMessage ex)); raise ex) )
    in message "Loading:";
       map load_file filenames;
       message "\n"
    end 

  datatype OutputType =
    ASM0_Output
  | SMV_Output

  val output_type     = ref SMV_Output
  val program	      = ref ""
  val invariant_files = ref ([] :string list)
in
  fun ASM2SMV_CmdLineInterface (progName, args) =
    let open Main
        fun error () = ( ASM2SMV_Usage (); raise Exit )
        fun skip_first_arg [] = []
          | skip_first_arg (x::xs) = xs
        fun set_output_type otype =
          output_type := otype
        fun set_options args =
          case args of
            "-asm0" :: rest         => (set_output_type ASM0_Output; set_options rest)
          | "-smv" :: rest          => (set_output_type SMV_Output; set_options rest)
	  | "-no_cc" :: rest	    => ( SMV_CodeGen.generate_consistency_conditions := false;
				         set_options rest )
	  | "-no_rc" :: rest	    => ( SMV_CodeGen.generate_range_conditions := false;
				         set_options rest )
          | _                       => args
        fun cmdline_ok args =
          let fun F (state, args) =
            case state of
              1 => (case args of "-files" :: rest => F (2, #2 (args_to_next_option rest)) | _ => false)
            | 2 => (case args of "-program" :: x :: rest => F (3, rest) | _ => false)
            | 3 => (case args of [] => true | ("-invariants" :: _) => true | _ => false)
            | _ => false
          in F (1, args)
          end
        fun process_cmdline_arguments args =
          case args of
            "-files" :: rest =>
              let val (filenames, rest) = args_to_next_option rest
              in load_files filenames;
                 process_cmdline_arguments rest
              end
          | "-program" :: rule :: rest =>
            ( program := rule;    (*ASM2SMV rule;*)
              process_cmdline_arguments rest )
          | "-invariants" :: rest =>
	     invariant_files := rest
          | _ => ()
    in ASM2SMV_Init ();
       let val cmdline_rest = set_options args
       in if cmdline_ok cmdline_rest
          then ( process_cmdline_arguments cmdline_rest;
		 case (!output_type) of
		   ASM0_Output =>
		     ASM0 (!program)
		 | SMV_Output  =>
                     ASM2SMV_Error.catch ASM2SMV (!program, map readfile (!invariant_files)) )
          else error ()
       end handle ex as IO.Io _ => printErr (ASM.errorMessage ex)
                | _ => ();
(*     ASM.shutdown (); *)
       OS.Process.success
    end handle _ => OS.Process.failure
end


fun export () =
    SMLofNJ.exportFn ( valOf (OS.Process.getEnv "ASM_WB_HOME") ^ "/bin/asm2smv",
                       ASM2SMV_CmdLineInterface )

end (* structure ASM2SMV_Export *)
