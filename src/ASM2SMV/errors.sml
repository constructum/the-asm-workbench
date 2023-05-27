structure ASM2SMV_Error =
struct
  val replace = String_.replace

  exception ASM2SMV_Error

  fun show_location (s, value) =
    case value of
      ASM_Value.TUPLE [] => s
    | ASM_Value.TUPLE _  => s ^ " " ^ (ASM_Value.toString value)
    | x	=> s ^ " (" ^ (ASM_Value.toString value) ^ ")"

  fun msg ex =
    let val prefix = "ASM2SMV Error. "
	val msg_text =
	( case ex of
	    ASM0.NotImplemented s => "[ASM0]  Not implemented: " ^ s
	  | ASM0.Impossible s	  => "[ASM0]  Impossible: " ^ s
	  | ASM0.LocRange (loc as (f, _)) =>
	      replace "Range of location '$1' is not finite\n\
		      \  (please specify finiteness constraint for '$2')."
		      [ show_location loc, f ]
	  | ASM0.EmptyLocationRange (loc as (f,_)) =>
	      replace "Range of location '$1' is empty\n\
		      \  (please check finiteness constraint for '$2')."
		      [ show_location loc, f ]
	  | ASM2SMV.NotImplemented s => "[ASM2SMV]  Not implemented: " ^ s
	  | ASM2SMV.PossibleInconsistency loc => ( "[ASM2SMV]  Possible inconsistency (location "
						   ^ (show_location loc) ^ ")" )
(*	  | ASM2SMV.X		     => "[ASM2SMV]  X" *)
	  | IL.NotImplemented s	     => "[IL]  Not implemented: " ^ s
	  | IL.GetVal		     => "[IL]  GetVal"
(*        | ASM_Expand.RecursiveDerivedFunction s =>
	      replace "Recursive definition of derived function ('$1') can not be expanded" [ s ]
*)
          | SMV_CodeGen.Impossible s => "[SMV_Codegen]  Impossible: " ^ s
          | SMV_CodeGen.ValueNotAllowed x =>
	      replace "ASM value not allowed ($1):\n\
		      \  (no translation to SMV defined for this value)\n"
		      [ ASM_Value.toString x ]
          | SMV_CodeGen.RangeNotFinite (loc as (f,_)) =>
	      replace "Range of location '$1' is not finite\n\
		      \  (please specify finiteness constraint for '$2')."
                      [ show_location loc, f ]
          | SMV_CodeGen.LocType => "[SMV_Codegen]  LocType"
          | ASM2SMV.LocInit => "[ASM2SMV]  LocInit"

          | SMV_Pretty.ExprPrint_Error _ => "[SMV_Pretty]  Badly formed SMV expression"

	  | _ => raise ex )

(*
            IL.NotSupported s =>
	      replace ("$1: not supported by ASM2SMV", [ s ])
          | IL.NotImplemented s =>
	      replace ("$1: not implemented yet in ASM2SMV", [ s ])
	  | IL.ApplyPrimitiveOp (s, args) =>
	      replace ( "Illegal application of an SMV operator (ASM2SMV BUG): '$1($2)'",
		        [ s, Misc.List.output (map ASM_Value.toString args, ",") ] )
	  | IL.NotPrimitiveOp s =>
	      replace ( "Not a primitive operator (ASM2SMV BUG): '$1'", [ s ] )
	  | IL.EmptyLocationRange (loc as (f,_)) =>
	      replace ( "Range of location '$1' is empty\n\
		        \  (please check finiteness constraint for '$2').",
		        [ show_location loc, f ] )
	  | IL.LocRange (loc as (f, _)) =>
	      replace ( "Range of location '$1' is not finite\n\
		        \  (please specify finiteness constraint for '$2').",
		        [ show_location loc, f ] )
	  | IL.LocNext      => ("[IL.LocNext] (?)")
	  | IL.RangeOfExpr  => ("[IL.RangeOfExpr] (?)")
	  | IL.LocNextRange => ("[IL.LocNextRange] (?)")
	  | IL.Collect      => ("[IL.Collect] (?)")
          | IL.Impossible s => ("Impossible: " ^ s)
          | ASM2SMV.ValueNotAllowed x =>
	      replace ( "ASM value not allowed ($1):\n\
		        \  (no translation to SMV defined for this value)\n",
		        [ ASM_Value.toString x ] )
          | ASM2SMV.RangeNotFinite (loc as (f,_)) =>
	      replace ( "Range of location '$1' is not finite\n\
		        \  (please specify finiteness constraint for '$2').",
		        [ show_location loc, f ] )
          | ASM2SMV.loc_initComp_Error =>
	      "Nothing to check (no locations!)."
	  | ASM2SMV.il_expr_to_smv_expr_Error s =>
	      replace ( "Unexpected case in function 'ASM2SMV.il_expr_to_smv_expr': $1", [ s ] )
          | ASM_Expand.RecursiveDerivedFunction s =>
	      replace ( "Recursive definition of derived function ('$1') can not be expanded", [ s ] )
          | _ => "Unrecognized error"
*)
    in (prefix ^ msg_text ^ ".\n")
    end


  fun catch f x =
    (ASM.catch (fn () => (f x)) ())
    handle ex => (TextIO.output (TextIO.stdErr, msg ex); raise ex)

end
