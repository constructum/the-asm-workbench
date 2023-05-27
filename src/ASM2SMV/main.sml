(*----------------------------------------------------------------------------------*)
(*----------------------------------------------------------------------------------*)

structure Main =
struct
  exception Readfile
  fun readfile filename =
    let open TextIO
	val f   = openIn filename
(*	val len = valOf (canInput (f, valOf (Int.maxInt))) *)
	val res = inputAll f
	val _   = closeIn f
    in res
    end


  fun message s = let open TextIO in (output (stdErr, s); flushOut stdErr) end

  fun print_pe_term t  = Pretty.pr TextIO.stdOut 95 (IL_Pretty.pe_term t)
  fun print_pe_rule R  = Pretty.pr TextIO.stdOut 95 (IL_Pretty.pe_rule R)
  fun print_smv_prog P = Pretty.pr TextIO.stdOut 95 (SMV_Pretty.proc_ P)

  fun expand_rule_macros R =
    ASM_Expand.namedRulesFully (!ASM_Top.dict) R

  fun expand_derived_functions_in_term t =
    ASM_Expand.derivedFunctionsInTermFully (!ASM_Top.dict) t

  fun expand_derived_functions_in_rule R =
    ASM_Expand.derivedFunctionsInRuleFully (!ASM_Top.dict) R


  fun prepare_program s =
    let val P0 = ASM.catch ASM.parseRule s
	val _  = ASM.catch ASM.checkRule P0
	val P1 = ASM2SMV_Error.catch expand_rule_macros P0
	val P2 = ASM2SMV_Error.catch expand_derived_functions_in_rule P1
    in P2
    end

  fun prepare_formula s =
    let val phi = ASM.parseTerm s
	val typ = ASM.checkTerm phi
	val _  = if not (ASM_Type.isBoolType typ)
		 then message ("Error: specified invariant is not a boolean term!\n")
		 else ()
	val phi' = ASM2SMV_Error.catch expand_derived_functions_in_term phi
    in phi'
    end

  fun ASM0 (s :string) =
    let val P  = prepare_program s
	val _  = ASM0.init ()
	val _  = message "Unfolding...\n"
	val R  = ASM2SMV_Error.catch (ASM0.unfold_rule o IL.pe_rule) P
	val _  = message "Pretty printing...\n\n"
	val _  = ASM2SMV_Error.catch print_pe_rule R
    in ()
    end

  fun transform_formula (t :IL.PE_TERM) :IL.PE_TERM =
    let val t' = IL.simplify_term t
    in (*if SMV_CodeGen.evaluable t'
       then t'
       else*) ASM0.unfold_term t'
    end

  fun ASM2SMV (program :string, invariants :string list) =
    let val P    = prepare_program program
	val phis = map prepare_formula invariants
	val _    = ASM0.init ()
	val _    = message "Unfolding program...\n"
	val R    = ASM2SMV_Error.catch (ASM0.unfold_rule o IL.pe_rule) P
  (*      val R    = Misc.timing (fn () => ASM2SMV_Error.catch (ASM0.unfold_rule o IL.pe_rule) P) () *)
	val _    = message "Unfolding invariants...\n"
	val invs = map (ASM2SMV_Error.catch (transform_formula o IL.pe_term)) phis
	val _    = message "Translating to SMV...\n"
	val smvp = ASM2SMV_Error.catch SMV_CodeGen.transform (R, invs)
  (*      val smvp = Misc.timing (fn () => ASM2SMV_Error.catch SMV_CodeGen.transform (R, invs)) () *)
	val _    = message "Pretty printing...\n\n"
	val _    = ASM2SMV_Error.catch print_smv_prog smvp
    in ()
    end
end
