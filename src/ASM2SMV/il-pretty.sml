(*
##
## ASM2SMV - G. Del Castillo, K. Winter, 1999
##
## "il-pretty.ml", pretty-printer for intermediate language
##
##
*)

(* ---------------------------------------------------------------- *)
(* ---------------------------------------------------------------- *)

signature IL_PRETTY =
sig
  val pe_term :IL.PE_TERM -> Pretty.t
  val pe_rule :IL.PE_RULE -> Pretty.t
end


structure IL_Pretty :IL_PRETTY =
struct
  open IL

  fun app_term' (f, t) =
    ASM_Pretty.app (ASM.topContext ())
      ( fn ref (Term (ASM_AST.TupleTerm e_list)) => SOME e_list
	 | ref (Val (ASM_Value.TUPLE e_list)) => SOME (map (ref o Val) e_list)
	 | _ => NONE )
    pe_term (f, t)

  and pe_term (t :PE_TERM) :Pretty.t =
    let open Pretty
	fun term' t' =
          let val ctx = ASM.topContext ()
          in ASM_Pretty.term' ctx
               (ASM_Pretty.type_, ASM_Pretty.name, ASM_Pretty.patt ctx, pe_term) app_term' t'
          end
    in case t of
	 ref (Term t') => term' t'
       | ref (Val v)   => str (ASM_Value.toString v)
       | ref (Loc (f,v)) => app_term' (Id f, ref (Val v))
       | ref (Ctl (op_,t)) => blo (0, [ str op_, str " (", pe_term t, str ")" ])
		      (* locations are printed like fct applications *)
                      (* blo (0, [ str f, str " (", str (ASM.show_value v), str ")" ]) *)
    end

  fun pe_rule (ref (Rule R')) =
    let fun rule' R' =
          let val ctx = ASM.topContext ()
          in ASM_Pretty.rule' (ASM.topContext ()) false
	      (ASM_Pretty.name, ASM_Pretty.patt ctx, pe_term, pe_rule)
	      (fn t => ref (Term t))
              app_term' R'
          end
    in rule' R'
    end
end
