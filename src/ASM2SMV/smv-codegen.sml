structure SMV_CodeGen =
struct
  open IL

  type LOC_INFO = ASM2SMV.LOC_INFO
  structure ValSet = ASM2SMV.ValSet
  structure LocSet = ASM2SMV.LocSet

  exception Impossible of string
  fun impossible s = raise (Impossible s)

  val fnpair = Misc.fnpair
  val concat = List.concat

  structure S = SMV_AST
  structure V = ASM_Value

  (* --- flags to tune the code-generation --- *)

  val generate_consistency_conditions = ref true
  val generate_range_conditions       = ref true

  (*---------------------------------------------------------------------------------*)
  (*--ASM VALUE -> SMV CONSTANT -----------------------------------------------*)

  exception ValueNotAllowed of VALUE

  fun asm_value_to_smv_const (x :VALUE) :SMV_AST.CONSTANT =
    let open ASM_Value
    in case x of
	 BOOL true  => S.True
       | BOOL false => S.False
       | INT i      => S.Number (IntInf.toInt i)
       | _          => S.AtomConst ((SMV_ID.value2id x) handle _ => raise (ValueNotAllowed x))
    end


  (*---------------------------------------------------------------------------------*)
  (*--ASM LOCATION -> SMV TERM ------------------------------------------------*)

  fun loc_name (L :LOCATION) :string =    (* !!! this may need to be change in the final version !!! *)
    SMV_ID.location2id L           (* override the 'loc_name' from the 'Location' structure *)

  fun loc_to_DotTerm (L : LOCATION) : SMV_AST.TERM =
    S.Dot ("s",(loc_name L))

  fun loc_to_AtomTerm (L : LOCATION) : SMV_AST.TERM =
    S.AtomTerm (loc_name L)



  (*---------------------------------------------------------------------------------*)

  fun asmTerm2smvExpr (t :PE_TERM) :S.EXPR =
    case (!t) of
      Term t' => asmTerm2smvExpr' t'
    | Loc l   => S.Term (loc_to_DotTerm l)
    | Val v   => S.Const (asm_value_to_smv_const v)
    | Ctl (op_, t) => S.App (op_, [asmTerm2smvExpr t])
  and asmTerm2smvExpr' t' =
    case t' of
      CondTerm G_t_list => S.Case (map (fnpair (asmTerm2smvExpr, asmTerm2smvExpr)) G_t_list)
    | AppTerm (f, ref (Term (TupleTerm t_list))) => S.App (idStr f, map asmTerm2smvExpr t_list)
    | AppTerm (f, t) => S.App (idStr f, [asmTerm2smvExpr t])
    | _ => impossible "asmTerm2smvExpr"


  fun evaluable (t :PE_TERM) :bool =
    (asmTerm2smvExpr t; true) handle _ => false


  (* --- the following is needed because of a bug in SMV:
     --- SMV does not like 'case'-expressions within CTL formulae !!!  *)

  fun asmTerm2asmBoolTerm (t :PE_TERM) :PE_TERM = 
    case (!t) of
      Term t' => asmTerm2asmBoolTerm' t'
    | Loc l   => ref (Loc l)
    | Val v   => ref (Val v)
    | Ctl (op_, t) => ref (Ctl (op_, t))
  and asmTerm2asmBoolTerm' t' :PE_TERM = 
    case t' of
      CondTerm [] => IL.TRUE
    | CondTerm ((G,t)::rest) =>
        let val G_ = asmTerm2asmBoolTerm G
	in ifthenelse (G_, asmTerm2asmBoolTerm t, asmTerm2asmBoolTerm (ref (Term (CondTerm rest))))
        end
    | AppTerm (f as Id "=", ref (Term (TupleTerm (t_list as [t1, t2])))) =>
      ( case t2 of
	  ref (Val (BOOL true))  => t1
	| ref (Val (BOOL false)) => Not t1
	| _ => ref (Term (AppTerm (f, ref (Term (TupleTerm (map asmTerm2asmBoolTerm t_list)))))) )
    | AppTerm (f, ref (Term (TupleTerm t_list))) =>
        ref (Term (AppTerm (f, ref (Term (TupleTerm (map asmTerm2asmBoolTerm t_list))))))
    | AppTerm (f, t) =>
        ref (Term (AppTerm (f, asmTerm2asmBoolTerm t)))
    | _ => impossible "asmTerm2smvExpr"
  and ifthenelse (G, G1, G2) :PE_TERM =
    IL.Or (IL.And (G, G1), IL.And (IL.Not G, G2))

  fun bool_asmTerm2smvExpr (t :PE_TERM) :S.EXPR =
    asmTerm2smvExpr (asmTerm2asmBoolTerm t)


  (*----------------------------------------------------------------------------------*)
  (*-- declarations of state variables -----------------------------------------------*)

  fun functionKind f = valOf (ASM_Signature.functionKind (ASM_Signature.find (!ASM_Top.sign) f))
  fun functionType f = valOf (ASM_Signature.typeOf (ASM_Signature.find (!ASM_Top.sign) f))

  exception LocType
  fun loc_type ((f, x) :LOCATION) :ASM_Type.TYPE =
  ( (ASM_Type.range (functionType f))
    handle Option => raise LocType )

  exception RangeNotFinite of LOCATION
  fun var_decl_and_init (ts :(LOCATION * LOC_INFO) list) =
    let (* 'all_locations' includes locations of external function which occur somewhere,
	   while the domain of 'ts' only includes locations of dynamic functions !!! *)
	val all_locations =
          LocSet.UNION (map (ASM2SMV.collect_locations_in_term o #next o #2) ts)
        val internal_locations =    (* locations of dynamic functions *)
	  LocSet.fromList (map #1 ts)
        val external_locations =
	  LocSet.fromList ( List.filter
	    (fn (loc as (f,_)) => functionKind f = ASM_AST.External)
	    (LocSet.listItems all_locations) )
(*	  LocSet.difference (all_locations, internal_locations) *)
	fun type_of_i_loc (loc, loc_info :LOC_INFO) : SMV_AST.TYPE =
	  ( if (ASM_Type.isBoolType (loc_type loc))
	    then S.Boolean
	    else S.Enum (map asm_value_to_smv_const (ValSet.listItems (#loc_range loc_info))) )
	  handle ex as ValueNotAllowed _ => raise ex
	       | ASM0.LocRange loc => raise RangeNotFinite loc
	fun type_of_e_loc loc : SMV_AST.TYPE =
	  ( if (ASM_Type.isBoolType (loc_type loc))
	    then S.Boolean
	    else S.Enum (map asm_value_to_smv_const (ASM0.loc_range loc)) )
	  handle ex as ValueNotAllowed _ => raise ex
	       | ASM0.LocRange loc => raise RangeNotFinite loc
	fun init_list (ts :(LOCATION * LOC_INFO) list) =
	  case ts of
	    [] => []
	  | (loc, loc_info) :: rest =>
	      ( S.LhsInit (loc_to_AtomTerm loc),  (*(loc_to_DotTerm loc)*)
		S.Const (asm_value_to_smv_const (#init loc_info)) ) :: (init_list rest)
    in [ S.Var (
           ( map (fn loc => (loc_to_AtomTerm loc, type_of_e_loc loc))
	      (LocSet.listItems external_locations) ) @
           ( map (fn (loc,loc_info) => (loc_to_AtomTerm loc, type_of_i_loc (loc,loc_info)))
	      ts ) ),
         S.Assign (init_list ts) ]
    end


  (*----------------------------------------------------------------------------------*)
  (*--  'next'-assignments + consistency conditions + range conditions ---------------*)

  fun transition_list (ts :(LOCATION * LOC_INFO) list) =
    let fun next (loc, loc_info :LOC_INFO) =
	  ( S.LhsNext (loc_to_DotTerm loc),
	    asmTerm2smvExpr (#next loc_info) )
(*
	fun eqns (loc, loc_info :LOC_INFO) =
	  let fun one_equation (nodename, rhs) = ( "_" ^ nodename, asmTerm2smvExpr rhs )
	  in map one_equation (#eqns loc_info)
	  end
*)
	fun cc (loc, loc_info :LOC_INFO) =   (* consistency condition *)
	  S.CTL_A (S.CTL_G (S.Expr (bool_asmTerm2smvExpr (#cc loc_info))))
	fun rc (loc, loc_info :LOC_INFO) =   (* range condition *)
	  let val out_of_range =
	        ValSet.difference (#loc_range loc_info, ValSet.fromList (ASM0.loc_range loc))
	      val cond =
	        if ValSet.isEmpty out_of_range
		then S.Const S.True
		else bool_asmTerm2smvExpr
		       (IL.Not (IL.OR ( map (fn v => ASM0.eq (ref (Loc loc), ref (Val v)))
				        (ValSet.listItems out_of_range) )))
          in S.CTL_A (S.CTL_G (S.Expr (cond)))
	  end
    in concat ( map (fn (loc, loc_info) =>
(*                  [ S.Define (eqns (loc, loc_info)) ] @ *)
		  [ S.Assign [next (loc, loc_info)] ]
		  @ ( if (!generate_consistency_conditions)
		      then [ S.Spec (cc (loc, loc_info)) ]
		      else [] )
                  @ ( if (!generate_range_conditions)
		      then [ S.Spec (rc (loc, loc_info)) ]
		      else [] )) ts )
    end


  (*----------------------------------------------------------------------------------*)
  (*--  invariants  ------------------------------------------------------------------*)

  fun invariant_list (invs :PE_TERM list) =
    let fun one_inv phi = S.CTL_A (S.CTL_G (S.Expr (bool_asmTerm2smvExpr phi)))
    in map (S.Spec o one_inv) invs
    end


  (*----------------------------------------------------------------------------------*)
  (*--  main code generation function  ("putting all together")  ---------------------*)

  fun prt t = Pretty.pr TextIO.stdOut 1000 (IL_Pretty.pe_term t)

  fun transform (R :PE_RULE, invariants :PE_TERM list) =
    let val ts = ASM2SMV.transition_system R
(*
      fun prt_loc (loc, { cc = cc, next = next, changed = changed, ... } :LOC_INFO) =
        ( print "-- Location ";
	  prt (ref (IL.Loc loc));
	  print "-- Next\n";
	  prt next;
	  print "-- Changed when:\n";
	  prt changed;
	  print "-- Consistency condition\n";
	  prt cc;
(*	  print "-- Range\n";
	  map (fn v => print (ASM.show_value v ^ " ")) range;*)
	  print "\n\n" )
       val _ = map prt_loc ts
*)
    in S.Program(
	 [ S.Module ( "state", [], var_decl_and_init ts ) ] @
	 [ S.Module ( "behavior", ["s"], transition_list ts) ] @
	 ( case invariants of
	     [] => []
           | _  => [ S.Module ( "invariants", ["s"], invariant_list invariants ) ] ) @

	 (* --- main module --- *)

	 [ S.Module ( "main", [],
	   [ S.Var ( [ (S.AtomTerm "s", S.ModType ("state", [])),
		       (S.AtomTerm "b", S.ModType ("behavior", [ S.Term (S.AtomTerm "s") ])) ] @
                     ( case invariants of
		         [] => []
		       | _  => [ (S.AtomTerm "inv", S.ModType ("invariants", [ S.Term (S.AtomTerm "s") ])) ] ) ) ] ) ]
      )
    end
end
