(* ******************************************************************* *\
 *
 *   The ASM Workbench for SML/NJ 1.10
 *
 *   File:         asm-symbolic.sml
 *
 *   Description: 
 *     Some useful symbolic computation on ASM-SL structures
 *
 *   Author:       $Author: giusp $
 *   Date:         $Date: 2001/03/24 01:12:16 $
 *   Revision:     $Revision: 1.7 $
 *
\* ******************************************************************* *)


(* ******************************************************************* *)
(*                                                                     *)
(*    collect variables from patterns/terms                            *)
(*                                                                     *)
(* ******************************************************************* *)

structure ASM_Collect =
struct
  structure VarSet =
  struct
    open StringSet
    val Union = List.foldl union empty
  end

  type VAR_SET = VarSet.set

  infix U
  fun set1 U set2 = VarSet.union (set1, set2)
  fun union3 (set1, set2, set3) = set1 U set2 U set3
  fun union4 (set1, set2, set3, set4) = set1 U set2 U set3 U set4

  structure CollectVarsInPattSpec :PATT_ALGEBRA = struct
    structure S = ASM_AST

    structure T = struct
      type TYPE = ASM_AST.TYPE
      type NAME = ASM_AST.NAME

      type PATT  = VarSet.set
      type PATT' = PATT
    end

    val (h_type, h_name) = (Misc.id, Misc.id)

    fun h_patt patt' (ASM_AST.PattPos (_, p')) = patt' p'

    fun PattType (T, p) = p
    val Placeholder    = VarSet.empty
    fun VarPatt x      = VarSet.singleton x
    fun AppPatt (f, p) = p
    fun TuplePatt ps   = VarSet.Union ps
  end

  structure CollectVarsInPatt = PattInduction (CollectVarsInPattSpec)

  fun variablesInPatt p = CollectVarsInPatt.patt p


  structure CollectVarsInTermSpec = struct
    structure S = ASM_AST
    structure T = struct
      open CollectVarsInPattSpec.T
      type TERM  = VarSet.set
      type TERM' = TERM
    end
    val (h_type, h_name, h_patt) = (Misc.id, Misc.id, variablesInPatt)

    fun h_term term' (ASM_AST.TermPos (_, t')) = term' t'

    fun TermType (_, t) = t
    fun VarTerm v = VarSet.singleton v
    fun AppTerm (_, t) = t
    fun TupleTerm ts = VarSet.Union ts
    fun CondTerm Gts = VarSet.Union (map (fn (G, t) => G U t) Gts)
    fun CaseTerm (p, pts) = p U VarSet.Union (map (fn (p, t) => p U t) pts)
    fun LetTerm (p, t, t') = p U t U t'
    fun ListComprTerm (t, qual) = t U union3 qual
    fun FSetComprTerm (t, qual) = t U union3 qual
    fun FMapComprTerm ((t,t'), qual) = (t U t') U union3 qual
    fun ForallTerm qual = union3 qual
    fun ExistsTerm qual = union3 qual
    fun FSetEnumTerm ts = VarSet.Union ts
    fun FMapEnumTerm tts = VarSet.Union (map (fn (t, t') => t U t') tts)
  end

  structure CollectVarsInTerm = TermInduction (CollectVarsInTermSpec)

  fun variablesInTerm t = CollectVarsInTerm.term t
end



(* ******************************************************************* *)
(*                                                                     *)
(*    expand                                                           *)
(*                                                                     *)
(* ******************************************************************* *)


structure ASM_Replace =
struct
  open ASM_AST
  structure VarSet = ASM_Collect.VarSet


  (* --- substitutions: maps from variables to terms --- *)

  structure Subst =
  struct
    structure ExtdTable = struct
      open StringMap
      fun override (m1, m2) = unionWith #2 (m1, m2)
      fun Override ms = List.foldl override empty ms
      fun fromList L  = List.foldl insert' empty L
    end
    open ExtdTable
    type SUBST = TERM map
  end

  type SUBST   = Subst.SUBST
  type VAR_SET = ASM_Collect.VAR_SET

  val (empty :SUBST) = Subst.empty

  exception NameClash of string

  structure ReplaceInTermSpec = struct
    structure S = ASM_AST
    structure T = struct
      type TYPE = ASM_AST.TYPE
      type NAME = ASM_AST.NAME
      type PATT = ASM_AST.PATT
      type TERM' = ASM_AST.TERM (*(ASM_AST.TYPE, ASM_AST.NAME, ASM_AST.PATT, ASM_AST.TERM) ASM_AST.TERM'*)
      type TERM  = ASM_AST.TERM

      type QUALIFIER = ASM_AST.PATT * ASM_AST.TERM * (ASM_AST.TERM -> ASM_AST.TERM)
    end
    structure CTX = struct
      type TYPE = unit
      type NAME = unit
      type PATT = unit
      type TERM' = (SUBST * ASM_Collect.VAR_SET StringMap.map) * VAR_SET
      type TERM  = TERM'
      fun term2name _ = ()
      fun term2type _ = ()
      fun term2patt _ = ()
    end

    fun replace_var (S :SUBST, vars_in_S :ASM_Collect.VAR_SET StringMap.map)
		    (BV :ASM_Collect.VAR_SET) s =
      if VarSet.member (BV, s)
      then TermPos (NONE, VarTerm s)
      else let val t = valOf (Subst.find (S, s))
	   in if VarSet.numItems (VarSet.intersection (BV, valOf (StringMap.find (vars_in_S, s)))) = 0
	      then t
	      else raise NameClash s
	   end
	   handle Option => TermPos (NONE, VarTerm s)   (* s is not in dom(S) *)

    fun h_type T _ = T
    fun h_name f _ = f
    fun h_patt p _ = p

    fun add_vars_to BV (p, t) = VarSet.union (BV, ASM_Collect.variablesInPatt p)

    fun bind ((S, vars_in_S), BV) (p, t) = ((S, vars_in_S), add_vars_to BV (p, t))

    fun qualifier (p, A, G_) _ = (p, A, G_)

    fun getTerm' (TermPos (_, t')) = t'
    fun mkTerm t' = TermPos (NONE, t')

    fun h_term term' (TermPos (newPos, t')) BV =
      TermPos (newPos, getTerm' (term' t' BV))

    fun TermType (T, t) _ = t
    fun VarTerm v ((S, vars_in_S), BV) = replace_var (S, vars_in_S) BV v
    fun AppTerm (f, t) _ = mkTerm (ASM_AST.AppTerm (f, t))
    fun CondTerm Gts _   = mkTerm (ASM_AST.CondTerm (List.map (fn (G_, t_) => (G_(), t_())) Gts))
    fun TupleTerm ts _   = mkTerm (ASM_AST.TupleTerm ts)

    fun LetTerm (p, t, t') _ = mkTerm (ASM_AST.LetTerm (p, t, t'))
    fun CaseTerm (t, pts) _  = mkTerm (ASM_AST.CaseTerm (t, map (fn (p,t_)=>(p,t_())) pts))

    fun F_qual (p, A, G_) = (p, A, G_ ASM_AST.trueTerm)
    fun ctx' ctx (qual :T.QUALIFIER) = bind ctx (#1 qual, #2 qual)

    fun ListComprTerm (t_, qual) ctx = mkTerm (ASM_AST.ListComprTerm (t_ (ctx' ctx qual), F_qual qual))
    fun FSetComprTerm (t_, qual) ctx = mkTerm (ASM_AST.FSetComprTerm (t_ (ctx' ctx qual), F_qual qual))
    fun FMapComprTerm ((t_,t_'), qual) ctx =
      mkTerm (ASM_AST.FMapComprTerm ((t_ (ctx' ctx qual), t_' (ctx' ctx qual)), F_qual qual))

    fun ForallTerm qual _ = mkTerm (ASM_AST.ForallTerm (F_qual qual))
    fun ExistsTerm qual _ = mkTerm (ASM_AST.ExistsTerm (F_qual qual))

    fun FSetEnumTerm ts _ = mkTerm (ASM_AST.FSetEnumTerm ts)
    fun FMapEnumTerm ts _ = mkTerm (ASM_AST.FMapEnumTerm ts)
  end

  structure ReplaceInTerm = TermTransf (ReplaceInTermSpec)


  structure ReplaceInRuleSpec = struct
    structure S = ASM_AST
    structure T = struct
      open ReplaceInTermSpec.T
      type RULE' = ASM_AST.RULE
      type RULE  = ASM_AST.RULE
    end
    structure CTX = struct
      open ReplaceInTermSpec.CTX
      type RULE' = (SUBST * ASM_Collect.VAR_SET StringMap.map) * VAR_SET
      type RULE  = RULE'
      fun rule2name _ = ()
      fun rule2patt _ = ()
      val rule2term = Misc.id

    end

    fun h_name f _ = f
    fun h_patt p _ = p
    fun h_term t _ = t

    val bind = ReplaceInTermSpec.bind
    val qualifier = ReplaceInTermSpec.qualifier

    fun getRule' (RulePos (_, t')) = t'
    fun mkRule t' = RulePos (NONE, t')

    fun h_rule rule' (RulePos (newPos, t')) BV =
      RulePos (newPos, getRule' (rule' t' BV))

    fun UpdateRule ((f,t),t_r) _ = mkRule (ASM_AST.UpdateRule ((f,t),t_r))
    fun BlockRule Rs _ = mkRule (ASM_AST.BlockRule Rs)
    fun CondRule GRs _   = mkRule (ASM_AST.CondRule (List.map (fn (G_, R_) => (G_(), R_())) GRs))
    fun CaseRule (t, pRs) _  = mkRule (ASM_AST.CaseRule (t, map (fn (p,R_)=>(p,R_())) pRs))
    fun LetRule (p, t, R) _  = mkRule (ASM_AST.LetRule (p, t, R))

    fun AppRule (f, t) _ = mkRule (ASM_AST.AppRule (f, t))

    fun F_qual (p, A, G_) = (p, A, G_ ASM_AST.trueTerm)
    fun ctx' ctx (qual :T.QUALIFIER) = bind ctx (#1 qual, #2 qual)

    fun ChooseRule (qual, R_) ctx = mkRule (ASM_AST.ChooseRule (F_qual qual, R_ (ctx' ctx qual)))
    fun ForallRule (qual, R_) ctx = mkRule (ASM_AST.ForallRule (F_qual qual, R_ (ctx' ctx qual)))
  end

  structure ReplaceInRule = RuleTransf (ReplaceInRuleSpec)

  fun from_list L =
    List_.foldll (fn (T, (x, y)) => StringMap.insert (T, x, y)) (StringMap.empty) L

  structure Subst = struct
    open Subst
    local
      fun table_of_occurring_variables (S :SUBST) :ASM_Collect.VAR_SET StringMap.map =
	from_list (List.map (fn (s, t) => (s, ASM_Collect.variablesInTerm t)) (StringMap.listItemsi S))
    in
      fun apply_to_term S t =
	ReplaceInTerm.term t ((S, table_of_occurring_variables S), VarSet.empty)

      fun apply_to_rule S R =
	ReplaceInRule.rule R ((S, table_of_occurring_variables S), VarSet.empty)

      fun compose_subs (S2 :SUBST) (S1 :SUBST) =
	let val first  = Subst.fromList
                           (List.map (fn (x, t) => (x, apply_to_term S2 t)) (Subst.listItemsi S1))
	    val second = Subst.filteri
			   (fn (x2,_) => not (List.exists (fn (x1,_) => x1 = x2) (Subst.listItemsi S1)))
			   S2
	in Subst.override (first, second)
	end
    end
  end
end



(* ******************************************************************* *)
(*                                                                     *)
(*    symbolic pattern matching                                        *)
(*                                                                     *)
(* ******************************************************************* *)

structure ASM_Match =
struct
  exception ASM_Match_Impossible

  open ASM_AST

  structure Subst = ASM_Replace.Subst

  (* --- matcher --- *)

  local
    open ASM_Value
    exception Failed
    infix U
    fun env1 U env2 = Subst.override (env1, env2)    (* if ASM type-correct, domains are disjoint! *)
    val id = Misc.id

    (* only for the purpose of simplification !!!! *)
    fun do_match_patt_term (p :PATT, t :TERM) =
      let val do_match = do_match_patt_term
          fun succeed cond = if cond then Subst.empty else raise Failed
          val PattPos (p_pos, p) = p
          val TermPos (t_pos, t) = t
      in case p of
           PattType _ => raise ASM_Match_Impossible
         | AppPatt (IntConst i,       _) => succeed (case t of AppTerm (IntConst i', _) => i = i' | _ => false)
         | AppPatt (FloatConst r,     _) =>
             succeed ( case t of
               AppTerm (FloatConst r', _) =>
                 ASM_Value.compare (ASM_Value.FLOAT r, ASM_Value.FLOAT r') = EQUAL
             | _ => false )
         | AppPatt (StringConst s,    _) =>
             succeed ( case t of
               AppTerm (StringConst s', _) => s = s'
             | _ => false )
         | AppPatt (Id "true",  _) =>
             succeed ( case t of
               AppTerm (Id "true", TermPos (_, TupleTerm [])) => true
             | _ => false )
         | AppPatt (Id "false", _) =>
             succeed ( case t of
               AppTerm (Id "false", TermPos (_, TupleTerm [])) => true
             | _ => false )
         | AppPatt (Id "undef", _) =>
             succeed ( case t of
               AppTerm (Id "undef", TermPos (_, TupleTerm [])) => true
             | _ => false )
         | VarPatt x                     => Subst.singleton (x, TermPos (t_pos, t))
         | Placeholder                   => Subst.empty
         | TuplePatt []                  => succeed (case t of TupleTerm [] => true | _ => false)
         | TuplePatt p_list              =>
           ( case t of
               TupleTerm t_list => do_match_list (p_list, t_list)
             | _ => raise Failed )
         | _ => raise Failed
      end
    and do_match_list ([], []) = Subst.empty
      | do_match_list ([], _)  = raise Failed
      | do_match_list (_, [])  = raise Failed
      | do_match_list (p :: p_rest, t :: t_rest) = do_match_patt_term (p, t) U do_match_list (p_rest, t_rest)
  in
    datatype MATCH = Success of Subst.SUBST | Failure

    fun patt_term (p :PATT, t :TERM) =
      Success (do_match_patt_term (p, t)) handle Failed => Failure
  end
end

(* ******************************************************************* *)
(*                                                                     *)
(*    expand                                                           *)
(*                                                                     *)
(* ******************************************************************* *)


structure ASM_Expand =
struct
  exception ASM_Expand_Impossible
  exception ASM_Expand of string

  open ASM_AST
  structure Subst  = ASM_Replace.Subst
  structure VarSet = ASM_Collect.VarSet

  val id = Misc.id
  val fnpair = Misc.fnpair

  infix \\
  fun S \\ p =
    let val vars = ASM_Collect.variablesInPatt p
    in Subst.filteri (fn (s, _) => not (VarSet.member (vars, s))) S
    end

  structure ExpandLetInTermSpec = struct
    structure S = ASM_AST
    structure T = struct
      type TYPE = TYPE
      type NAME = NAME
      type PATT = PATT
      type TERM' = Subst.SUBST -> TERM
      type TERM  = TERM'
    end
    val h_type = Misc.id
    val h_name = Misc.id
    val h_patt = Misc.id

    fun getTerm' (TermPos (_, t')) = t'
    fun mkTerm t' = TermPos (NONE, t')

    fun h_term term' (TermPos (newPos, t')) S =
      TermPos (newPos, getTerm' (term' t' S))

    fun TermType (T, t) S = t S
    fun VarTerm v S = Subst.apply_to_term S (mkTerm (ASM_AST.VarTerm v))
    fun AppTerm (f, t) S = mkTerm (ASM_AST.AppTerm (f, t S))
    fun TupleTerm ts S   = mkTerm (ASM_AST.TupleTerm (map (fn t => t S) ts))
    fun CondTerm Gts S   = mkTerm (ASM_AST.CondTerm (map (fn (G, t)=> (G S, t S)) Gts))

    fun LetTerm (p, t, t') S =
      case (ASM_Match.patt_term (p, t S)) of
	ASM_Match.Success S' => t' (Subst.compose_subs S S')
      | ASM_Match.Failure => raise ASM_Expand "matching failed"

    fun CaseTerm (t0, pts) S =
      mkTerm (ASM_AST.CaseTerm (t0 S, map (fn (p, t) => (p, t (S \\ p))) pts))

    fun ListComprTerm (t, (p, A, G)) S =
      let val S' = S \\ p
      in mkTerm (ASM_AST.ListComprTerm (t S', (p, A S, G S')))
      end

    fun FSetComprTerm (t, (p, A, G)) S =
      let val S' = S \\ p
      in mkTerm (ASM_AST.FSetComprTerm (t S', (p, A S, G S')))
      end

    fun FMapComprTerm ((t, t'), (p, A, G)) S =
      let val S' = S \\ p
      in mkTerm (ASM_AST.FMapComprTerm ((t S', t' S'), (p, A S, G S')))
      end

    fun ForallTerm (p, A, G) S = mkTerm (ASM_AST.ForallTerm (p, A S, G (S \\ p)))
    fun ExistsTerm (p, A, G) S = mkTerm (ASM_AST.ExistsTerm (p, A S, G (S \\ p)))

    fun FSetEnumTerm ts S = mkTerm (ASM_AST.FSetEnumTerm (map (fn t => t S) ts))
    fun FMapEnumTerm ts S = mkTerm (ASM_AST.FMapEnumTerm (map (fn (t, t') => (t S, t' S)) ts))
  end

  structure ExpandLetInTerm = TermInduction (ExpandLetInTermSpec)

  fun letInTerm t = ExpandLetInTerm.term t Subst.empty




  structure ExpandLetInRuleSpec = struct
    structure S = ASM_AST
    structure T = struct
      open ExpandLetInTermSpec.T
      type RULE' = Subst.SUBST -> RULE
      type RULE  = RULE'
    end

    fun h_name x = x
    fun h_patt x = x
    fun h_term t S = ExpandLetInTerm.term t S

    fun getRule' (RulePos (_, t')) = t'
    fun mkRule t' = RulePos (NONE, t')

    fun h_rule rule' (RulePos (newPos, t')) S =
      RulePos (newPos, getRule' (rule' t' S))

    fun UpdateRule ((f,t),t_r) S = mkRule (ASM_AST.UpdateRule ((f,t S), t_r S))
    fun BlockRule Rs           S = mkRule (ASM_AST.BlockRule (map (fn R => R S) Rs))
    fun CondRule GRs           S = mkRule (ASM_AST.CondRule (map (fn (G, R)=>(G S,R S)) GRs))

    fun ChooseRule ((p,A,G),R) S = 
      let val S' = S \\ p
      in mkRule (ASM_AST.ChooseRule ((p, A S, G S'), R S'))
      end

    fun ForallRule ((p,A,G),R) S =
      let val S' = S \\ p
      in mkRule (ASM_AST.ForallRule ((p, A S, G S'), R S'))
      end

    fun CaseRule (t, pRs) S =
      mkRule (ASM_AST.CaseRule (t S, map (fn (p,R)=>(p, R (S \\ p))) pRs))

    fun AppRule (fName, t)     S = mkRule (ASM_AST.AppRule (fName, t S))

    fun LetRule (p, t, R) S =
      case (ASM_Match.patt_term (p, t S)) of
	ASM_Match.Success S' => R (Subst.compose_subs S S')
      | ASM_Match.Failure => raise ASM_Expand "matching failed"
  end


  structure ExpandLetInRule = RuleInduction (ExpandLetInRuleSpec)

  fun letInRule R = ExpandLetInRule.rule R Subst.empty



  structure ExpandNamedRulesSpec = struct
    structure S = ASM_AST
    structure T = struct
      open ASM_Replace.ReplaceInRuleSpec.T
    end
    structure CTX = struct
      type NAME  = unit
      type PATT  = unit
      type TERM  = unit
      type RULE  = bool * ASM_Dictionary.DICTIONARY      (* true = "fully expand", false = "expand once" *)
      type RULE' = bool * ASM_Dictionary.DICTIONARY
      fun rule2name _ = ()
      fun rule2patt _ = ()
      fun rule2term _ = ()
    end

    val expandNamedRules :(ASM_AST.RULE -> bool * ASM_Dictionary.DICTIONARY -> ASM_AST.RULE) ref =
      ref (fn _ => fn _ => raise ASM_Expand_Impossible)
    
    fun h_name x _ = x
    fun h_patt x _ = x
    fun h_term x _ = x

    fun bind dict _ = dict
    fun qualifier (p,A,G) _ = (p,A,G)

    fun getRule' (RulePos (_, t')) = t'
    fun mkRule t' = RulePos (NONE, t')

    fun h_rule rule' (RulePos (newPos, t')) dict =
      RulePos (newPos, getRule' (rule' t' dict))

    fun UpdateRule ((f,t),t_r) _ = mkRule (ASM_AST.UpdateRule ((f,t),t_r))
    fun BlockRule Rs         _ = mkRule (ASM_AST.BlockRule Rs)
    fun CondRule GRs         _ = mkRule (ASM_AST.CondRule (map (fn (G_,R_)=>(G_(),R_())) GRs))
    fun ChooseRule (qual, R) _ = mkRule (ASM_AST.ChooseRule (qual, R))
    fun ForallRule (qual, R) _ = mkRule (ASM_AST.ForallRule (qual, R))
    fun LetRule (p, t, R)    _ = mkRule (ASM_AST.LetRule (p, t, R))
    fun CaseRule (t, pRs)    _ = mkRule (ASM_AST.CaseRule (t, map (fn (p,R_)=>(p,R_())) pRs))

    fun F_qual (p, A, G_) = (p, A, G_ ASM_AST.trueTerm)

    fun ChooseRule (qual, R_) ctx = mkRule (ASM_AST.ChooseRule (F_qual qual, R_ ctx))
    fun ForallRule (qual, R_) ctx = mkRule (ASM_AST.ForallRule (F_qual qual, R_ ctx))

    fun AppRule (rName, t) (fullyExpand, dict) =
      let open ASM_Dictionary
      in case findNamedRule (dict, idStr rName) of
           SOME (_, ASM_SL_NamedRule { definition = TransitionExprPos (_, LambdaRule (p, R)) }) =>
             if fullyExpand
             then (!expandNamedRules) (letInRule (mkRule (ASM_AST.LetRule (p, t, R)))) (fullyExpand, dict)
             else letInRule (mkRule (ASM_AST.LetRule (p, t, R)))
         | NONE =>
             mkRule (ASM_AST.AppRule (rName, t))
      end
  end

  structure ExpandNamedRules = RuleTransf (ExpandNamedRulesSpec)
  val _ = ( ExpandNamedRulesSpec.expandNamedRules := ExpandNamedRules.rule )

  fun namedRulesOnce  dict R = ExpandNamedRules.rule R (false, dict)
  fun namedRulesFully dict R = ExpandNamedRules.rule R (true, dict)


  structure ExpandDerivedFunctionsInTermSpec = struct
    structure S = ASM_AST
    structure T = ExpandNamedRulesSpec.T
    structure CTX = struct
      type TYPE  = unit
      type NAME  = unit
      type PATT  = unit
      type TERM  = bool * ASM_Dictionary.DICTIONARY      (* true = "fully expand", false = "expand once" *)
      type TERM' = bool * ASM_Dictionary.DICTIONARY
      fun term2type _ = ()
      fun term2name _ = ()
      fun term2patt _ = ()
    end

    val expandDerivedInTerm :(ASM_AST.TERM -> bool * ASM_Dictionary.DICTIONARY -> ASM_AST.TERM) ref =
      ref (fn _ => fn _ => raise ASM_Expand_Impossible)

    fun h_type x _ = x
    fun h_name x _ = x
    fun h_patt x _ = x

    local open ASM_Replace in
      val (getTerm', mkTerm) = (ReplaceInTermSpec.getTerm', ReplaceInTermSpec.mkTerm)
    end

    fun bind dict _ = dict
    fun qualifier (p,A,G) _ = (p,A,G)

    fun h_term term' (TermPos (newPos, t')) dict =
      TermPos (newPos, getTerm' (term' t' dict))

    fun TermType (T, t) _ = t
    fun VarTerm v _      = mkTerm (ASM_AST.VarTerm v)
    fun CondTerm Gts _   = mkTerm (ASM_AST.CondTerm (List.map (fn (G_, t_) => (G_(), t_())) Gts))
    fun TupleTerm ts _   = mkTerm (ASM_AST.TupleTerm ts)

    fun LetTerm (p, t, t') _ = mkTerm (ASM_AST.LetTerm (p, t, t'))
    fun CaseTerm (t, pts) _  = mkTerm (ASM_AST.CaseTerm (t, map (fn (p,t_)=>(p,t_())) pts))

    fun F_qual (p, A, G_) = (p, A, G_ ASM_AST.trueTerm)

    fun ListComprTerm (t_, qual) ctx = mkTerm (ASM_AST.ListComprTerm (t_ ctx, F_qual qual))
    fun FSetComprTerm (t_, qual) ctx = mkTerm (ASM_AST.FSetComprTerm (t_ ctx, F_qual qual))
    fun FMapComprTerm ((t_,t_'), qual) ctx =
      mkTerm (ASM_AST.FMapComprTerm ((t_ ctx, t_' ctx), F_qual qual))

    fun ForallTerm qual _ = mkTerm (ASM_AST.ForallTerm (F_qual qual))
    fun ExistsTerm qual _ = mkTerm (ASM_AST.ExistsTerm (F_qual qual))

    fun FSetEnumTerm ts _ = mkTerm (ASM_AST.FSetEnumTerm ts)
    fun FMapEnumTerm ts _ = mkTerm (ASM_AST.FMapEnumTerm ts)

    fun isRecursiveFunction _ = false   (*!!!!!!!!!!! check for recursive derived functions !!!!! *)

    fun AppTerm (f, t) (fullyExpand, dict) =
      case f of
        ASM_AST.IntConst i => mkTerm (ASM_AST.AppTerm (f, t))
      | ASM_AST.FloatConst r => mkTerm (ASM_AST.AppTerm (f, t))
      | ASM_AST.StringConst s => mkTerm (ASM_AST.AppTerm (f, t))
      | ASM_AST.Id fName =>
        ( case (ASM_Dictionary.findFunction (dict, fName)) of
	     SOME (_, ASM_Dictionary.ASM_SL_DerivedFunction { definition = FE }) =>
	       if not (isRecursiveFunction (fName))
	       then case FE of
                      FunctionExprPos (_, LambdaTerm (p, t')) =>
	              ( if fullyExpand
                        then (!expandDerivedInTerm)
                               (letInTerm (mkTerm (ASM_AST.LetTerm (p, t, t'))))
                               (fullyExpand, dict)
			else letInTerm (mkTerm (ASM_AST.LetTerm (p, t, t'))) )
		    | _ => raise ASM_Expand_Impossible
	       else raise ASM_Expand ("recursive derived function: "^fName)
	   | _ => mkTerm (ASM_AST.AppTerm (f, t)) )
  end

  structure ExpandDerivedFunctionsInTerm = TermTransf (ExpandDerivedFunctionsInTermSpec)
  val _ = ( ExpandDerivedFunctionsInTermSpec.expandDerivedInTerm :=
              ExpandDerivedFunctionsInTerm.term )

  fun derivedFunctionsInTermOnce  dict t = ExpandDerivedFunctionsInTerm.term t (false, dict)
  fun derivedFunctionsInTermFully dict t = ExpandDerivedFunctionsInTerm.term t (true, dict)


  structure ExpandDerivedFunctionsInRuleSpec = struct
    structure S = ASM_AST
    structure T = struct
      open ASM_Replace.ReplaceInRuleSpec.T
    end
    structure CTX = struct
      type NAME  = unit
      type PATT  = unit
      type TERM  = bool * ASM_Dictionary.DICTIONARY
      type RULE  = bool * ASM_Dictionary.DICTIONARY      (* true = "fully expand", false = "expand once" *)
      type RULE' = bool * ASM_Dictionary.DICTIONARY
      fun rule2name _ = ()
      fun rule2patt _ = ()
      fun rule2term ctx = ctx
    end

    fun h_name x _ = x
    fun h_patt x _ = x
    fun h_term t ctx = ExpandDerivedFunctionsInTerm.term t ctx

    fun bind dict _ = dict
    fun qualifier (p,A,G) _ = (p,A,G)

    fun getRule' (RulePos (_, t')) = t'
    fun mkRule t' = RulePos (NONE, t')

    fun h_rule rule' (RulePos (newPos, t')) dict =
      RulePos (newPos, getRule' (rule' t' dict))

    fun UpdateRule ((f,t),t_r) _ = mkRule (ASM_AST.UpdateRule ((f,t),t_r))
    fun BlockRule Rs         _ = mkRule (ASM_AST.BlockRule Rs)
    fun CondRule GRs         _ = mkRule (ASM_AST.CondRule (map (fn (G_,R_)=>(G_(),R_())) GRs))
    fun ChooseRule (qual, R) _ = mkRule (ASM_AST.ChooseRule (qual, R))
    fun ForallRule (qual, R) _ = mkRule (ASM_AST.ForallRule (qual, R))
    fun LetRule (p, t, R)    _ = mkRule (ASM_AST.LetRule (p, t, R))
    fun CaseRule (t, pRs)    _ = mkRule (ASM_AST.CaseRule (t, map (fn (p,R_)=>(p,R_())) pRs))
    fun AppRule (rName, t) _   = mkRule (ASM_AST.AppRule (rName, t))

    fun F_qual (p, A, G_) = (p, A, G_ ASM_AST.trueTerm)

    fun ChooseRule (qual, R_) ctx = mkRule (ASM_AST.ChooseRule (F_qual qual, R_ ctx))
    fun ForallRule (qual, R_) ctx = mkRule (ASM_AST.ForallRule (F_qual qual, R_ ctx))
  end

  structure ExpandDerivedFunctionsInRule = RuleTransf (ExpandDerivedFunctionsInRuleSpec)

  fun derivedFunctionsInRuleOnce  dict t = ExpandDerivedFunctionsInRule.rule t (false, dict)
  fun derivedFunctionsInRuleFully dict t = ExpandDerivedFunctionsInRule.rule t (true, dict)

end
