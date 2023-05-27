(* ******************************************************************* *\
 *
 *   The ASM Workbench for SML/NJ 1.10
 *
 *   File:         asm-induction.sml
 *
 *   Description: 
 *     Structural induction over ASM-SL abstract syntax trees (ASTs),
 *     context-dependent transformations of the ASTs
 *
 *   Author:       $Author: giusp $
 *   Date:         $Date: 2001/04/28 13:14:53 $
 *   Revision:     $Revision: 1.7 $
 *
\* ******************************************************************* *)

(* =================================================================== *\

      induction over patterns

\* =================================================================== *)

signature PATT_ALGEBRA =
sig
  structure S :sig      (* source types *)
    type TYPE and NAME and PATT
  end
  structure T :sig      (* target types *)
    type TYPE and NAME and PATT
    type PATT'
  end

  val h_type : S.TYPE -> T.TYPE
  val h_name : S.NAME -> T.NAME

  val h_patt : ((S.TYPE, S.NAME, S.PATT) ASM_AST.PATT' -> T.PATT') -> S.PATT -> T.PATT

  val PattType    : T.TYPE * T.PATT -> T.PATT'
  val Placeholder : T.PATT'
  val VarPatt     : string -> T.PATT'
  val AppPatt     : T.NAME * T.PATT -> T.PATT'
  val TuplePatt   : T.PATT list -> T.PATT'
end


functor PattInduction (H :PATT_ALGEBRA)
  : sig
      val patt  : H.S.PATT  -> H.T.PATT
      val patt' : (H.S.TYPE, H.S.NAME, H.S.PATT) ASM_AST.PATT' -> H.T.PATT'
    end =
struct
  structure S = H.S
  structure T = H.T

  fun patt p = H.h_patt patt' p

  and patt' (p' : (S.TYPE, S.NAME, S.PATT) ASM_AST.PATT') :T.PATT' =
    let open ASM_AST
    in case p' of
	 PattType (T, p) => H.PattType (H.h_type T, patt p)
       | Placeholder     => H.Placeholder
       | VarPatt s       => H.VarPatt s
       | AppPatt (f, p)  => H.AppPatt (H.h_name f, patt p)
       | TuplePatt ps    => H.TuplePatt (map patt ps)
    end
end


(* =================================================================== *\

      context-dependent transformation of patterns

\* =================================================================== *)

signature PATT_TRANSF_SPEC =
sig
  structure S :sig      (* source types *)
    type TYPE and NAME and PATT
  end
  structure T :sig      (* target types *)
    type TYPE and NAME and PATT
    type PATT'
  end
  structure CTX :sig
    type TYPE and NAME and PATT

    val patt2name :PATT -> NAME
    val patt2type :PATT -> TYPE
  end

  val h_type : S.TYPE -> CTX.TYPE -> T.TYPE
  val h_name : S.NAME -> CTX.NAME -> T.NAME

  val h_patt : ((S.TYPE, S.NAME, S.PATT) ASM_AST.PATT' -> CTX.PATT -> T.PATT') -> (S.PATT -> CTX.PATT -> T.PATT)

  val PattType    : T.TYPE * T.PATT -> CTX.PATT -> T.PATT'
  val Placeholder : CTX.PATT -> T.PATT'
  val VarPatt     : string -> CTX.PATT -> T.PATT'
  val AppPatt     : T.NAME * T.PATT -> CTX.PATT -> T.PATT'
  val TuplePatt   : T.PATT list -> CTX.PATT -> T.PATT'
end


functor PattTransf (H :PATT_TRANSF_SPEC)
  : sig
      val patt  : H.S.PATT -> H.CTX.PATT -> H.T.PATT
      val patt' : (H.S.TYPE, H.S.NAME, H.S.PATT) ASM_AST.PATT' -> H.CTX.PATT -> H.T.PATT'
    end =
struct
  structure S = H.S
  structure T = H.T
  structure CTX = H.CTX

  fun patt p ctx = H.h_patt patt' p ctx

  and patt' (p' : (S.TYPE, S.NAME, S.PATT) ASM_AST.PATT') (ctx :CTX.PATT) :T.PATT' =
    let open ASM_AST
    in case p' of
	 PattType (T, p) => H.PattType (H.h_type T (CTX.patt2type ctx), patt p ctx) ctx
       | Placeholder     => H.Placeholder ctx
       | VarPatt s       => H.VarPatt s ctx
       | AppPatt (f, p)  => H.AppPatt (H.h_name f (CTX.patt2name ctx), patt p ctx) ctx
       | TuplePatt ps    => H.TuplePatt (map (fn p => patt p ctx) ps) ctx
    end
end


(* =================================================================== *\

      induction over terms

\* =================================================================== *)

signature TERM_ALGEBRA =
sig
  structure S :sig      (* source types *)
    type TYPE and NAME and PATT and TERM
  end
  structure T :sig      (* target types *)
    type TYPE and NAME and PATT and TERM
    type TERM'
  end

  val h_type : S.TYPE -> T.TYPE
  val h_name : S.NAME -> T.NAME
  val h_patt : S.PATT -> T.PATT

  val h_term : ((S.TYPE, S.NAME, S.PATT, S.TERM) ASM_AST.TERM' -> T.TERM') -> S.TERM -> T.TERM

  val TermType  : T.TYPE * T.TERM -> T.TERM'

  val VarTerm   : string -> T.TERM'
  val AppTerm   : T.NAME * T.TERM -> T.TERM'
  val TupleTerm : T.TERM list -> T.TERM'

  val CondTerm  : (T.TERM * T.TERM) list -> T.TERM'
  val LetTerm   : T.PATT * T.TERM * T.TERM -> T.TERM'
  val CaseTerm  : T.TERM * (T.PATT * T.TERM) list -> T.TERM'

  val ListComprTerm : T.TERM * ((T.PATT, T.TERM, T.TERM) ASM_AST.QUALIFIER) -> T.TERM'
  val FSetComprTerm : T.TERM * ((T.PATT, T.TERM, T.TERM) ASM_AST.QUALIFIER) -> T.TERM'
  val FMapComprTerm : (T.TERM * T.TERM) * ((T.PATT, T.TERM, T.TERM) ASM_AST.QUALIFIER) -> T.TERM'

  val FSetEnumTerm  : T.TERM list -> T.TERM'
  val FMapEnumTerm  : (T.TERM * T.TERM) list -> T.TERM'

  val ForallTerm : ((T.PATT, T.TERM, T.TERM) ASM_AST.QUALIFIER) -> T.TERM'
  val ExistsTerm : ((T.PATT, T.TERM, T.TERM) ASM_AST.QUALIFIER) -> T.TERM'
end


functor TermInduction (H :TERM_ALGEBRA)
  : sig
      val term  : H.S.TERM -> H.T.TERM
      val term' : (H.S.TYPE, H.S.NAME, H.S.PATT, H.S.TERM) ASM_AST.TERM' -> H.T.TERM'
    end =
struct
  open Misc

  structure S = H.S
  structure T = H.T

  fun term t = H.h_term term' t

  and term' (t' : (S.TYPE, S.NAME, S.PATT, S.TERM) ASM_AST.TERM') :T.TERM' =
    let open ASM_AST
        fun qualifier (p, A, G) = (H.h_patt p, term A, term G)
    in case t' of
	 TermType (T, t)   => H.TermType (H.h_type T, term t)

       | VarTerm s         => H.VarTerm s
       | AppTerm (f, t)    => H.AppTerm (H.h_name f, term t)
       | TupleTerm ts      => H.TupleTerm (map term ts)

       | CondTerm Gts        => H.CondTerm (map2 (term, term) Gts)
       | LetTerm (p, t1, t2) => H.LetTerm (H.h_patt p, term t1, term t2)
       | CaseTerm (t, pts)   => H.CaseTerm (term t, map2 (H.h_patt, term) pts)

       | ListComprTerm (t, qual) => H.ListComprTerm (term t, qualifier qual)
       | FSetComprTerm (t, qual) => H.FSetComprTerm (term t, qualifier qual)
       | FMapComprTerm ((t1,t2), qual) => H.FMapComprTerm ((term t1, term t2), qualifier qual)

       | FSetEnumTerm ts => H.FSetEnumTerm (map term ts)
       | FMapEnumTerm es => H.FMapEnumTerm (map2 (term, term) es)

       | ForallTerm qual => H.ForallTerm (qualifier qual)
       | ExistsTerm qual => H.ExistsTerm (qualifier qual)
    end
end


(* =================================================================== *\

      context-dependent transformation of terms

\* =================================================================== *)

signature TERM_TRANSF_SPEC =
sig
  structure S :sig      (* source types *)
    type TYPE and NAME and PATT and TERM
  end
  structure CTX :sig
    type TYPE and NAME and PATT and TERM

    val term2name :TERM -> NAME
    val term2type :TERM -> TYPE
    val term2patt :TERM -> PATT
  end
  structure T :sig      (* target types *)
    type TYPE and NAME and PATT and TERM
    type TERM'
    type QUALIFIER
  end

  val h_type : S.TYPE -> CTX.TYPE -> T.TYPE
  val h_name : S.NAME -> CTX.NAME -> T.NAME
  val h_patt : S.PATT -> CTX.PATT -> T.PATT

  val bind      : CTX.TERM -> T.PATT * T.TERM -> CTX.TERM

  val qualifier : (T.PATT, T.TERM, T.TERM -> T.TERM) ASM_AST.QUALIFIER -> CTX.TERM -> T.QUALIFIER

  val h_term : ((S.TYPE, S.NAME, S.PATT, S.TERM) ASM_AST.TERM' -> CTX.TERM -> T.TERM')
               -> (S.TERM -> CTX.TERM -> T.TERM)

  val TermType    : T.TYPE * T.TERM -> CTX.TERM -> T.TERM'

  val VarTerm     : string -> CTX.TERM -> T.TERM'
  val AppTerm     : T.NAME * T.TERM -> CTX.TERM -> T.TERM'
  val TupleTerm   : T.TERM list -> CTX.TERM -> T.TERM'

  val CondTerm    : ((unit -> T.TERM) * (unit -> T.TERM)) list -> CTX.TERM -> T.TERM'
  val LetTerm     : T.PATT * T.TERM * T.TERM -> CTX.TERM -> T.TERM'
  val CaseTerm    : T.TERM * (T.PATT * (unit -> T.TERM)) list -> CTX.TERM -> T.TERM'

  val ListComprTerm : (CTX.TERM -> T.TERM) * T.QUALIFIER -> CTX.TERM -> T.TERM'
  val FSetComprTerm : (CTX.TERM -> T.TERM) * T.QUALIFIER -> CTX.TERM -> T.TERM'
  val FMapComprTerm : ((CTX.TERM -> T.TERM) * (CTX.TERM -> T.TERM)) * T.QUALIFIER -> CTX.TERM -> T.TERM'

  val FSetEnumTerm  : T.TERM list -> CTX.TERM -> T.TERM'
  val FMapEnumTerm  : (T.TERM * T.TERM) list -> CTX.TERM -> T.TERM'

  val ForallTerm    : T.QUALIFIER -> CTX.TERM -> T.TERM'
  val ExistsTerm    : T.QUALIFIER -> CTX.TERM -> T.TERM'
end


functor TermTransf (H :TERM_TRANSF_SPEC)
  : sig
      val term  : H.S.TERM -> H.CTX.TERM -> H.T.TERM
      val term' : (H.S.TYPE, H.S.NAME, H.S.PATT, H.S.TERM) ASM_AST.TERM' -> H.CTX.TERM -> H.T.TERM'
    end =
struct
  type ('patt, 'term, 'guard) QUALIFIER = ('patt, 'term, 'guard) ASM_AST.QUALIFIER

  structure S = H.S
  structure T = H.T
  structure CTX = H.CTX

  fun qualInd (p, A, G) ctx =
    let val p' = H.h_patt p (CTX.term2patt ctx)
	val A' = term A ctx
	val G' = fn x => term G (H.bind ctx (p', x))
    in H.qualifier (p', A', G') ctx
    end

  and term t ctx = H.h_term term' t ctx

  and term' (p' : (S.TYPE, S.NAME, S.PATT, S.TERM) ASM_AST.TERM') (ctx :CTX.TERM) :T.TERM' =
    let open ASM_AST
    in case p' of
	 TermType (T, p) => H.TermType (H.h_type T (CTX.term2type ctx), term p ctx) ctx

       | VarTerm s       => H.VarTerm s ctx
       | AppTerm (f, p)  => H.AppTerm (H.h_name f (CTX.term2name ctx), term p ctx) ctx
       | TupleTerm ps    => H.TupleTerm (map (fn p => term p ctx) ps) ctx

       | CondTerm Gts    => H.CondTerm (map (fn (G, t) => (fn _ => term G ctx, fn _ => term t ctx)) Gts) ctx
       | LetTerm (p, t1, t2) =>
           let val (p', t1') = (H.h_patt p (CTX.term2patt ctx), term t1 ctx)
           in H.LetTerm (p', t1', term t2 (H.bind ctx (p', t1'))) ctx
           end
       | CaseTerm (t0, pts) => 
           let val t0' = term t0 ctx
               fun pt (p, t) =
                 let val p' = H.h_patt p (CTX.term2patt ctx)
		     val t' = fn _ => term t (H.bind ctx (p', t0'))
		 in (p', t') 
                 end
           in H.CaseTerm (t0', map pt pts) ctx
           end

       | ListComprTerm (t, qual) =>
           let val qual' = qualInd qual ctx
           in H.ListComprTerm (term t, qual') ctx
           end
       | FSetComprTerm (t, qual) =>
           let val qual' = qualInd qual ctx
           in H.FSetComprTerm (term t, qual') ctx
           end
       | FMapComprTerm ((t1, t2), qual) =>
           let val qual' = qualInd qual ctx
           in H.FMapComprTerm ((term t1, term t2), qual') ctx
           end

       | FSetEnumTerm ts  => H.FSetEnumTerm (map (fn t => term t ctx) ts) ctx
       | FMapEnumTerm tts => H.FMapEnumTerm (map (fn (t1, t2) => (term t1 ctx, term t2 ctx)) tts) ctx

       | ForallTerm qual => H.ForallTerm (qualInd qual ctx) ctx
       | ExistsTerm qual => H.ExistsTerm (qualInd qual ctx) ctx
    end
end


(* =================================================================== *\

      induction over transition rules

\* =================================================================== *)

signature RULE_ALGEBRA =
sig
  structure S :sig      (* source types *)
    type NAME and PATT and TERM and RULE
  end
  structure T :sig      (* target types *)
    type NAME and PATT and TERM and RULE
    type RULE'
  end

  val h_name : S.NAME -> T.NAME
  val h_patt : S.PATT -> T.PATT
  val h_term : S.TERM -> T.TERM

  val h_rule : ((S.NAME, S.PATT, S.TERM, S.RULE) ASM_AST.RULE' -> T.RULE') -> S.RULE -> T.RULE

  val UpdateRule : (T.NAME * T.TERM) * T.TERM -> T.RULE'
  val BlockRule  : T.RULE list -> T.RULE'

  val CondRule   : (T.TERM * T.RULE) list -> T.RULE'
  val LetRule    : T.PATT * T.TERM * T.RULE -> T.RULE'
  val CaseRule   : T.TERM * (T.PATT * T.RULE) list -> T.RULE'

  val ChooseRule : ((T.PATT, T.TERM, T.TERM) ASM_AST.QUALIFIER) * T.RULE -> T.RULE'
  val ForallRule : ((T.PATT, T.TERM, T.TERM) ASM_AST.QUALIFIER) * T.RULE -> T.RULE'

  val AppRule    : T.NAME * T.TERM -> T.RULE'
end


functor RuleInduction (H :RULE_ALGEBRA)
  : sig
      val rule  : H.S.RULE -> H.T.RULE
      val rule' : (H.S.NAME, H.S.PATT, H.S.TERM, H.S.RULE) ASM_AST.RULE' -> H.T.RULE'
    end =
struct
  open Misc

  type ('patt, 'term, 'guard) QUALIFIER = ('patt, 'term, 'guard) ASM_AST.QUALIFIER

  structure S = H.S
  structure T = H.T

  fun rule R = H.h_rule rule' R

  and rule' (R' : (S.NAME, S.PATT, S.TERM, S.RULE) ASM_AST.RULE') :T.RULE' =
    let open ASM_AST
        fun qualifier (p, A, G) = (H.h_patt p, H.h_term A, H.h_term G)
    in case R' of
         UpdateRule ((f,t), t') => H.UpdateRule ((H.h_name f, H.h_term t), H.h_term t')

       | BlockRule Rs      => H.BlockRule (map rule Rs)
       | CondRule GRs      => H.CondRule (map2 (H.h_term, rule) GRs)

       | LetRule (p, t, R) => H.LetRule (H.h_patt p, H.h_term t, rule R)
       | CaseRule (t, pRs) => H.CaseRule (H.h_term t, map2 (H.h_patt, rule) pRs)

       | ChooseRule (qual, R) => H.ChooseRule (qualifier qual, rule R)
       | ForallRule (qual, R) => H.ForallRule (qualifier qual, rule R)

       | AppRule (r, t) => H.AppRule (H.h_name r, H.h_term t)
    end
end


(* =================================================================== *\

      context-dependent transformation of transition rules

\* =================================================================== *)

signature RULE_TRANSF_SPEC =
sig
(*  type ('patt, 'range, 'guard) QUALIFIER = ('patt, 'range, 'guard) ASM_AST.QUALIFIER *)

  structure S :sig      (* source types *)
    type NAME and PATT and TERM and RULE
  end
  structure CTX :sig
    type NAME and PATT and TERM and RULE

    val rule2name :RULE -> NAME
    val rule2patt :RULE -> PATT
    val rule2term :RULE -> TERM
  end
  structure T :sig      (* target types *)
    type NAME and PATT and TERM and RULE
    type RULE'
    type QUALIFIER
  end

  val h_name : S.NAME -> CTX.NAME -> T.NAME
  val h_patt : S.PATT -> CTX.PATT -> T.PATT
  val h_term : S.TERM -> CTX.TERM -> T.TERM

  val bind      : CTX.RULE -> T.PATT * T.TERM -> CTX.RULE
  val qualifier : (T.PATT, T.TERM, T.TERM -> T.TERM) ASM_AST.QUALIFIER -> CTX.RULE -> T.QUALIFIER

  val h_rule : ((S.NAME, S.PATT, S.TERM, S.RULE) ASM_AST.RULE' -> CTX.RULE -> T.RULE')
               -> (S.RULE -> CTX.RULE -> T.RULE)

  val UpdateRule : (T.NAME * T.TERM) * T.TERM -> CTX.RULE -> T.RULE'
  val BlockRule  : T.RULE list -> CTX.RULE -> T.RULE'

  val CondRule   : ((unit -> T.TERM) * (unit -> T.RULE)) list -> CTX.RULE -> T.RULE'
  val LetRule    : T.PATT * T.TERM * T.RULE -> CTX.RULE -> T.RULE'
  val CaseRule   : T.TERM * (T.PATT * (unit -> T.RULE)) list -> CTX.RULE -> T.RULE'

  val ChooseRule : T.QUALIFIER * (CTX.RULE -> T.RULE) -> CTX.RULE -> T.RULE'
  val ForallRule : T.QUALIFIER * (CTX.RULE -> T.RULE) -> CTX.RULE -> T.RULE'

  val AppRule    : T.NAME * T.TERM -> CTX.RULE -> T.RULE'
end


functor RuleTransf (H :RULE_TRANSF_SPEC)
  : sig
      val rule  : H.S.RULE -> H.CTX.RULE -> H.T.RULE
      val rule' : (H.S.NAME, H.S.PATT, H.S.TERM, H.S.RULE) ASM_AST.RULE' -> H.CTX.RULE -> H.T.RULE'
    end =
struct
  type ('patt, 'term, 'guard) QUALIFIER = ('patt, 'term, 'guard) ASM_AST.QUALIFIER

  structure S = H.S
  structure T = H.T
  structure CTX = H.CTX

  fun qualInd (p, A, G) ctx =
    let val p' = H.h_patt p (CTX.rule2patt ctx)
	val A' = H.h_term A (CTX.rule2term ctx)
	val G' = fn x => H.h_term G (CTX.rule2term (H.bind ctx (p', x)))
    in H.qualifier (p', A', G') ctx
    end

  fun rule R ctx = H.h_rule rule' R ctx

  and rule' (R' : (S.NAME, S.PATT, S.TERM, S.RULE) ASM_AST.RULE') (ctx :CTX.RULE) :T.RULE' =
    let open ASM_AST
    in case R' of
         UpdateRule ((f,t), t') =>
           H.UpdateRule ( (H.h_name f (CTX.rule2name ctx), H.h_term t (CTX.rule2term ctx)),
                          H.h_term t' (CTX.rule2term ctx) ) ctx

       | BlockRule Rs      => H.BlockRule (map (fn R => rule R ctx) Rs) ctx

       | CondRule GRs      =>
           H.CondRule (map (fn (G, R) => (fn _ => H.h_term G (CTX.rule2term ctx), fn _ => rule R ctx)) GRs) ctx
       | LetRule (p, t, R) =>
           let val (p', t') = (H.h_patt p (CTX.rule2patt ctx), H.h_term t (CTX.rule2term ctx))
           in H.LetRule (p', t', rule R (H.bind ctx (p', t'))) ctx
           end
       | CaseRule (t0, pRs) =>
           let val t0' = H.h_term t0 (CTX.rule2term ctx)
               fun pR (p, R) =
                 let val p' = H.h_patt p (CTX.rule2patt ctx)
		     val R' = fn _ => rule R (H.bind ctx (p', t0'))
		 in (p', R') 
                 end
           in H.CaseRule (t0', map pR pRs) ctx
           end
       | ForallRule (qual, R) =>
           let val qual' = qualInd qual ctx
           in H.ForallRule (qual', rule R) ctx
           end
       | ChooseRule (qual, R) =>
           let val qual' = qualInd qual ctx
           in H.ChooseRule (qual', rule R) ctx
           end
       | AppRule (r, t)  =>
           H.AppRule (H.h_name r (CTX.rule2name ctx), H.h_term t (CTX.rule2term ctx)) ctx
    end
end


(* =================================================================== *\

      context-dependent transformation of function expressions

\* =================================================================== *)

signature FUNCTION_EXPR_TRANSF_SPEC =
sig
  structure S :sig      (* source types *)
    type PATT and TERM and FUNCTION_EXPR
  end
  structure CTX :sig
    type PATT and TERM and FUNCTION_EXPR

    val functionExpr2patt :FUNCTION_EXPR -> PATT
    val functionExpr2term :FUNCTION_EXPR -> TERM
  end
  structure T :sig      (* target types *)
    type PATT and TERM and FUNCTION_EXPR
    type FUNCTION_EXPR'
  end

  val h_patt : S.PATT -> CTX.PATT -> T.PATT
  val h_term : S.TERM -> CTX.TERM -> T.TERM

  val h_functionExpr :
    ((S.PATT, S.TERM) ASM_AST.FUNCTION_EXPR' -> CTX.FUNCTION_EXPR -> T.FUNCTION_EXPR')
    -> (S.FUNCTION_EXPR -> CTX.FUNCTION_EXPR -> T.FUNCTION_EXPR)

  val LambdaTerm : (T.PATT * (CTX.TERM -> T.TERM)) -> CTX.FUNCTION_EXPR -> T.FUNCTION_EXPR'
  val SetToRel   : T.TERM -> CTX.FUNCTION_EXPR -> T.FUNCTION_EXPR'
  val MapToFun   : T.TERM -> CTX.FUNCTION_EXPR -> T.FUNCTION_EXPR'
end


functor FunctionExprTransf (H :FUNCTION_EXPR_TRANSF_SPEC)
  : sig
      val functionExpr  : H.S.FUNCTION_EXPR
                          -> H.CTX.FUNCTION_EXPR -> H.T.FUNCTION_EXPR
      val functionExpr' : (H.S.PATT, H.S.TERM) ASM_AST.FUNCTION_EXPR'
                          -> H.CTX.FUNCTION_EXPR -> H.T.FUNCTION_EXPR'
    end =
struct
  structure S = H.S
  structure T = H.T
  structure CTX = H.CTX

  fun functionExpr FE ctx = H.h_functionExpr functionExpr' FE ctx

  and functionExpr' (FE' :(S.PATT, S.TERM) ASM_AST.FUNCTION_EXPR')
                    (ctx :CTX.FUNCTION_EXPR) :T.FUNCTION_EXPR' =
    let open ASM_AST
    in case FE' of
         LambdaTerm (p, t) =>
           let val p' = H.h_patt p (CTX.functionExpr2patt ctx)
           in H.LambdaTerm (p', H.h_term t) ctx
           end
       | SetToRel t => H.SetToRel (H.h_term t (CTX.functionExpr2term ctx)) ctx
       | MapToFun t => H.MapToFun (H.h_term t (CTX.functionExpr2term ctx)) ctx
    end
end


(* =================================================================== *\

      context-dependent transformation of transition expressions

\* =================================================================== *)

signature TRANSITION_EXPR_TRANSF_SPEC =
sig
  structure S :sig      (* source types *)
    type PATT and RULE and TRANSITION_EXPR
  end
  structure CTX :sig
    type PATT and RULE and TRANSITION_EXPR

    val transitionExpr2patt :TRANSITION_EXPR -> PATT
  end
  structure T :sig      (* target types *)
    type PATT and RULE and TRANSITION_EXPR
    type TRANSITION_EXPR'
  end

  val h_patt : S.PATT -> CTX.PATT -> T.PATT
  val h_rule : S.RULE -> CTX.RULE -> T.RULE

  val h_transitionExpr :
    ((S.PATT, S.RULE) ASM_AST.TRANSITION_EXPR' -> CTX.TRANSITION_EXPR -> T.TRANSITION_EXPR')
    -> (S.TRANSITION_EXPR -> CTX.TRANSITION_EXPR -> T.TRANSITION_EXPR)

  val LambdaRule : (T.PATT * (CTX.RULE -> T.RULE)) -> CTX.TRANSITION_EXPR -> T.TRANSITION_EXPR'
end


functor TransitionExprTransf (H :TRANSITION_EXPR_TRANSF_SPEC)
  : sig
      val transitionExpr  : H.S.TRANSITION_EXPR
                          -> H.CTX.TRANSITION_EXPR -> H.T.TRANSITION_EXPR
      val transitionExpr' : (H.S.PATT, H.S.RULE) ASM_AST.TRANSITION_EXPR'
                          -> H.CTX.TRANSITION_EXPR -> H.T.TRANSITION_EXPR'
    end =
struct
  structure S = H.S
  structure T = H.T
  structure CTX = H.CTX

  fun transitionExpr TE ctx = H.h_transitionExpr transitionExpr' TE ctx

  and transitionExpr' (TE' :(S.PATT, S.RULE) ASM_AST.TRANSITION_EXPR')
                      (ctx :CTX.TRANSITION_EXPR) :T.TRANSITION_EXPR' =
    let open ASM_AST
    in case TE' of
         LambdaRule (p, R) =>
           let val p' = H.h_patt p (CTX.transitionExpr2patt ctx)
           in H.LambdaRule (p', H.h_rule R) ctx
           end
    end
end

