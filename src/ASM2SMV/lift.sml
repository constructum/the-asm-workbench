functor LiftTermFn ( T : sig
    type TYPE
    type NAME
    type PATT
    type TERM
    val h_type :ASM_AST.TYPE -> TYPE
    val h_name :ASM_AST.NAME -> NAME
    val h_patt :ASM_AST.PATT -> PATT
    val F_term :(TYPE, NAME, PATT, TERM) ASM_AST.TERM' -> TERM
  end ) : sig
    val term  : ASM_AST.TERM -> T.TERM
  end =
struct
  structure LiftTermSpec :TERM_ALGEBRA = struct
    structure S = ASM_AST
    structure T = struct
      open T
      type TERM' = (TYPE, NAME, PATT, TERM) ASM_AST.TERM'
    end

    val h_type = T.h_type
    val h_name = T.h_name
    val h_patt = T.h_patt

    fun h_term F (ASM_AST.TermPos (pos, t)) = T.F_term (F t)

    open ASM_AST
  end

  structure LiftTerm = TermInduction (LiftTermSpec)

  val term = LiftTerm.term
end





functor LiftRuleFn ( T : sig
    type NAME
    type PATT
    type TERM
    type RULE
    val h_name :ASM_AST.NAME -> NAME
    val h_patt :ASM_AST.PATT -> PATT
    val h_term :ASM_AST.TERM -> TERM
    val F_rule :(NAME, PATT, TERM, RULE) ASM_AST.RULE' -> RULE
  end ) : sig
    val rule  : ASM_AST.RULE -> T.RULE
  end =
struct
  structure LiftRuleSpec :RULE_ALGEBRA = struct
    structure S = ASM_AST
    structure T = struct
      open T
      type RULE' = (NAME, PATT, TERM, RULE) ASM_AST.RULE'
    end

    val h_name = T.h_name
    val h_patt = T.h_patt
    val h_term = T.h_term

    fun h_rule F (ASM_AST.RulePos (pos, t)) = T.F_rule (F t)

    open ASM_AST
  end

  structure LiftRule = RuleInduction (LiftRuleSpec)

  val rule = LiftRule.rule
end
