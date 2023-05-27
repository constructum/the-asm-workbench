structure ASM_Interface =
struct
  open TkTypes SmlTk

  val asmErrorMsgRef = ref (fn (_:exn) => ())
  fun asmErrorMsg ex = (!asmErrorMsgRef) ex

  val simpleMsgRef = ref (fn (_:string) => ())
  fun simpleMsg s = (!simpleMsgRef) s

  fun catchAndDismiss f args = (f args; ()) handle ex => asmErrorMsg ex
  fun catchAndForward f args = (f args) handle ex => (asmErrorMsg ex; raise ex)

  fun parseAndCheckTerm s = let val t = ASM.parseTerm' s in ASM.checkTerm' t; t end
  fun parseAndCheckRule s = let val R = ASM.parseRule' s in ASM.checkRule' R; R end

  fun parseAndCheckTermWithTypeConstraint T s =
    let val t   = ASM.parseTerm' s
	val T_t = ASM.checkTerm' t
    in ASM_Type.unify (ASM.typealias ()) (T, T_t);
       t
    end
end





functor MemoCell (MemoInfo :sig type A type B val f : A -> B end) =
struct
  open MemoInfo

  datatype CELL_STATE = UNEVALUATED | VALUE_IS of B | ERROR of exn
  datatype CELL = Cell of (A * CELL_STATE) ref

  fun new (x) = Cell (ref (x, UNEVALUATED))

  fun update (Cell (R as ref _), new_x) = ( R := (new_x, UNEVALUATED); Cell R )

  fun evaluate (Cell (R as ref (x, UNEVALUATED))) = ( R := (x, VALUE_IS (f x) handle ex => ERROR ex); Cell R )
    | evaluate (Cell (R as ref (x, ERROR ex))) = evaluate (Cell (ref (x, UNEVALUATED)))
    | evaluate (cell as Cell _) = cell

  fun invalidate (Cell (R as ref (x, _))) = ( R := (x, UNEVALUATED); Cell R )

  fun argument (Cell (ref (x, _))) = x

  fun value (Cell (ref (x, VALUE_IS y))) = y
    | value (Cell (ref (x, ERROR ex))) = raise ex
    | value (cell as Cell _) = value (evaluate cell)
end



structure ParsedTerm =
  MemoCell ( struct
    type A = string
    type B = ASM_AST.TERM
    val f  = ASM_Interface.parseAndCheckTerm
  end )

functor ParsedTermWithTypeConstraint (S :sig val T :ASM_Type.TYPE end) =
  MemoCell ( struct
    type A = string
    type B = ASM_AST.TERM
    val f = ASM_Interface.parseAndCheckTermWithTypeConstraint S.T
  end )

structure ParsedBooleanTerm =
  ParsedTermWithTypeConstraint ( struct
    val T = ASM_Type.Bool
  end )

structure ParsedRule =
  MemoCell ( struct
    type A = string
    type B = ASM_AST.RULE
    val f = ASM_Interface.parseAndCheckRule
  end )
