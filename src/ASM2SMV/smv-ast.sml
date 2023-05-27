structure SMV_AST =
struct
  type ATOM = string
  type ID   = string

  datatype TERM =
    AtomTerm of ATOM
  | Dot of ATOM * ATOM

  datatype CONSTANT =
    AtomConst of ATOM
  | Number of int
  | True
  | False

  datatype EXPR =
    Const of CONSTANT
  | Term of TERM
  | Next of TERM
  | App of string * EXPR list
  | Case of (EXPR * EXPR) list

  datatype TYPE =
    Boolean
  | Enum of CONSTANT list
  | SubRange of int * int   
  | ModType of ATOM * EXPR list
  | ProcType of ATOM * EXPR list

  datatype ASSIGN_LHS =
    LhsTerm of TERM
  | LhsNext of TERM
  | LhsInit of TERM

  datatype CTL_FORM =
    Expr of EXPR         (* simple boolean expression without temporal operators *)
  | CTL_E of PATHFORM
  | CTL_A of PATHFORM

  and PATHFORM =
    CTL_X of CTL_FORM
  | CTL_F of CTL_FORM
  | CTL_G of CTL_FORM
  | CTL_U of CTL_FORM * CTL_FORM

  datatype DECLARATION =
    Var of (TERM * TYPE) list
  | Init of EXPR
  | Trans of EXPR
  | Define of (ATOM * EXPR) list
  | Spec of CTL_FORM
  | Fairness of CTL_FORM
  | Assign of (ASSIGN_LHS * EXPR) list

  datatype MODULE =
    Module of ATOM * ATOM list * DECLARATION list

  datatype PROGRAM =
    Program of MODULE list     (* [Module (main_state,...),  *)
end                            (*  Module (behavior,....)]   *)
