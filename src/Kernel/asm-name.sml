signature ASM_NAME =
sig
  include ASM_TYPE

  datatype ID_KIND =
    TypeKind
  | FuncKind
  | RuleKind
 
  datatype OP_STATUS =
    NonInfix
  | OpL of int
  | OpR of int
    
  datatype NAME =
    IntConst of int
  | FloatConst of real
  | StringConst of string
  | Id of string
  | FunToMap of string
  | RelToSet of string

  val showOpStatus :OP_STATUS -> string
  val idStr        :NAME -> string
  val showName     :NAME -> string
end


structure ASM_Name :ASM_NAME =
struct
  open ASM_Type

  datatype ID_KIND =
    TypeKind
  | FuncKind
  | RuleKind
 
  datatype OP_STATUS =
    NonInfix
  | OpL of int
  | OpR of int
    
  datatype NAME =
    IntConst of int
  | FloatConst of real
  | StringConst of string
  | Id of string
  | FunToMap of string
  | RelToSet of string

  fun idStr (Id s) = s
    | idStr (FunToMap s) = "FUN_TO_MAP " ^ s
    | idStr (RelToSet s) = "REL_TO_SET " ^ s
    | idStr _ = "[ASM_Name.id <???>]"

  fun showOpStatus NonInfix    = ""
    | showOpStatus (OpL prior) = "op_l " ^ Int.toString prior ^ " "
    | showOpStatus (OpR prior) = "op_r " ^ Int.toString prior ^ " "

  fun showName (IntConst i) = Int.toString i
    | showName (FloatConst r) = Real.toString r
    | showName (StringConst s) = "\"" ^ String.toString s ^ "\""         (* !!!! quoting / unquoting *)
    | showName (Id id) = id
    | showName (FunToMap id) = "FUN_TO_MAP " ^ id
    | showName (RelToSet id) = "REL_TO_SET " ^ id
end
