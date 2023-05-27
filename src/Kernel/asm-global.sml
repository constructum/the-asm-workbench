(*
##
## "asm-global.sml", G. Del Castillo, Jan 2000
##
##
##   
*)

signature ASM_GLOBAL =
sig
  exception Error of string Error.ERROR
  val error :string -> string -> exn option -> 'a

  val ASM_WB_HOME :unit -> string

  datatype UNDEF_DISCIPLINE = Permissive | Restrictive
  val undefDiscipline :UNDEF_DISCIPLINE ref

  val debugFlag :bool ref          (* debug mode *)
  val debugMsg  :string -> unit    (* message to be emitted only in debug mode *)

  type CONTEXT
  val ASM_SL_Context :ASM_Signature.SIGNATURE -> CONTEXT
  val isKeyword :CONTEXT -> string -> bool
  val idKind    :CONTEXT -> string -> ASM_AST.ID_KIND option
  val opStatus  :CONTEXT -> string -> ASM_AST.OP_STATUS option
  val addNames  :CONTEXT -> (string * (ASM_AST.ID_KIND * ASM_AST.OP_STATUS)) list -> CONTEXT
end


structure ASM_SL_Keyword =
struct
  val keywordSet = StringSet.fromList
  [ ":", "->", ":=", "==", "|",
    "MAP_TO_FUN",    "SET_TO_REL",    "FUN_TO_MAP",    "REL_TO_SET",
    "block",	     "case",	      "choose",	       "datatype",
    "derived",       "do",	      "dynamic",       "else",
    "elseif",	     "end",	      "endblock",      "endcase",
    "endchoose",     "enddo",	      "endif",	       "endlet",
    "endvar",	     "exists",	      "external",      "forall",
    "freetype",	     "fn",	      "function",      "if",
    "in",	     "initially",     "let",	       "of",
    "op",	     "op_l",	      "op_r",	       "otherwise",
    "predicate",     "relation",      "rule",	       "simultaneous",
    "skip",	     "static",	      "then",	       "tn",
    "transition",    "typealias",     "with",	       "var" ]

  fun isKeyword s =
    StringSet.member (keywordSet, s)

  (*
    the following alternative implementation does not seem to be faster:
    fun isKeyword s =
      case s of
	":"  => true
      | "->" => true
      | ...
      | "var" => true
      | _ => false
  *)
end


structure ASM_Global :ASM_GLOBAL =
struct
  open Misc Error
  exception Error of string Error.ERROR
  fun error fct what ex =
    raise Error { module = "ASM_Global", function = fct, problem = what, message = fn s => s, cause = ex }

  fun ASM_WB_HOME () =
    case OS.Process.getEnv "ASM_WB_HOME" of
      SOME s => s
    | NONE   => error "ASM_WB_HOME" "Environment variable ASM_WB_HOME undefined" NONE

  datatype UNDEF_DISCIPLINE = Permissive | Restrictive
  val undefDiscipline = ref Restrictive


  (* --- debug mode --- *)

  val debugFlag  = ref false
  fun debugMsg s = if (!debugFlag) then (print s; print "\n") else ()

  (* --- context for lexical analysis and pretty-printing --- *)

  type CONTEXT = {
    isKeyword : string -> bool,
    sgn       : ASM_Signature.SIGNATURE,
    moreNames : (ASM_AST.ID_KIND * ASM_AST.OP_STATUS) StringMap.map
  }

  fun isKeyword (ctx :CONTEXT) s =
    #isKeyword ctx s

  fun idKind (ctx :CONTEXT) s    =
    case ASM_Signature.find (#sgn ctx) s of
      SOME item => ASM_Signature.idKind (SOME item)
    | NONE      => Option.map #1 (StringMap.find (#moreNames ctx, s))

  fun opStatus (ctx :CONTEXT) s =
    case ASM_Signature.find (#sgn ctx) s of
      SOME item => ASM_Signature.opStatus (ASM_Signature.find (#sgn ctx) s)
    | NONE      => Option.map #2 (StringMap.find (#moreNames ctx, s))

  fun ASM_SL_Context sgn =
  ( { isKeyword  = ASM_SL_Keyword.isKeyword,
      sgn        = sgn,
      moreNames  = StringMap.empty } :CONTEXT )

  fun addNames (ctx :CONTEXT) nameList =
  ( { isKeyword = #isKeyword ctx,
      sgn       = #sgn ctx,
      moreNames = List.foldl StringMap.insert' (#moreNames ctx) nameList } :CONTEXT )

(*
  val (stdContext :ASM_Lexer.CONTEXT) = {
    isKeyword = ASM_Keyword.isKeyword,
    idKind    = ASM_Signature.idKind o (ASM_Signature.find (!ASM_Top.sign)),
    opStatus  = ASM_Signature.opStatus o (ASM_Signature.find (!ASM_Top.sign))
  }

*)
end
