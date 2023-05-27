(*
## "asm-ast.ml", G. Del Castillo, Mar 1997
##
##   
*)

(* --- positions and locations -------------------------- *)

signature ASM_LOCATION =
sig
  datatype LOCATION =
    Primitive
  | Interactive of string
  | File of string

  val toString      :LOCATION -> string
  val toErrorString :LOCATION -> string
end


signature ASM_POSITION =
sig
  datatype POSITION =
    Pos of {
      first : (int * int),
      last  : (int * int)
    }

  val toString      :POSITION -> string
  val toErrorString :POSITION -> string

  val until  :POSITION * POSITION -> POSITION
  val until' :POSITION option * POSITION option -> POSITION option

  val fst :POSITION -> int * int
  val lst :POSITION -> int * int
end


structure ASM_Location :ASM_LOCATION =
struct
  datatype LOCATION =
    Primitive
  | Interactive of string
  | File of string

  fun toString Primitive = "[primitive]"
    | toString (Interactive s) = "[not in a file]"
    | toString (File s) = (File.mkRelative s)

  val toErrorString = toString
end


structure ASM_Position :ASM_POSITION =
struct
  datatype POSITION =
    Pos of
    { first : (int * int),
      last  : (int * int) }

  fun toString (Pos { first = (l1,c1), last = (l2,c2) }) =
    String_.replace "$1.$2-$3.$4" (map Int.toString [ l1, c1, l2, c2 ])

  fun toErrorString (Pos { first = (l1,c1), last = (l2,c2) }) =
    String_.replace "$1: [$1.$2-$3.$4]" (map Int.toString [ l1, c1, l2, c2 ])

  fun getLine (l, c) = l
  fun getChar (l, c) = c

  infix until until'

  fun (Pos pos1') until (Pos pos2')  = Pos { first = #first pos1', last = #last pos2' }

  fun (SOME pos1) until' (SOME pos2) = SOME (pos1 until pos2)
               | _ until' _ = NONE

  fun fst (Pos { first = first, ... }) = first
  fun lst (Pos { last = last, ... })   = last
end


(* --- abstract syntax trees ---------------------------- *)

structure ASM_AST (*:ASM_AST*) =
struct
  open ASM_Name ASM_Location ASM_Position

  type 'a POS_ANNOT = POSITION option * 'a

  datatype ('type, 'name, 'patt) PATT' =
    PattType of 'type * 'patt
  | Placeholder
  | VarPatt of string
  | AppPatt of 'name * 'patt
  | TuplePatt of 'patt list

  datatype PATT = PattPos of ((TYPE, NAME, PATT) PATT') POS_ANNOT

  fun AppPatt0 f0 = AppPatt (f0, PattPos (NONE, TuplePatt []))
  val nilPatt     = PattPos (NONE, AppPatt0 (Id "nil"))
  val unitPatt    = PattPos (NONE, TuplePatt [])

  type ('patt, 'range, 'guard) QUALIFIER = ('patt * 'range * 'guard)
    (* concrete syntax: "p in A with G" *)

  datatype ('type, 'name, 'patt, 'term) TERM' =
    TermType of 'type * 'term

  | VarTerm of string
  | AppTerm of 'name * 'term
  | TupleTerm of 'term list

  | CondTerm of ('term * 'term) list
  | LetTerm  of 'patt * 'term * 'term
  | CaseTerm of 'term * ('patt * 'term) list

  | ListComprTerm of 'term * (('patt, 'term, 'term) QUALIFIER)
  | FSetComprTerm of 'term * (('patt, 'term, 'term) QUALIFIER)
  | FMapComprTerm of ('term * 'term) * (('patt, 'term, 'term) QUALIFIER)

  | FSetEnumTerm of 'term list
  | FMapEnumTerm of ('term * 'term) list

  | ForallTerm of (('patt, 'term, 'term) QUALIFIER)
  | ExistsTerm of (('patt, 'term, 'term) QUALIFIER)


  datatype TERM = TermPos of ((TYPE, NAME, PATT, TERM) TERM') POS_ANNOT

  fun AppTerm0 f0 = AppTerm (f0, TermPos (NONE, TupleTerm []))
  val trueTerm    = TermPos (NONE, AppTerm0 (Id "true"))
  val falseTerm   = TermPos (NONE, AppTerm0 (Id "false"))
  val undefTerm   = TermPos (NONE, AppTerm0 (Id "undef"))

  val nilTerm     = TermPos (NONE, AppTerm0 (Id "nil"))
  fun consTerm pos (h, t) = TermPos (SOME pos, AppTerm (Id "::", TermPos (NONE, TupleTerm [ h, t ])))


  datatype ('name, 'patt, 'term, 'rule) RULE' =
    UpdateRule of ('name * 'term) * 'term
  | BlockRule of 'rule list

  | CondRule of ('term * 'rule) list
  | LetRule of 'patt * 'term * 'rule
  | CaseRule of 'term * ('patt * 'rule) list

  | ChooseRule of (('patt, 'term, 'term) QUALIFIER) * 'rule
  | ForallRule of (('patt, 'term, 'term) QUALIFIER) * 'rule

  | AppRule of 'name * 'term

  datatype RULE = RulePos of ((NAME, PATT, TERM, RULE) RULE') POS_ANNOT

  val skipRule = RulePos (NONE, BlockRule [])



  datatype ('patt, 'term) FUNCTION_EXPR' =
    LambdaTerm of 'patt * 'term
  | SetToRel of 'term
  | MapToFun of 'term

  datatype FUNCTION_EXPR = FunctionExprPos of ((PATT, TERM) FUNCTION_EXPR') POS_ANNOT


  datatype ('patt, 'rule) TRANSITION_EXPR' =
    LambdaRule of 'patt * 'rule

  datatype TRANSITION_EXPR = TransitionExprPos of ((PATT, RULE) TRANSITION_EXPR') POS_ANNOT


  type ('type, 'term, 'function_expr) CONSTRAINT' =
  { TypeConstraint   : 'type option,
    DomainConstraint : 'term option,
    RangeConstraint  : 'function_expr option }

  type CONSTRAINT = (TYPE, TERM, FUNCTION_EXPR) CONSTRAINT'

  val NoConstraint = { TypeConstraint = NONE, DomainConstraint = NONE, RangeConstraint = NONE }
  fun addTypeConstraint ({ DomainConstraint = dc, RangeConstraint = rc, ... } :CONSTRAINT)
                        (T :TYPE option) =
    { TypeConstraint = T, DomainConstraint = dc, RangeConstraint = rc }
  fun addDomainConstraint ({ TypeConstraint = tc, RangeConstraint = rc, ... } :CONSTRAINT)
                          (t :TERM option) =
    { TypeConstraint = tc, DomainConstraint = t, RangeConstraint = rc }
  fun addRangeConstraint ({ TypeConstraint = tc, DomainConstraint = dc, ... } :CONSTRAINT)
                         (FE :FUNCTION_EXPR option) =
    { TypeConstraint = tc, DomainConstraint = dc, RangeConstraint = FE }
  


  datatype ( 'type, 'function_kind, 'op_status, 'constraint, 'function_expr, 'transition_expr, 'def ) DEF' =
    DefSeq of 'def list
  | TypealiasDef of string * string list * 'type
  | FreetypeDef of (string * string list * (('op_status * string) * 'type) list) list
  | FunctionDef of 'function_kind * (('op_status * string) * 'constraint * 'function_expr) list
  | RuleDef of string * 'transition_expr

  datatype DEF = DefPos of ( ( TYPE, FUNCTION_KIND, OP_STATUS, CONSTRAINT,
			       FUNCTION_EXPR, TRANSITION_EXPR, DEF ) DEF' ) POS_ANNOT

  fun namesDefinedInDef (DefPos (_, D')) =
    case D' of
      DefSeq Ds                 => List.concat (map namesDefinedInDef Ds)
    | TypealiasDef (name, _, _) => [name]
    | FreetypeDef fts           => List.concat (map (fn (T,_,conss) => [T] @ (map (fn ((_,C),_) => C) conss)) fts)
    | FunctionDef (_, fds)      => map (fn ((_,name),_,_) => name) fds
    | RuleDef (name, _)         => [name]

  datatype DEF_BLOCK = DefBlock of LOCATION * DEF

  fun namesDefinedInDefBlock (DefBlock (_, D)) = namesDefinedInDef D
end

(* ---------------------------------------------------------------- *)
