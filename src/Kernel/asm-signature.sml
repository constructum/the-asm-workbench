(*
##
## "asm-signature.sml", G. Del Castillo, May 1997-Feb 2000
##
##
##   
*)


structure ASM_Signature =
struct
  open ASM_Name

  exception Error of string Error.ERROR
  fun error fct what =
    raise Error
    { module = "ASM_Signature",
      function = fct,
      problem = what,
      message = Misc.id,
      cause = NONE }

  datatype SIGN_ITEM =
    Type of { arity : int }
  | TypeAlias of { lhs : TYPE, rhs : TYPE }
  | Func of { functionKind : FUNCTION_KIND, opStatus : OP_STATUS, constructor : bool, type_ : TYPE }
  | Rule of { type_ : TYPE }

  type SIGN_ENTRY = string * SIGN_ITEM

  type SIGNATURE = SIGN_ITEM StringMap.map


  val (empty :SIGNATURE) = StringMap.empty
  val (insert :SIGNATURE * string * SIGN_ITEM -> SIGNATURE)    = StringMap.insert
  val (insert' :(string * SIGN_ITEM) * SIGNATURE -> SIGNATURE) = StringMap.insert'
  val (singleton :string * SIGN_ITEM -> SIGNATURE)             = StringMap.singleton
  val (fromList :(string * SIGN_ITEM) list -> SIGNATURE)       = List.foldl insert' empty
  val (toList :SIGNATURE -> (string * SIGN_ITEM) list)         = StringMap.listItemsi
  val (override :SIGNATURE * SIGNATURE -> SIGNATURE)           = StringMap.unionWith #2
  val (union :SIGNATURE * SIGNATURE -> SIGNATURE)              = StringMap.unionWithi (fn (x,_,_) => error "union" x)
  val (defines :SIGNATURE * string -> bool)                    = StringMap.inDomain

  val find :SIGNATURE -> string -> SIGN_ITEM option            = fn sgn => fn s => StringMap.find (sgn, s)
  val findEntry :SIGNATURE -> string -> SIGN_ENTRY option      = fn sgn => fn s => Option.map (fn item => (s, item)) (find sgn s)

  val idKind :SIGN_ITEM option -> ID_KIND option =
    Option.map ( fn 
      Type _      => TypeKind
    | TypeAlias _ => TypeKind
    | Func _	  => FuncKind
    | Rule _	  => RuleKind )

  val opStatus :SIGN_ITEM option -> OP_STATUS option =
    fn SOME (Func { opStatus = opStatus, ... }) => SOME opStatus
     | _ => NONE

  val typeArity :SIGN_ITEM option -> int option =
    let val ERROR = error "typeArity"
    in fn SOME (Type { arity = arity }) => SOME arity
	| SOME (TypeAlias { lhs = T,... }) => ((SOME (length (#2 (valOf (selBaseType T))))) handle _ => NONE)
	| _ => NONE
    end

  val typeOf :SIGN_ITEM option -> TYPE option =
    let val ERROR = error "typeOf"
    in fn SOME (Func { type_ = T, ... }) => SOME T
	| SOME (Rule { type_ = T, ... }) => SOME T
	| _ => NONE
    end

  val functionKind :SIGN_ITEM option -> FUNCTION_KIND option =
    let val ERROR = error "functionKind"
    in fn SOME (Func { functionKind = kind, ... }) => SOME kind
	| _ => NONE
    end

  fun typealias (sgn :SIGNATURE) :ASM_Type.GET_TYPEALIAS =
    let val ERROR = error "typealias"
    in fn s => ( case (find sgn s) of
		   SOME (TypeAlias { lhs = T1, rhs = T2 }) => (T1, T2)
                 | _ => ERROR "(1)" )
    end

  val isConstructor :SIGN_ITEM option -> bool =
    let val ERROR = error "isConstructor"
    in fn SOME (Func { constructor = b, ... }) => b
	| _ => false
    end

  fun itemToString (si :SIGN_ITEM) :string -> string =
    case si of
      Type { arity = arity_ } =>
	let val L = ASM_Type.type_param_name_list (StringSet.empty) (arity_)
	in fn typeName => "type " ^ typeName ^ " (" ^ (List_.output ", " L) ^ ")"
	end
    | TypeAlias { lhs : TYPE, rhs : TYPE } =>
      ( fn typeName => "typealias " ^ (ASM_Type.toString lhs) ^ " == " ^ (ASM_Type.toString rhs) )
    | Func { functionKind = fk, opStatus = os, constructor = isCons, type_ = T }  =>
      ( fn funcName => (ASM_Type.functionKindToString fk) ^ " function " ^
	               ( case os of
                           NonInfix  => ""
                         | OpL prior => "op_l " ^ (Int.toString prior) ^ " "
                         | OpR prior => "op_r " ^ (Int.toString prior) ^ " " ) ^
	               funcName ^ " : " ^ (ASM_Type.toString (domain T)) ^
                       ( if equalType noTypealias (range T, Unit)
			 then ""
			 else " -> " ^ (ASM_Type.toString (range T)) ) )
    | Rule { type_ = T } =>
	fn ruleName => "rule " ^ ruleName ^
		       ( if equalType noTypealias (domain T, Unit)
			 then ""
			 else " : " ^ (ASM_Type.toString (domain T)) )

  fun toString (sgn : SIGNATURE) =
    String.concat (map (fn (name, item) => itemToString item name ^ "\n") (StringMap.listItemsi sgn))

  (* sign_entry = (name, sign_item) *)
  fun entryToString (name, item) = itemToString item name
end
