signature ASM_DICTIONARY =
sig
  type TYPE
  type POSITION and LOCATION
  type FUNCTION_EXPR and TRANSITION_EXPR
  type CONSTRAINT
  type VALUE

  datatype CARRIER_SET =
    ML_Carrier of VALUE -> bool
  | FreetypeCarrier of string list
  | TypeAlias of TYPE * TYPE

  datatype FUNCTION =
    ML_StaticFunction of { interpretation : VALUE -> VALUE , memoFlag : bool }
  | ASM_SL_DataConstructor
  | ASM_SL_StaticFunction   of { definition : FUNCTION_EXPR }
  | ASM_SL_DynamicFunction  of { constraint : CONSTRAINT, initially  : FUNCTION_EXPR }
  | ASM_SL_DerivedFunction  of { definition : FUNCTION_EXPR }
  | ASM_SL_ExternalFunction of { constraint : CONSTRAINT }

  datatype NAMED_RULE = 
    ASM_SL_NamedRule of { definition : TRANSITION_EXPR }

  type DICTIONARY

  val empty    :DICTIONARY
  val override :DICTIONARY * DICTIONARY -> DICTIONARY

  val addType      :DICTIONARY * string * ((LOCATION option * POSITION option) * CARRIER_SET) -> DICTIONARY
  val addFunction  :DICTIONARY * string * ((LOCATION option * POSITION option) * FUNCTION)    -> DICTIONARY
  val addNamedRule :DICTIONARY * string * ((LOCATION option * POSITION option) * NAMED_RULE)  -> DICTIONARY

  val findType      :DICTIONARY * string -> ((LOCATION option * POSITION option) * CARRIER_SET) option
  val findFunction  :DICTIONARY * string -> ((LOCATION option * POSITION option) * FUNCTION) option
  val findNamedRule :DICTIONARY * string -> ((LOCATION option * POSITION option) * NAMED_RULE) option

  val locate :DICTIONARY * string -> (LOCATION * POSITION) option
end


structure ASM_Dictionary :ASM_DICTIONARY =
struct
  open ASM_AST

  type TYPE  = ASM_Type.TYPE
  type VALUE = ASM_Value.VALUE

  datatype CARRIER_SET =
    ML_Carrier of VALUE -> bool
  | FreetypeCarrier of string list
  | TypeAlias of TYPE * TYPE

  datatype FUNCTION =
    ML_StaticFunction of { interpretation : VALUE -> VALUE , memoFlag : bool }
  | ASM_SL_DataConstructor
  | ASM_SL_StaticFunction   of { definition : FUNCTION_EXPR }
  | ASM_SL_DynamicFunction  of { constraint : CONSTRAINT, initially  : FUNCTION_EXPR }
  | ASM_SL_DerivedFunction  of { definition : FUNCTION_EXPR }
  | ASM_SL_ExternalFunction of { constraint : CONSTRAINT }

  datatype NAMED_RULE = 
    ASM_SL_NamedRule of { definition : TRANSITION_EXPR }

  type DICTIONARY =
  { typeDict : ((LOCATION option * POSITION option) * CARRIER_SET) StringMap.map,
    funcDict : ((LOCATION option * POSITION option) * FUNCTION) StringMap.map,
    ruleDict : ((LOCATION option * POSITION option) * NAMED_RULE) StringMap.map }

  val empty :DICTIONARY =
  { typeDict = StringMap.empty,
    funcDict = StringMap.empty,
    ruleDict = StringMap.empty }

  fun override ( { typeDict = td1, funcDict = fd1, ruleDict = rd1 },
                 { typeDict = td2, funcDict = fd2, ruleDict = rd2 } ) :DICTIONARY =
  { typeDict = StringMap.unionWith #2 (td1, td2),
    funcDict = StringMap.unionWith #2 (fd1, fd2),
    ruleDict = StringMap.unionWith #2 (rd1, rd2) }

  fun addType ( { typeDict, funcDict, ruleDict }, key, typ ) :DICTIONARY =
  { typeDict = StringMap.insert (typeDict, key, typ), funcDict = funcDict, ruleDict = ruleDict }

  fun addFunction ( { typeDict, funcDict, ruleDict }, key, fct ) :DICTIONARY =
  { typeDict = typeDict, funcDict = StringMap.insert (funcDict, key, fct), ruleDict = ruleDict }

  fun addNamedRule ( { typeDict, funcDict, ruleDict }, key, nrl ) :DICTIONARY =
  { typeDict = typeDict, funcDict = funcDict, ruleDict = StringMap.insert (ruleDict, key, nrl) }

  fun findType      ({ typeDict, funcDict, ruleDict } :DICTIONARY, key) = StringMap.find (typeDict, key)
  fun findFunction  ({ typeDict, funcDict, ruleDict } :DICTIONARY, key) = StringMap.find (funcDict, key)
  fun findNamedRule ({ typeDict, funcDict, ruleDict } :DICTIONARY, key) = StringMap.find (ruleDict, key)

  fun locate ({ typeDict, funcDict, ruleDict } :DICTIONARY, key) :(LOCATION * POSITION) option =
  ( case StringMap.find (typeDict, key) of
      SOME ((SOME loc, SOME pos), _) => SOME (loc, pos)
    | NONE =>
      ( case StringMap.find (funcDict, key) of
	  SOME ((SOME loc, SOME pos), _) => SOME (loc, pos)
	| NONE =>
	  ( case StringMap.find (ruleDict, key) of
	      SOME ((SOME loc, SOME pos), _) => SOME (loc, pos)
	    | NONE => NONE
	    | _    => NONE)
	| _ => NONE )
    | _ => NONE )
end

