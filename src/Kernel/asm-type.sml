(*
##
## "asm-type.ml", G. Del Castillo, July 1998
##
##   
*)


(* =================================================================== *\

NOTE:

'TypeParam' denotes universally quantified type variables: thus, we
  can avoid explicitly introducing type quantifiers in the type syntax
  (this would be too much overhead, as they can only occur as 'head'
  of a type)

'TypeVar' denotes free type variables instead: they arise during
  type checking, but never occur in signatures (as they are converted
  into TypeParams by the type closure operation)

\* =================================================================== *)


signature ASM_TYPE =
sig
  type TYPE
  val BaseType  :string * TYPE list -> TYPE
  val TypeParam :string -> TYPE
  val FuncType  :TYPE * TYPE -> TYPE
  val RuleType  :TYPE
  val TypeVar   :int * TYPE -> TYPE       (* should not be used, in general *)

  val selBaseType   :TYPE -> (string * TYPE list) option
  val selFuncType   :TYPE -> (TYPE * TYPE) option
  val selTypeParam  :TYPE -> string option
  val isRuleType    :TYPE -> bool
  val isPureTypeVar :TYPE -> bool

  val Bool   :TYPE
  val Int    :TYPE
  val Float  :TYPE
  val String :TYPE
  val Unit   :TYPE

  val List   :TYPE -> TYPE
  val Set    :TYPE -> TYPE
  val Map    :TYPE * TYPE -> TYPE
  val Tuple  :TYPE list -> TYPE

  datatype FUNCTION_KIND =
    Static | Dynamic | Derived | External | CTL

  type GET_TYPEALIAS = (string -> TYPE * TYPE)

  val noTypealias :GET_TYPEALIAS

  exception EqualType
  val equalType         :GET_TYPEALIAS -> TYPE * TYPE -> bool
  val toString          :TYPE -> string
  val isMonomorphicType :TYPE -> bool

  structure ParSet  :ORD_SET
  val typeparamsSet :TYPE -> ParSet.set

  structure VarSet  :ORD_SET
  val typevarsSet   :TYPE -> VarSet.set

  exception Unify
  exception IncompatibleType of TYPE * TYPE
  val freshTypeVar :unit -> TYPE
  val unify        :GET_TYPEALIAS -> TYPE * TYPE -> unit
  val expand       :GET_TYPEALIAS -> TYPE -> TYPE
  val fullyExpand  :GET_TYPEALIAS -> TYPE -> TYPE


  type 'a MAPPING = {
    RuleType  : 'a,
    BaseType  : string * TYPE list -> 'a,
    FuncType  : TYPE * TYPE -> 'a,
    TypeParam : string -> 'a,
    TypeVar   : int * TYPE -> 'a
  }

  val apply :'a MAPPING -> TYPE -> 'a

  type 'a MORPHISM = {
    RuleType  : 'a,
    BaseType  : string * 'a list -> 'a,
    FuncType  : 'a * 'a -> 'a,
    TypeParam : string -> 'a,
    TypeVar   : int * TYPE -> 'a
  }

  val inductive :'a MORPHISM -> TYPE -> 'a

  exception Domain and Range
  val domain :TYPE -> TYPE
  val range  :TYPE -> TYPE

  val typeClosure :TYPE -> TYPE
end


structure ASM_Type (* :ASM_TYPE *) =
struct
  (* --- types and constructors for ASM-SL types --- *)

  datatype TYPE' =
    RuleType'
  | BaseType' of string * (TYPE' ref) list
  | FuncType' of (TYPE' ref) * (TYPE' ref)
  | TypeParam' of string
  | Indirection' of TYPE' ref
  | TypeVar' of (int * TYPE' ref)

  type TYPE = TYPE' ref

  datatype FUNCTION_KIND =
   Static | Dynamic | Derived | External | CTL
						     
  fun functionKindToString Static = "static"
    | functionKindToString Dynamic = "dynamic"
    | functionKindToString Derived = "derived"
    | functionKindToString External = "external"
    | functionKindToString CTL = "CTL"

  type GET_TYPEALIAS = (string -> TYPE * TYPE)

  val RuleType                = ref RuleType'
  fun BaseType (C, T_list)    = ref (BaseType' (C, T_list))
  fun FuncType (T_dom, T_cod) = ref (FuncType' (T_dom, T_cod))
  fun TypeParam s             = ref (TypeParam' s)
  fun Indirection T           = ref (Indirection' T)
  fun TypeVar (i, self_T)     = self_T

  fun selBaseType (ref (BaseType' (C, Ts))) = SOME (C, Ts)
    | selBaseType _ = NONE

  fun selFuncType (ref (FuncType' (T_dom, T_ran))) = SOME (T_dom, T_ran)
    | selFuncType _ = NONE

  fun selTypeParam (ref (TypeParam' s)) = SOME s
    | selTypeParam _ = NONE

  fun isRuleType (ref RuleType') = true
    | isRuleType _ = false

  fun isPureTypeVar (ref (TypeVar' (i, T as ref T'))) = (T' = TypeVar' (i, T))
    | isPureTypeVar _ = false


  (* --- predefined types --- *)

  fun TupleType [ (ref (BaseType' ("_TUPLE", L'))) ] = TupleType L'
    | TupleType [ T ]  = T
    | TupleType T_list = BaseType ("_TUPLE", T_list) 

  val Bool           = BaseType ("BOOL", [])
  val Int            = BaseType ("INT", [])
  val Float          = BaseType ("FLOAT", [])
  val String         = BaseType ("STRING", [])
  fun List T         = BaseType ("LIST", [ T ])
  fun Set T          = BaseType ("SET", [ T ])
  fun Map (dom, ran) = BaseType ("MAP", [ dom, ran ])
  fun Tuple T_list   = TupleType T_list
  val Unit           = TupleType []


  (* --- function to check equality for simple monomorphic types --- *)
  
  fun isBoolType T =
    case (!T) of
      BaseType' ("BOOL", _) => true
    | _ => false

  fun isUnitType T =
    case (!T) of
      BaseType' ("_TUPLE", []) => true
    | _ => false


  (* --- data structures needed by this module --- *)

  structure ParSet = struct
    open StringSet
    val Union    = List.foldl union empty
  end


  structure TypeVar = struct
    structure IntSet = IntBinarySet

    val free = ref ((IntSet.empty) :IntSet.set)
    val top  = ref 1

    fun allocate () =
      if IntSet.isEmpty (!free)
      then let val result = !top
           in top := !top + 1;
              result
           end
      else let val result = valOf (IntSet.find (fn _ => true) (!free))  (* valOf safe here: set is not empty! *)
	   in free := IntSet.delete (!free, result);
              result
           end

    fun remove n =
      if (n < !top)
      then free := IntSet.add (!free, n)
      else ()

    fun new () =
      let val a = allocate ()
          val selfref  = TypeParam ""   (* content irrelevant (s. 2 lines below), but must be no constant! *)
          val result   = TypeVar' (a, selfref)
          val _        = selfref := result
      in selfref
      end

    exception Var
    fun self_type (_ :int, self_T :TYPE) = self_T
    fun var (ref (TypeVar' pair)) = pair
      | var _ = raise Var
  end  


  val freshTypeVar = TypeVar.new


  structure TypeVarOrder = struct
    type ord_key = int * TYPE
    fun compare ((i1,_), (i2,_)) = Int.compare (i1, i2)
  end

  structure VarSet = struct
    structure SetOfVar = BinarySetFn ( TypeVarOrder )
    open SetOfVar
    val Union = List.foldl union empty
  end


  (* --- structural induction over types --- *)

  fun K c _ = c

  type 'a MAPPING = {
    RuleType  : 'a,
    BaseType  : string * TYPE list -> 'a,
    FuncType  : TYPE * TYPE -> 'a,
    TypeParam : string -> 'a,
    TypeVar   : int * TYPE -> 'a
  }

  fun apply (F :'a MAPPING) (T :TYPE) :'a  =
    case (!T) of
      Indirection' T           => apply F T
    | RuleType'                => #RuleType F
    | BaseType' (C, T_list)    => #BaseType F (C, T_list)
    | FuncType' (T_dom, T_cod) => #FuncType F (T_dom, T_cod)
    | TypeParam' s             => #TypeParam F s
    | TypeVar' (i, T)          => #TypeVar F (i, T)

  type 'a MORPHISM = {
    RuleType  : 'a,
    BaseType  : string * 'a list -> 'a,
    FuncType  : 'a * 'a -> 'a,
    TypeParam : string -> 'a,
    TypeVar   : int * TYPE -> 'a
  }

  fun inductive (F :'a MORPHISM) (T :TYPE) :'a =
    case (!T) of
      Indirection' T           => inductive F T
    | RuleType'		       => #RuleType F
    | BaseType' (C, T_list)    => #BaseType F (C, map (inductive F) T_list)
    | FuncType' (T_dom, T_cod) => #FuncType F (inductive F T_dom, inductive F T_cod)
    | TypeParam' s             => #TypeParam F s
    | TypeVar' (i, T)          => #TypeVar F (i, T)


  (* --- some functions defined by structural induction --- *)

  val typeparamsSet = inductive {
    RuleType  = ParSet.empty,
    BaseType  = ParSet.Union o #2,
    FuncType  = ParSet.union,
    TypeParam = ParSet.singleton,
    TypeVar   = K ParSet.empty
  }

  val typevarsSet = inductive {
    RuleType  = VarSet.empty,
    BaseType  = VarSet.Union o #2,
    FuncType  = VarSet.union,
    TypeParam = K VarSet.empty,
    TypeVar   = VarSet.singleton
  }

  val show_base_type =
    fn ("_TUPLE", [])     => "()"
     | ("_TUPLE", T_list) => concat [ "(", (List_.output " * " T_list), ")" ]
     | (C, [])            => C
     | (C, T_list)        => concat [ C, " (", (List_.output "," T_list), ")" ]

  val toString = inductive {
    RuleType  = "<RULE>",
    BaseType  = show_base_type,
    FuncType  = fn (T_dom, T_cod) => concat [ T_dom, " -> ", T_cod ],
    TypeParam = fn s => "'" ^ s,
    TypeVar   = fn (i,_) => "'_a" ^ (Int.toString i)
  }

  val isMonomorphicType = inductive {
    RuleType  = true,
    BaseType  = (List.all Misc.id) o #2,
    FuncType  = fn (x, y) => x andalso y,
    TypeParam = K false,
    TypeVar   = K false
  }

  (* --- type substitution --- *)

  fun substitute ((i :int, self_T :TYPE), T :TYPE) =
    case (!T) of
      Indirection' T' => substitute ((i, self_T), T')
    | TypeVar' _      => ( self_T := Indirection' T; TypeVar.remove i )
    | _               => ( self_T := !T; TypeVar.remove i )


  (* --- instantiate type schemes --- *)

  local
    fun replace_typeparams table = inductive {
      RuleType  = RuleType,
      BaseType	= BaseType,
      FuncType	= FuncType,
      TypeParam	= fn s => Option.valOf (StringMap.find (table, s)),      (* !!! dangerous !!! *)
      TypeVar	= TypeVar
    }
  in
    fun instanceOf T =
      let val instance_table = ParSet.foldr
            (fn (s, tab) => StringMap.insert (tab, s, TypeVar.new ()))
            (StringMap.empty) (typeparamsSet T)
      in replace_typeparams instance_table T
      end

    fun instanceOfPair (T1, T2) =
      let val instance_table = ParSet.foldr
            (fn (s, tab) => StringMap.insert (tab, s, TypeVar.new ()))
             (StringMap.empty) (ParSet.union (typeparamsSet T1, typeparamsSet T2))
      in (replace_typeparams instance_table T1, replace_typeparams instance_table T2)
      end

    fun instanceOfList T_list =
      let val instance_table = ParSet.foldr
            (fn (s, tab) => StringMap.insert (tab, s, TypeVar.new ()))
             (StringMap.empty) (ParSet.Union (map typeparamsSet T_list))
      in map (replace_typeparams instance_table) T_list
      end
  end


  (* --- cleanup types (remove indirection nodes) --- *)

  val cleanupType = inductive {
    RuleType  = RuleType,
    BaseType  = BaseType,
    FuncType  = FuncType,
    TypeParam = TypeParam,
    TypeVar   = TypeVar
  }


  (* --- compute universal closures of types --- *)

  local
    fun next_name param_set s =
      let (*
	  fun next_name' (L, len) =
            if L = []
            then ["a"]
            else if L = List.tabulate (len, fn _ => "z")
            then List.tabulate (len + 1, fn _ => "a")
            else let val last_char = List.nth (explode L, len - 1)
                 in if last_char < "z"
                    then List.take (L, len - 1) @ [ chr (ord last_char + 1) ]
                    else next_name' (List.take (L, len - 1), len - 1) @ [ "a" ]
                 end
	  *)
          fun next_name' (s, len) =
            if s = ""
            then "a"
            else if s = implode (List.tabulate (len, fn _ => #"z"))
            then implode (List.tabulate (len + 1, fn _ => #"a"))
            else let val L = explode s
		     val last_char = List.nth (L, len - 1)
                 in if last_char < #"z"
                    then implode (List.take (L, len - 1) @ [ chr (ord last_char + 1) ])
                    else (next_name' (implode (List.take (L, len - 1)), len - 1)) ^ "a"
                 end
          val candidate = (fn es => next_name' (es, size es)) s
                          (*(fn es => concat (next_name' (es, length es))) (explode s)*)
      in if not (ParSet.member (param_set, candidate))
         then candidate
         else next_name param_set candidate
      end
    fun first_name param_set =
      next_name param_set ""
  in 
    val first_type_param_name = first_name
    val next_type_param_name  = next_name
    fun type_param_name_list S n =
      let fun F n =
	    if n <= 0 then []
	    else if n = 1 then [ first_type_param_name S ]
	    else let val L = F (n - 1)
		 in (next_type_param_name S (hd L)) :: L  end
      in rev (map (fn x => "'"^x) (F n))
      end



    fun typeClosure T =
      let val vars      = VarSet.listItems (typevarsSet T)
          val params    = typeparamsSet T
          val newparams = map TypeParam (List_.fixpoint (next_name params, first_name params) (length vars))
          val S         = ListPair.zip (vars, newparams)
      in map substitute S;
         cleanupType T
      end
  end


  (* --- unification algorithm: exceptions & aux. functions --- *)

  exception Unify
  exception IncompatibleType of TYPE * TYPE

  fun occurs i0 = inductive {
    RuleType  = false,
    BaseType  = (List.exists Misc.id) o #2,
    FuncType  = fn (x, y) => x orelse y,
    TypeParam = K false,
    TypeVar   = fn (i,_) => i = i0
  }

  (* --- type equality: exceptions & aux. functions --- *)

  exception EqualType


  (* --- expansion of type aliases --- *)

  exception GetTypealias

  fun noTypealias _ = raise GetTypealias

  fun expand (getTypealias :string -> TYPE * TYPE) (T :TYPE) =
  ( case (!T) of
      BaseType' (s, T_list) =>
      ( case s of
	  "_TUPLE" => T     (* [[ avoid searching for primitive type in signature (efficiency!) ]] *)
	| "BOOL"   => T
	| "INT"    => T
	| "FLOAT"  => T
	| "STRING" => T
	| "LIST"   => T
	| "SET"    => T
	| "MAP"    => T
	| _ => ( let val (T_l, T_r) = instanceOfPair ((getTypealias s) handle _ => raise GetTypealias)
		 in unify (fn s => raise GetTypealias) (T_l, T);
		      (* unify with no type aliases, i.e. with no expansion, in order to avoid an endless loop *)
		    expand getTypealias T_r
		 end
		 handle GetTypealias => T (* exception raised by getTypealias: this type is not an alias *) ) )
    | _ => T )

  (* --- type equality --- *)

  and equalType (getTypealias :string -> TYPE * TYPE) (T1, T2) =
    let val equalType = equalType getTypealias
	val (T1, T2)   = (expand getTypealias T1, expand getTypealias T2)
    in if T1 = T2
       then true      (* if references are equal, contents are equal too *)
       else case (!T1, !T2) of
	      (RuleType', RuleType') =>
		true
	    | (BaseType' (C1, T1_list), BaseType' (C2, T2_list)) =>
		(C1 = C2) andalso (equalTypeList getTypealias (T1_list, T2_list))
	    | (FuncType' (T1_dom, T1_cod), FuncType' (T2_dom, T2_cod)) =>
		equalType (T1_dom, T2_dom) andalso equalType (T1_cod, T2_cod)
	    | (TypeParam' s1, TypeParam' s2) =>
		s1 = s2
	    | (TypeVar' (i1,_), TypeVar' (i2,_)) =>
		if i1 <> i2
		then false
		else raise EqualType   (* invariant violated: i1 = i2 => T1 = T2 (as references) *)
	    | (Indirection' T1', Indirection' T2') => equalType (T1', T2')
	    | (Indirection' T1', _) => equalType (T1', T2)
	    | (_, Indirection' T2') => equalType (T1, T2')
	    | _ => false
    end
  and equalTypeList getTypealias (T1_list, T2_list) =
    case (T1_list, T2_list) of
      ([], []) =>
        true
    | (T1 :: T1_rest, T2 :: T2_rest) =>
        equalType getTypealias (T1, T2)
	andalso (equalTypeList getTypealias (T1_rest, T2_rest))
    | _ =>
        false


  (* --- type unification --- *)

  and unify (getTypealias :string -> TYPE * TYPE) (T1, T2) =
    let val equalType = equalType getTypealias
        fun disagreement_pair (T1, T2) =
	  let val (T1, T2) = (expand getTypealias T1, expand getTypealias T2)
	  in case (!T1, !T2) of
	       (Indirection' T1', Indirection' T2') => disagreement_pair (T1', T2')
	     | (Indirection' T1', _) => disagreement_pair (T1', T2)
	     | (_, Indirection' T2') => disagreement_pair (T1, T2')
	     | (TypeVar' (TV as (i,_)), _) =>
		 if not (occurs i T2) then (TV, T2) else raise IncompatibleType (T1, T2)
	     | (_, TypeVar' (TV as (i,_))) =>
		 if not (occurs i T1) then (TV, T1) else raise IncompatibleType (T1, T2)
	     | (BaseType' (C1, T1_list), BaseType' (C2, T2_list)) =>
		 if (C1 <> C2) orelse (length T1_list <> length T2_list)
		 then raise IncompatibleType (T1, T2)
		 else search_disagreement_pair (T1_list, T2_list)
	     | (FuncType' (T1_dom, T1_cod), FuncType' (T2_dom, T2_cod)) =>
		 if not (equalType (T1_dom, T2_dom))
		 then disagreement_pair (T1_dom, T2_dom)
		 else disagreement_pair (T1_cod, T2_cod)
	     | (_, _) => raise IncompatibleType (T1, T2)
          end
        and search_disagreement_pair (T1_list, T2_list) =
          case (T1_list, T2_list) of
            (T1 :: T1_rest, T2 :: T2_rest) =>
              if not (equalType (T1, T2))
              then disagreement_pair (T1, T2)
              else search_disagreement_pair (T1_rest, T2_rest)
          | _ => raise Unify
    in if not (equalType (T1, T2))
       then ( substitute (disagreement_pair (T1, T2)); unify getTypealias (T1, T2) )
       else ()  (* T1 := Indirection' T2 *)
    end


  (* --- fully expand type aliases within a type --- *)

  fun fullyExpand getTypealias = inductive {
    BaseType  = (expand getTypealias) o BaseType,
    RuleType  = RuleType,
    FuncType  = FuncType,
    TypeParam = TypeParam,
    TypeVar   = TypeVar
  }


  (* --- domain and range of a function type --- *)

  exception Domain and Range
  fun domain T = case (!(cleanupType T)) of FuncType' (T_dom, _) => T_dom | _ => raise Domain
  fun range T  = case (!(cleanupType T)) of FuncType' (_, T_cod) => T_cod | _ => raise Range

  (* --- function kind to string --- *)
  fun functionKindToString Static   = "static"
    | functionKindToString Dynamic  = "dynamic"
    | functionKindToString Derived  = "derived"
    | functionKindToString External = "external"
    | functionKindToString CTL      = "CTL"
end




structure ASM_TypeEnv =
struct
  exception Clash of string

  type TYPE = ASM_Type.TYPE
  structure Map = StringMap
  type TYPE_ENV = TYPE Map.map
  open Map

  fun union (env1, env2) = unionWithi (fn (x, y1, y2) => raise Clash x) (env1, env2)
  fun Union envs         = List.foldl union empty envs
  fun override (env1, env2) = unionWithi (fn (x, y1, y2) => y2) (env1, env2)

  fun toString env =
    let fun envPairToString (x, y) = x ^ " -> " ^ (ASM_Type.toString y)
    in ListFormat.fmt { init = "{ ", final=" }", sep=", ", fmt = envPairToString } (listItemsi env)
    end
end
