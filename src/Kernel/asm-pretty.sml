(*
##
## "asm-pretty.sml", G. Del Castillo, Sep 1999-Feb 2000
##
##   Pretty-printer for ASM-SL
##
##   Note: pretty-printing of terms depends on the signature, as infix
##     operators must be treated specially. Here we use the top-level
##     signature for simplicity (be aware of this dependency).
##
##   Note: there are some irregularities in the pretty-printing of (term
##     and rule) applications. This is to avoid printing the argument ()
##     in nullary applications and duplication of parentheses in n-ary
##     applications with n > 1. That is the reason why functions 'app_term_',
##     'app_rule_' to print applications must be passed as arguments to
##     pretty-printing functions for generalized terms and rules (and therefore
##     the function 'app' is exported: it is very useful when writing pretty
##     printers for user-defined generalized terms/rules, see examples).
##
##   To do: perhaps something can be done to avoid proliferation of
##     parentheses in terms when many infix operators are used (priority: low).
##
*)


(* === structure ASM_Pretty ================================================= *)

structure ASM_Pretty (*:ASM_PRETTY*) =
struct
  open Misc ASM_Global ASM_AST Pretty

  type CONTEXT = ASM_Global.CONTEXT

  type PROBLEM = string
  exception Error of (LOCATION option * POSITION * PROBLEM) Error.ERROR

  fun message (location, position, problem) =
    let val where_ = String_.replace "$1:$2:\n"
	               [ ASM_Location.toErrorString (valOf location) handle Option => "--",
			 ASM_Position.toErrorString position ]
        val what_  = problem
    in where_ ^ "  Pretty printer -- " ^ what_ ^ "\n"
    end

  fun error fct (firstPos, lastPos) what =
    let fun makePos () = ASM_Position.Pos { first = firstPos, last = lastPos }
    in raise Error
       { module = "ASM_Pretty",
	 function = fct,
	 problem = (NONE, makePos (), what),        (* ??? *)
	 message = message,
	 cause = NONE }
    end


  val concat = List.concat

  fun getPatt (PattPos (_, p')) = p'
  fun getTerm (TermPos (_, t')) = t'
  fun getRule (RulePos (_, R')) = R'
  fun getFunctionExpr (FunctionExprPos (_, FE')) = FE'
  fun getTransitionExpr (TransitionExprPos (_, TE')) = TE'

  fun blo0 pps = blo (0, pps)
  fun blo2 pps = blo (2, str "  " :: pps)

  (* --- see remarks at the beginning of this file... ------------- *)

  fun getOpStatus (ctx :CONTEXT) s =
    case idKind ctx s of
      SOME TypeKind => NonInfix
    | SOME RuleKind => NonInfix
    | SOME FuncKind => ((valOf (opStatus ctx s)) handle Option => NonInfix)
    | NONE => NonInfix


  (* --- shared by many pretty printing functions ----------------- *)

  fun commaList_ elem_ []         = []
    | commaList_ elem_ [x]        = [ elem_ x ]
    | commaList_ elem_ (x :: xs') = elem_ x :: str "," :: brk 1 :: commaList_ elem_ xs'

  fun list_ sep elem_ []         = []
    | list_ sep elem_ [x]        = [ elem_ x ]
    | list_ sep elem_ (x :: xs') = elem_ x :: (sep (list_ sep elem_ xs'))

  (* --- function kinds, types, and names ------------------------- *)

  fun functionKind_ kind =
    case kind of
      Static   => str "static"
    | Dynamic  => str "dynamic"
    | Derived  => str "derived"
    | External => str "external"
    | CTL      => str "CTL"

  fun type_ (parenthesizeProd :bool) ty =
    let fun typeList_ flag = list_ (fn rest => str "," :: brk 1 :: rest) (type_ flag)
        fun prodList_ flag = list_ (fn rest => str " *" :: brk 1 :: rest) (type_ flag)
        fun baseType (parenthesizeProd :bool) (tycon, tyargs) = case tycon of
              "_TUPLE" => if parenthesizeProd
		          then blo0 [ str "(", blo0 (prodList_ true tyargs), str ")" ]
			  else if List.null tyargs
                          then blo0 [ str "(", str ")" ]
                          else blo0 (prodList_ true tyargs)
            | _        => if tyargs = []
                          then str tycon
                          else blo0 [ str tycon, str " (", blo0 (typeList_ false tyargs), str ")" ]
    in ASM_Type.apply {
         RuleType   = str "<RULE>",
         BaseType   = fn (tycon, tyargs) => baseType parenthesizeProd (tycon, tyargs),
         FuncType   = fn (ty1, ty2) => blo0 [ type_ false ty1, str " -> ", type_ false ty2 ],
         TypeParam  = fn s => str ("'" ^ s),
         TypeVar    = fn (i, T) => if isPureTypeVar T   (* [[ (!T' <> ASM_Type.TypeVar' (i, T')) ]]*)
				   then str ("'a" ^ Int.toString i)
				   else type_ true T
       } ty
    end

  val type_ = type_ false


  fun opStatus_ opstat = ASM_Name.showOpStatus opstat

  fun name__ nm = str (ASM_Name.showName nm)

  fun infixed_name__ (opStat, id) =
    case opStat of
      OpL prior => blo0 [ str "op_l ", str (Int.toString prior), str " ", str id ]
    | OpR prior => blo0 [ str "op_r ", str (Int.toString prior), str " ", str id ]
    | NonInfix  => str id

  val name_ = name__    (* usually type constraints are parenthesized,
                           except in the constructor list within freetype declarations *)


  (* --- shared by terms and patterns ----------------------------- *)

  fun tuple_ elem_ t_list =
    blo0 [ str "(", blo0 (commaList_ elem_ t_list), str ")" ]

  fun type_annotation_ type_ elem_ (T, e) =
    (* blo0 [ str "(", elem_ e, str ":", type_ T, str ")" ] *)
    elem_ e


  (* --- shared by patterns, terms, and rules --------------------- *)

  fun app_ (ctx :CONTEXT) (getTupleElems :'elem -> 'elem list option) (elem_ :'elem -> Pretty.t) (f :NAME, e :'elem) =
    case getTupleElems e of
      NONE    => blo0 [ name_ f, str " (", elem_ e, str ")" ]
    | SOME [] => name_ f
    | SOME e_list =>
        if (length e_list = 2) andalso (getOpStatus ctx (idStr f) <> NonInfix)
        then let val (e1,e2) = (hd e_list, hd (tl e_list))
             in blo0 [ str "(", blo0 [ elem_ e1, brk 1, str (idStr f ^ " "), elem_ e2 ], str ")" ] end
        else blo0 [ name_ f, str " ", elem_ e ]


  (* --- patterns ------------------------------------------------- *)

  fun patt_' ctx getPatt (type_, name_, patt_) p' =
    let fun app_patt_ (f, p) = app_ ctx
	  (fn p => (case getPatt p of TuplePatt ps => SOME ps | _ => NONE))
	  patt_ (f, p)
    in case p' of
         PattType (T, p) => type_annotation_ type_ patt_ (T, p)
       | Placeholder     => str "_"
       | VarPatt s       => str s
       | AppPatt (f, p)  => app_patt_ (f, p)
       | TuplePatt ps    => tuple_ patt_ ps
    end

  fun patt_ ctx p =
    patt_' ctx getPatt (type_, name_, patt_ ctx) (getPatt p)


  (* --- shared by terms and rules -------------------------------- *)

  fun cond_ guard_ body_ G_b_list =
    let fun cond' _ _ _ [] = [ str "endif" ]
          | cond' kw guard_ body_ ((G,b) :: G_b_list') = 
             str (kw ^ " ") :: guard_ G :: line_brk
              :: str "then " :: body_ b :: line_brk
              :: cond' "elseif" guard_ body_ G_b_list'
    in case G_b_list of
         [] => blo0 []
       | _  => blo0 (cond' "if" guard_ body_ G_b_list)
    end

  fun let_ patt_ term_ body_ (p, t, b) =
    blo0 [ str "let ", patt_ p, str " == ", term_ t, line_brk,
           str "in ", body_ b, line_brk,
           str "endlet" ]

  fun case_ patt_ term_ body_ (t, p_b_list) =
    let fun caseList_ CL = case CL of
          []             => []
        | [(p,b)]        => [ patt_ p, str " :", line_brk, blo2 [ body_ b ] ]
        | (p,b) :: rest  => patt_ p :: str " :" :: line_brk :: blo2 [ body_ b, str " ;" ] :: line_brk
                            :: caseList_ rest
    in blo0 [ str "case ", term_ t, str " of", line_brk,
              blo2 (caseList_ p_b_list), line_brk,
              str "endcase" ]
    end

  (* --- terms ---------------------------------------------------- *)

  fun comprehension_ elem_ patt_ term_ lparen (p, A, G, t) rparen =
    blo0 [ str (lparen ^ " "), elem_ t, str " | ",
           blo0 [ patt_ p, brk 1, str "in ", term_ A, brk 1, str "with ", term_ G ],
           str (" " ^ rparen) ]

  fun quantifier_ patt_ term_ which (p, A, G) =
    blo0 [ str "(", str which, str " ", patt_ p, str " in ", term_ A, str " : ", term_ G, str ")" ]


  (* --- generalized terms ---------------------------------------- *)

  fun app_term_ ctx term_ (f, t) =
    app_ ctx (fn (TermPos (_, TupleTerm e_list)) => SOME e_list | _ => NONE) term_ (f, t)


  fun term_' ctx 
        (type_ :'type -> Pretty.t, name_ :'name -> Pretty.t, patt_: 'patt -> Pretty.t, term_ :'term -> Pretty.t)
	(app_term_ :'name * 'term -> Pretty.t)
	(t :('type,'name,'patt,'term) ASM_AST.TERM') :Pretty.t =
    let fun mapElem_ (t1, t2) = blo0 [ term_ t1, str " -> ", term_ t2 ]
        val term_' = term_' ctx (type_, name_, patt_, term_) app_term_
    in case t of
         TermType (T, t)        => type_annotation_ type_ term_ (T, t)
       | AppTerm (f, t)         => app_term_ (f, t)
       | TupleTerm t_list       => tuple_ term_ t_list
       | VarTerm v              => str v
       | CondTerm G_t_list      => cond_ term_ term_ G_t_list
       | LetTerm (p, t1, t2)    => let_ patt_ term_ term_ (p, t1, t2)
       | CaseTerm (t, p_t_list) => case_ patt_ term_ term_ (t, p_t_list)
       | ListComprTerm (t,(p,A,G)) => comprehension_ term_ patt_ term_ "[" (p, A, G, t) "]"
       | FSetComprTerm (t,(p,A,G)) => comprehension_ term_ patt_ term_ "{" (p, A, G, t) "}"
       | FMapComprTerm (e,(p, A, G)) => comprehension_ mapElem_ patt_ term_ "{" (p, A, G, e) "}"
       | FSetEnumTerm ts        => blo0 [ str "{", brk 1, blo0 (commaList_ term_ ts), brk 1, str "}" ]
       | FMapEnumTerm es        => blo0 [ str "{", brk 1, blo0 (commaList_ mapElem_ es), brk 1, str "}" ]
       | ExistsTerm (p, A, G)   => quantifier_ patt_ term_ "exists" (p, A, G)
       | ForallTerm (p, A, G)   => quantifier_ patt_ term_ "forall" (p, A, G)
    end

  fun term_ ctx t =
    term_'  ctx  (type_, name_, patt_ ctx, term_ ctx) (app_term_ ctx (term_ ctx)) (getTerm t)


  (* --- rules ---------------------------------------------------- *)

  fun update_rule_ term_ (t_l, t_r) =
    blo0 [ term_ t_l, brk 1, str ":= ", term_ t_r ]

  fun block_ (block_delimiters : bool) rule_ R_list =
    let val rule_ = rule_ true
        fun ruleList_ []         = []
          | ruleList_ [R]        = [ rule_ R ]
          | ruleList_ (R :: Rs') = rule_ R :: line_brk :: ruleList_ Rs'
    in case R_list of
         [] => str "skip"
       | _  => if block_delimiters
               then blo0 [ str "block", line_brk,
                           blo2 (ruleList_ R_list), line_brk,
                           str "endblock" ]
               else blo0 (ruleList_ R_list)
    end


  fun rule_with_range_ patt_ term_ rule_ which (p, A, G, R) whichEnd =
    blo0 [ str which, str " ", patt_ p, str " in ", term_ A, str " with ", term_ G, line_brk,
           blo2 [ rule_ R ], line_brk,
           str whichEnd ]


  fun rule_' ctx (block_delim :bool) 
        (name_ :'name -> Pretty.t, patt_: 'patt -> Pretty.t, term_ :'term -> Pretty.t, rule_ :'rule -> Pretty.t)
        (mkTerm :('type,'name,'patt,'term) ASM_AST.TERM' -> 'term)
        (app_rule_ :('name * 'term -> Pretty.t))
	(R :('name,'patt,'term,'rule) ASM_AST.RULE') :Pretty.t =
    let val rule_  = fn (b : bool) => rule_
        val block_ = block_ block_delim rule_
        val rule0  = rule_ false
    in case R of
	 UpdateRule ((f,t), t') => update_rule_ term_ (mkTerm (AppTerm (f, t)), t')

                (* update_rule_ term_ (TermPos (NONE, AppTerm (f, t)), t') *)
       | BlockRule Rs           => block_ Rs
       | CondRule GRs           => cond_ term_ rule0 GRs
       | LetRule (p, t1, R)     => let_ patt_ term_ rule0 (p, t1, R)
       | CaseRule (t, pRs)      => case_ patt_ term_ rule0 (t, pRs)
       | ChooseRule ((p,A,G),R) => rule_with_range_ patt_ term_ rule0 "choose" (p,A,G,R) "endchoose"
       | ForallRule ((p,A,G),R) => rule_with_range_ patt_ term_ rule0 "do forall" (p,A,G,R) "enddo"
       | AppRule (r, t)         => app_rule_ (r, t)
    end

  fun rule_ ctx (block_delim :bool) R =
    rule_' ctx block_delim (name_, patt_ ctx, term_ ctx, rule_ ctx block_delim)
    (fn t => TermPos (NONE,t))
    (app_term_ ctx (term_ ctx))
    (getRule R)


  val rule_ = fn ctx => rule_ ctx true



  (* --- function and transition expressions ---------------------- *)

  fun skip_patt_type (PattPos (_, p), fnp) =
    case p of
      PattType (_, p')  => skip_patt_type (p', fnp)
    | _ => fnp p

  fun functionExpr_' ctx (patt_, term_) E :Pretty.t =
    case E of
      LambdaTerm (p, t) =>
      ( skip_patt_type ( p, fn
          TuplePatt [] => term_ t
        | TuplePatt p_list => blo0 [ str "fn ", patt_ p, str " -> ", line_brk, blo2 [term_ t] ]
        | _ => blo0 [ str "fn (", patt_ p, str ") -> ", line_brk, blo2 [term_ t] ] ) )
    | SetToRel t => blo0 [ str "SET_TO_REL ", term_ t ]
    | MapToFun t => blo0 [ str "MAP_TO_FUN ", term_ t ]

  fun functionExpr_ ctx E =
    functionExpr_' ctx (patt_ ctx, term_ ctx) (getFunctionExpr E)

  fun transitionExpr_' ctx (patt_, rule_) E :Pretty.t =
    case E of
      LambdaRule (p, R) =>
      ( skip_patt_type ( p, fn
          TuplePatt [] => rule_ R
        | TuplePatt p_list => blo0 [ str "tn ", patt_ p, str " -> ", line_brk, blo2 [rule_ R] ]
        | _ => blo0 [ str "tn (", patt_ p, str ") -> ", line_brk, blo2 [rule_ R] ] ) )

  fun transitionExpr_ ctx E =
    transitionExpr_' ctx (patt_ ctx, rule_ ctx) (getTransitionExpr E)


  (* --- definitions ---------------------------------------------- *)

  fun typename_with_params (T, []) = [ str T ]
    | typename_with_params (T, params) =
        [ str (T^" (") ] @ (commaList_ (fn s => str ("'"^s)) params) @ [ str ")" ]

  fun typealias_def_ (typename, params, T) =
    blo0 ( (str "typealias " :: typename_with_params (typename, params)) @ [ str " == ", line_brk,
	   blo2 [ type_ T ] ] )

  fun datatype_def_ T_params_constructors_list =
    let fun pp_constructor ((opStatus, id), T) =
              [ str (opStatus_ opStatus ^ id) ] @
              ( ( if (T = ASM_Type.Unit)
                  then []
                  else [ str " : ", type_ T ] )
                handle _ => [] )
        fun pp_constructors constructors =
          case constructors of
            [] => []
          | [cons_name] => pp_constructor cons_name
          | cons_name :: cons_names => pp_constructor cons_name @ [ str ",", line_brk ] @
                                       (pp_constructors (cons_names))
        fun one_datatype_def (T, params, constructors) =
	  (typename_with_params (T, params)) @ [ str " == {", line_brk ] @
	  [ blo2 (pp_constructors constructors), line_brk,
	    str "}" ]
	fun simultaneous_defs T_params_constructors_list =
	  case T_params_constructors_list of
            []  => []
	  | [triple] => [ blo0 (one_datatype_def triple) ]
          | triple::rest => blo0 (one_datatype_def triple) :: line_brk :: simultaneous_defs rest
    in case T_params_constructors_list of
         [(T, params, constructors)] =>
           blo0 ([ str "freetype " ] @ (one_datatype_def (T, params, constructors)))
       | _ => 
           blo0 [ str "freetypes {", line_brk,
                  blo2 (simultaneous_defs T_params_constructors_list), line_brk,
                  str "}" ]
    end


  fun formals_ ctx p :Pretty.t list =
  ( skip_patt_type ( p, fn
      TuplePatt [] => []
    | TuplePatt p_list => [ str " ", patt_ ctx p ]
    | _ => [ str " (", patt_ ctx p, str ")" ] ) )

  fun function_def_ ctx (kind, name_constr_fexpr_list) =
    let fun finconstr_ (name:string) ( { RangeConstraint = SOME (FunctionExprPos (_, LambdaTerm (p, t))), ... }
                              :CONSTRAINT ) =
		    [ line_brk, blo2 ( [ str "with ", str (name) ] @
                                       (formals_ ctx p) @
                                       [ str " in ", term_ ctx t ] ) ]
	  | finconstr_ _ _ = []
        fun function_expr__ ( FunctionExprPos ( _, MapToFun
                              ( TermPos (_, FMapEnumTerm [ ( TermPos(_,TupleTerm[]), t ) ]) ) ) ) =
              term_ ctx t

	  | function_expr__ ( FunctionExprPos ( _, LambdaTerm
			      ( PattPos (_, TuplePatt []), t ) ) ) =

              term_ ctx t
          | function_expr__ fexpr = functionExpr_ ctx fexpr 
        fun initial_ fexpr =
	  [ line_brk, blo2 [ str "initially ", function_expr__ fexpr ] ]
        fun one_relation_def (name, T, fexpr) =
	  [ infixed_name__ name, str " : ", type_ T ] @
	  ( case kind of
	      Static   => [ str " == ", line_brk, blo2 [ function_expr__ fexpr ] ]
            | Derived  => [ str " == ", line_brk, blo2 [ function_expr__ fexpr ] ]
	    | External => []
	    | Dynamic  => initial_ fexpr
            | CTL      => [] )
        fun one_function_def (name as (_, id:string), finconstr, fexpr) =
	  [ infixed_name__ name ] @
	  ( case kind of
	      Static   => [ str " == ", line_brk, blo2 [ function_expr__ fexpr ] ]
            | Derived  => [ str " == ", line_brk, blo2 [ function_expr__ fexpr ] ]
	    | External => finconstr_ (id) finconstr
	    | Dynamic  => (finconstr_ (id) finconstr) @ (initial_ fexpr)
            | CTL      => [])
	fun simultaneous_defs name_constr_fexpr_list =
	  case name_constr_fexpr_list of
            []  => []
	  | [triple] => [ blo0 (one_function_def triple) ]
          | triple::rest => blo0 (one_function_def triple) :: line_brk :: simultaneous_defs rest
    in blo0 ( [ functionKind_ kind, str " " ] @
              ( case (name_constr_fexpr_list :((OP_STATUS * string) * CONSTRAINT * FUNCTION_EXPR) list) of
	          [(name as (_,id), constr, fexpr)] =>
		  ( case constr of
		      { TypeConstraint = SOME T, ... } =>
		      ( ( if equalType (ASM_Signature.typealias (#sgn ctx)) (ASM_Type.range T, Bool)
			  then str "relation " :: one_relation_def (name,ASM_Type.domain T,fexpr)
			  else str "function " :: one_function_def (name,constr,fexpr) )
                        handle _ => [] )
		    | _ => str "function " :: one_function_def (name,constr,fexpr) )
                | _ =>
		  [ str "functions {", line_brk,
		    blo2 (simultaneous_defs name_constr_fexpr_list), line_brk,
		    str "}" ] ) )
    end


  fun rule_def_ ctx (name :string, texpr :TRANSITION_EXPR) :Pretty.t =
    blo0 [ str "transition ", str name, str " == ", line_brk,
	   blo2 [ transitionExpr_ ctx texpr ] ]


  fun def_' ctx (type_, functionKind_, opStatus_, name_, (* finite_range_constraint,*)
		        functionExpr_, transitionExpr_, def_) D :Pretty.t =
    case D of
      DefSeq D_list => blo0 (map (fn D => blo0 [ def_ D, line_brk, line_brk ]) D_list)
    | FreetypeDef T_params_constructors_list => datatype_def_ T_params_constructors_list
    | TypealiasDef (typename, params, T) => typealias_def_ (typename, params, T)
    | FunctionDef (kind, name_constr_fexpr_list) => function_def_ ctx (kind, name_constr_fexpr_list)
    | RuleDef (name, texpr) => rule_def_ ctx (name, texpr)

  fun def_ ctx (DefPos (_, D')) =
    def_' ctx
      (type_, functionKind_, name_, opStatus_, functionExpr_ ctx, transitionExpr_ ctx, def_ ctx) D'

  (* --- type environments ---------------------------------------- *)

  fun typeEnv_ (env :ASM_TypeEnv.TYPE_ENV) =
    let val L = ASM_TypeEnv.listItemsi env
        fun envElem (x, T) = blo0 [ str x, str " : ", type_ T ]
    in blo0 [ str "{", brk 1, blo0 (commaList_ envElem L), brk 1, str "}" ]
    end

  (* --- exported functions --------------------------------------- *)


  val functionKind = functionKind_

  val name  = name_
  val infixedName = infixed_name__

  val patt  = patt_
  val term  = term_
  val rule  = rule_

  val app   = app_

  val term' = term_'
  val rule' = rule_'  

  val functionExpr   = functionExpr_
  val transitionExpr = transitionExpr_

  val def = def_
end
