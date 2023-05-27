signature SMV_Pretty_sig =
sig

(*-- building_block_fcts: --------------------*)

  val proc_ : SMV_AST.PROGRAM -> Pretty.t 

end


structure SMV_Pretty (*:SMV_Pretty_sig*) =
struct
  open SMV_AST
  open Pretty

  exception ProcPrint_Error of string
  exception ExprPrint_Error of string

  fun output_list L = List_.output ", " L

  fun term_ T =
    case T of
      AtomTerm (a)  => a
    | Dot (s, a)    => s ^ "." ^ a

  fun asm_int_to_smv_int (i :int) =
    if i >= 0 then Int.toString i else ("-" ^ (Int.toString (~i)))

  fun constant_ C =
    case C of
      AtomConst s => s
    | Number i    => asm_int_to_smv_int i
    | True	  => "TRUE"
    | False	  => "FALSE"

  fun constants_ C_list =
   case C_list of
      []           => [ str "" ]
    | [c1]         => [ str(constant_ c1) ]
    | c1 :: c_rest => str(constant_ c1) :: str"," :: brk 1 :: constants_ c_rest 

  fun expr_ E =
    case E of
      Const (c)              => str(constant_ c)
    | Term  (t)              => str(term_ t)
    | App ("not", [e1])      => blo(0, [ str"(!", expr_ e1,  str")" ])
    | App ("and", [e1, e2])  => blo(0, [ str"(", expr_ e1, str" &", brk 1, expr_ e2, str")" ])
    | App ("=>", [e1, e2])   => blo(0, [ str"(", expr_ e1, str" ->", brk 1, expr_ e2, str")" ])
    | App ("or",  [e1, e2])  => blo(0, [ str"(", expr_ e1, str" |", brk 1, expr_ e2, str")" ])
    | App ("->",  [e1, e2])  => blo(0, [ str"(", expr_ e1, str" ->", brk 1, expr_ e2, str")" ])
    | App ("<->", [e1, e2])  => blo(0, [ str"(", expr_ e1, str" <->", brk 1, expr_ e2, str")" ])
    | App ("=",   [e1, e2])  => blo(0, [ str"(", expr_ e1, str" =", brk 1, expr_ e2, str")" ])
    | App ("!=",  [e1, e2])  => blo(0, [ str"(!(", expr_ e1, str" =", brk 1, expr_ e2, str"))" ])
    | App ("<",   [e1, e2])  => blo(0, [ str"(", expr_ e1, str" <", brk 1, expr_ e2, str")" ])
    | App (">",   [e1, e2])  => blo(0, [ str"(", expr_ e1, str" >", brk 1, expr_ e2, str")" ])
    | App ("<=",  [e1, e2])  => blo(0, [ str"(", expr_ e1, str" <=", brk 1, expr_ e2, str")" ])
    | App (">=",  [e1, e2])  => blo(0, [ str"(", expr_ e1, str" >=", brk 1, expr_ e2, str")" ])
    | App ("+",   [e1, e2])  => blo(0, [ str"(", expr_ e1, str" +", brk 1, expr_ e2, str")" ])
    | App ("-",   [e1, e2])  => blo(0, [ str"(", expr_ e1, str" -", brk 1, expr_ e2, str")" ])
    | App ("*",   [e1, e2])  => blo(0, [ str"(", expr_ e1, str" *", brk 1, expr_ e2, str")" ])
    | App ("/",   [e1, e2])  => blo(0, [ str"(", expr_ e1, str" /", brk 1, expr_ e2, str")" ])
    | App ("mod", [e1, e2])  => blo(0, [ str"(", expr_ e1, str" mod", brk 1, expr_ e2, str")" ])

(*!!!!!!!!!! not clean: should produce CTL formulae instead !!!!!!!!!!!*)
    | App ("AG", [e1])      => blo(0, [ str"AG (", expr_ e1,  str")" ])
    | App ("AF", [e1])      => blo(0, [ str"AF (", expr_ e1,  str")" ])
    | App ("EG", [e1])      => blo(0, [ str"EG (", expr_ e1,  str")" ])
    | App ("EF", [e1])      => blo(0, [ str"EF (", expr_ e1,  str")" ])
    | App ("AX", [e1])      => blo(0, [ str"AX (", expr_ e1,  str")" ])
    | App ("EX", [e1])      => blo(0, [ str"EX (", expr_ e1,  str")" ])

    | App (f, [e1])         => blo(0, [ str f, str" (", expr_ e1,  str")" ])

    | Next (t)               => blo(0, [ str"next (", str(term_ t), str")" ])
    | Case (exprList)        => blo(0, [ (*brk 2,*) str"case", line_brk, blo (2, ( str"  " :: caseList_ exprList)),                                          line_brk, str"esac" (*, line_brk*) ]) 

    | _                      => raise (ExprPrint_Error "\nwrong SMV-Expression\n")

  and 
(*
  caseList_ CL =
    case CL of
      []                => [ str "" ]
    | [(e1, e2)]        => [ expr_ e1, str" : ", expr_ e2, str";" ]
    | (e1, e2) :: rest  => expr_ e1 :: str" : " :: expr_ e2 :: str";" :: line_brk :: caseList_ rest
*)
  caseList_ CL =
    case CL of
      []                => [ str "" ]
    | [(e1, e2)]        => [ expr_ e1, str" : ", line_brk, blo (2, [str "  ", expr_ e2, str";"]) ]
    | (e1, e2) :: rest  => expr_ e1 :: str" : " :: line_brk ::
			   blo (2, [str "  ", expr_ e2, str";"]) :: line_brk ::
			   caseList_ rest




  fun type_ T =
    case T of
      Boolean                 => str "boolean"
    | Enum const_list	      => blo(0, [ str"{", blo (0, (constants_ const_list)), str"}" ] )
    | SubRange (a, b)         => blo(0, [ str (Int.toString a), str"..", str (Int.toString b) ] )
(*  wird im folgenden nicht benutzt  *)
    | ModType (atom, [])      => str atom
    | ModType (atom, E_list)  => blo(0, [ str atom, str"(", blo(0,(map expr_ E_list)), str")" ])
    | ProcType (atom, [])     => blo(0, [ str"process ", str atom ] )
    | ProcType (atom, E_list) => blo(0, [ str"process ", str atom, str"(", blo(0, (map expr_ E_list)), str")" ])


 fun varList_ VL =
    case VL of
      []                   => [ str "" ]
    | (t1, type1) :: rest  => str(term_ t1) ::  str" : " :: type_ type1 :: str";" :: line_brk :: varList_ rest 

  fun defList_ DL =
    case DL of
      []              => str ""
    | (a, e) :: rest  => blo(0, [ brk 2, str a, str " := ", expr_ e, str ";", line_brk, defList_ rest ])

  fun assignLhs_ Lhs =
    case Lhs of
      LhsTerm (t)    => [str(term_ t)]
    | LhsNext (t)    => [str"next (", str(term_ t), str")" ]
    | LhsInit (t)    => [str"init (", str(term_ t), str")" ]

  fun assign_ AL =
    case AL of
      []               => [ str "" ]
    | (a, e) :: rest   => (line_brk :: assignLhs_ a) @ (str ":= " :: line_brk :: str "  " :: expr_ e :: str ";" :: assign_ rest)

  fun ctl_ F =
    case F of
      Expr E   => expr_ E
    | CTL_E PF => blo (2, [ str"E", pathform_ PF ])
    | CTL_A PF => blo (2, [ str"A", pathform_ PF ])

  and pathform_ PF =
    case PF of
      CTL_X F0       => blo (0, [ str"X (", ctl_ F0, str ")" ])
    | CTL_F F0       => blo (0, [ str"F (", ctl_ F0, str ")" ])
    | CTL_G F0       => blo (0, [ str"G (", ctl_ F0, str ")" ])
    | CTL_U (F1, F2) => blo (0, [ str" [",  ctl_ F1, str " U ", ctl_ F2, str "]" ])



  fun decl_ D =
    case D of
      Var ( vList )      => blo (2, [ str "VAR", line_brk, blo(0, (varList_ vList)), line_brk ])
    | Init (e)           => blo (2, [ str "INIT", line_brk, expr_ e, line_brk ])
    | Trans (e)          => blo (2, [ str "TRANS", line_brk, expr_ e, line_brk ])
    | Define ( dList )   => blo (2, [ str "DEFINE", line_brk, defList_ dList ])
    | Spec (f)           => blo (2, [ str "SPEC", line_brk, ctl_ f, line_brk ])
    | Fairness (f)       => blo (2, [ str "FAIR", line_brk, ctl_ f, line_brk ])
    | Assign (alist)     => blo (2, [ str "ASSIGN", line_brk, blo(0, (assign_ alist)), line_brk ])

  fun declList_ DL =
    case DL of
      []         => str ""
    | d :: rest  => blo (0, [ decl_ d, line_brk, declList_ rest ])


  val dashLine =
     blo(0, [ str "--------------------------------------------------------------------------------", line_brk ])

  fun list_of (open_paren, L, close_paren) =
    case L of
      [] => str ""
    | _  => blo(0, [ str open_paren, str(output_list L), str close_paren ])

  fun module_ (Module (name, params, decl_list)) =
    blo (0, [ str "MODULE ", 
              str name,
	      list_of (" (", params, ")"),
              line_brk, line_brk,
              declList_ decl_list ] )


  fun moduleList_ M_list =
    case M_list of
      []           => [ str "" ]
    | [M]          => [ dashLine, module_ M, dashLine ]
    | M1 :: M_rest => dashLine :: module_ M1 :: (moduleList_ M_rest)

  fun proc_ (Program M_list) = blo(0, moduleList_ M_list)
end
