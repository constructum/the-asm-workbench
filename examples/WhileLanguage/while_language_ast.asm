//////////////////////////////////////////////////
//
// Operational semantics of the "while"-language
//
//   while_language_ast.asm:
//     abstract syntax trees
//

typealias ID == STRING

freetype EXPR == {
  Con : INT,
  Var : ID,
  App : ID * [EXPR],
  Let : ID * EXPR * EXPR
}

freetype STMT == {
  Seq    : [STMT],
  If     : EXPR * STMT,
  While  : EXPR * STMT,
  Assign : ID * EXPR,
  Input  : ID,
  Output : EXPR
}


static function display_list (list, separator) ==
  case list of
         [] : "" ;
      [ x ] : x ;
    x :: xs : x ## separator ## " " ## display_list (xs, separator)
  endcase

static function display_expr (t) ==
  case t of
    Con (x)         : int_to_string (x) ;
    Var (v)         : v ;
    App (f, t_list) : f ## " (" ## display_list ([ display_expr (t) | t in t_list ], ",") ## ")" ;
    Let (x, t1, t2) : "let " ## x ## " = " ## (display_expr (t1)) ## " in " ## (display_expr (t2)) ## " endlet"
  endcase

simultaneous {
  static function display_stmt (stmt) ==
    case stmt of
      Seq (stmt_list) : "{ " ## (display_stmt_list (stmt_list)) ## " }" ;
      While (t, stmt) : "while " ## (display_expr (t)) ## " do " ## (display_stmt (stmt)) ;
      If (t, stmt)    : "if " ## (display_expr (t)) ## " then " ## (display_stmt (stmt)) ;
      Assign (v, t)   : v ## " := " ## (display_expr (t)) ## ";" ;
      Input (v)	      : "input " ## v ## ";" ;
      Output (t)      : "output " ## (display_expr (t)) ## ";"
    endcase

  static function display_stmt_list (stmt_list) ==
    display_list ([ display_stmt (stmt) | stmt in stmt_list ], " ")
}
