//////////////////////////////////////////////////
//
// Operational semantics of the "while"-language
//
//   while_language.asm:
//     semantics of expressions and statements
//


(* --- semantics of terms ---*)

freetype VALUE == {
  Bool : BOOL,
  Int  : INT
}

static function interpretation (f, args) ==
  case args of
    [] : case f of
      "true"  : Bool (true) ;
      "false" : Bool (false)
    endcase ;
    [ Bool (x) ] : case f of
      "not" : Bool (not (x))
    endcase ;
    [ Int (x) ] : case f of
      "abs" : Int (abs (x))
    endcase ;
    [ Int (x), Int (y) ] : case f of
      "+"   : Int (x + y) ;
      "-"   : Int (x - y) ;
      "*"   : Int (x * y) ;
      "div" : Int (x div y) ;
      "mod" : Int (x mod y) ;
      "="   : Bool (x = y) ;
      "!="  : Bool (x != y) ;
      "<"   : Bool (x < y) ;
      ">"   : Bool (x > y) ;
      "<="  : Bool (x <= y) ;
      ">="  : Bool (x >= y)
    endcase ;
    [ Bool (x), Bool (y) ] : case f of
      "and" : Bool (x and y) ;
      "or"  : Bool (x or y)
    endcase
  endcase

static function eval_expr_in_env (E, env) ==
  case E of
    Con (x)         : Int (x) ;
    Var (v)         : apply (env, v) ;
    App (f, E_list) : interpretation (f, [ eval_expr_in_env (E, env) | E in E_list ]) ;
    Let (x, E1, E2) : let E1_value == eval_expr_in_env (E1, env)
                      in eval_expr_in_env (E2, override (env, { x -> E1_value }))
                      endlet
  endcase


(* --- resources ---*)

// external function program0 :STMT
static function program ==
  Seq ([
    Input ("max"),
    Assign ("x", Con(1)),
    While (
      App ("<=", [ Var("x"), Var("max") ]),
      Seq ([
        If (App ("=", [ App ("mod", [ Var("x"), Con(2) ]), Con(0) ]), Output (Var("x"))),
        Assign ("x", App ("+", [ Var("x"), Con(1) ]))
      ])
    )
  ])


dynamic function curr_stmt  :STMT    initially program
dynamic function curr_cont  :[STMT]  initially []
dynamic function terminated :BOOL    initially false

dynamic function global_env :{ID -> VALUE}
initially emptymap


external function input     :VALUE
with input in { Int (i) | i in {1..10} }

dynamic function output     :[VALUE]   initially []


derived function eval_expr (E) ==
  eval_expr_in_env (E, global_env)


(* --- semantics of statements ---*)

rule Continue ==
  case curr_cont of
    stmt1 :: stmts : curr_stmt := stmt1
                     curr_cont := stmts ;
                [] : terminated := true
  endcase

rule ExecuteSeq (stmt_list) ==
  case stmt_list of
    stmt1 :: stmts : curr_stmt := stmt1
                     curr_cont := stmts @ curr_cont ;
                [] : Continue
  endcase

rule ExecuteIf (E, stmt) ==
  if eval_expr (E) = Bool (true)
  then curr_stmt := stmt
  else Continue
  endif

rule ExecuteWhile (E, stmt) ==
  if eval_expr (E) = Bool (true)
  then curr_stmt := stmt
       curr_cont := [ While (E, stmt) ] @ curr_cont
  else Continue
  endif

rule ExecuteAssign (v, E) ==
  block
    global_env := override (global_env, { v -> eval_expr (E) })
    Continue
  endblock

rule ExecuteInput (v) ==
  block
    global_env := override (global_env, { v -> input })
    Continue
  endblock

rule ExecuteOutput (E) ==
  block
    output := output @ [ eval_expr (E) ]
    Continue
  endblock


rule ExecuteStmt ==
  case curr_stmt of
    Seq (stmt_list) : ExecuteSeq (stmt_list) ;
    While (E, stmt) : ExecuteWhile (E, stmt) ;
    If (E, stmt)    : ExecuteIf (E, stmt) ;
    Assign (v, E)   : ExecuteAssign (v, E) ;
    Input (v)       : ExecuteInput (v) ;
    Output (E)      : ExecuteOutput (E)
  endcase


dynamic function a initially 0


rule Interpreter ==
  if not (terminated)
  then ExecuteStmt
  endif
