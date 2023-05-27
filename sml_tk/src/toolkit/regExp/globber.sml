(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/regExp/globber.sml,v $
 
   A regular expression matcher. 

   $Date: 2001/03/30 13:39:58 $
   $Revision: 3.0 $
   Original Author: Ryan Stansifer <ryan@ponder.csci.unt.edu> 
   (Last modification by $Author: 2cxl $)
 
  ************************************************************************** *)

(*  globber.sml -- Regular expression matcher with globber-matcher syntax. *)
(*  see "man glob"  *)
(*  Ryan Stansifer (ryan@cs.unt.edu) at Sat Sep 18 11:14:39 1993  *)
(*  The function match takes a regular expression and 
    matches it against a string.    A regular expression has meta-symbols 
    "*", "?", "{", "}", "\\", ","
 
Examples:
 
Globber.match "dfg*fg{qwr,fgh}" "dfgbhjqwr";   

Globber.match "dfg*fg{qwr,fgh}" "dfgbhjfgqwr";   

Globber.match "dfg" "dfg";

*)
 


structure Globber: MATCH =
  struct
 
    val  exists     = List.exists
    fun  fold f l s = List.foldr f s l


    (* parse tree for regular expressions *)
    datatype Leaf = char of char | any | eoe;
    datatype Rex  =
      cat of Rex * Rex |             (* concatenation of two regular expressions *)
      epsilon          |             (* denotes set containing empty string      *)
      alt of Rex * Rex |
      star of Rex      |
      leaf of Leaf;
 
    local
      (*  Parser builders from Reade, page 216.  *)
 
      infixr 4 &&;
      infixr 3 ||;
      infix  0 ##;
 
      exception reject;
 
      fun (p  ##  f) s = let val (x,y) = p s in (f x, y) end;
      fun (p1 && p2) s = let val (x,s') = p1 s in (p2 ## (fn y => (x,y))) s' end
;
      fun (p1 || p2) s = (p1 s) handle reject => (p2 s);
 
      fun optional pr = ((pr ## (fn x => SOME x)) || (fn s => (NONE, s)));
      (* [These are not used in the grammar for regular expressions, but
          are useful in other grammars.]
      (* The argument s to sequence is critical to avoid infinite loop.  *)
      fun sequence pr s = (((pr && sequence pr)##(op ::)) || (fn s => ([], s))) s;
      fun one_or_more pr = (pr && sequence pr) ##(op ::)
      *)
 
      (*
         The following grammar is used to parse strings into regular expressions.
 
         rx ::= sx [ "," rx ]
 
         ax ::= char 
         ax ::= "*" 
         ax ::= "?" 
         ax ::= "{" rx "}" 
         ax ::= "\" meta 

         sx ::= ax [sx]
      *)
 
(* lexical base functions *)
        fun
          literal c (c1::s) = if c=c1 then (c,s) else raise reject |
          literal c _       = raise reject;
 
        fun is_meta c =
           exists (fn x => x=c) [#"*", #"?", #"{", #"}", 
				 #"\\", #","];
 
        fun
          character (c::s) = if is_meta (c) then raise reject else (c, s) |
          character (_)    = raise reject;
 
        fun
          meta (c::s) = if is_meta (c) then (c, s) else raise reject |
          meta (_)    = raise reject;

        val char_meta  =  character || ((literal #"\\") && meta  ## (fn (_,m) => m));
 
        val suffix = ((literal #"*")||(literal #"+")||(literal #"?"));
 
(* attribute functions *)
        fun
          f (r, NONE)       = r           |
          f (r, SOME (_,s)) = alt (r,s)   ;
 
        fun
          h (r, NONE)     = r                    |
          h (r, (SOME s)) = cat (r, s)           ;
 

(* cf grammar parsing functions *)
 
        fun
          st s = (sx                          ## (fn x => cat(x,leaf eoe))) s
        and
          rx s = (sx && (optional (literal #"," && rx)) ## f) s
        and
          sx s = (ax && (optional sx)                   ## h) s
        and
          ax s = (
            (character                        ## (fn c => leaf (char c)))
            ||
            ((literal #"*")                   ## (fn c => star(leaf(any))))
            ||
            ((literal #"?")                   ## (fn c => leaf(any)))
            ||
            ((literal #"{") && rx && (literal #"}")  ## (fn (_,(r,_)) => r))
            ||
            ((literal #"\\") && meta          ## (fn (_,c) => leaf (char c)))
          ) s;
 
    in
      (*  parse -- parse a string, character by *)
      (*  character into a regular expression *)
      local
        fun f (x,nil) = x | f _ = raise reject;
      in
        exception bad_expr;
        fun parse rex =  f (st (explode rex)) handle reject => raise bad_expr
      end;
    end;
 
 
 
    (* Compute the following positions *)
    fun E n = IntBinarySet.empty;
    fun update s (i,f) n = if i=n then IntBinarySet.union (s, f(i)) else f(n);
    fun comp (f,g) n = IntBinarySet.union (f n, g n);
 
    (* lookup -- find value associated with key in a list of pairs.  *)
    exception not_found;
    fun
        lookup (x, nil)              = raise not_found |
        lookup (x, (key,value)::rest)= if x=key then value else lookup (x,rest);
    (*
       dfs -- Compute:
           nullable
           first posion
           last postion
           mapping from dfs number to leaf element
           mapping from position to set of following positions
 
       See:  Aho, Sethi, Ullman, section 3.9
    *)
    fun
      dfs n (leaf x) =
        (false, n+1, IntBinarySet.singleton n, IntBinarySet.singleton n, [(n,x)], E) |
      dfs n (epsilon) = (true, n, IntBinarySet.empty, IntBinarySet.empty, nil, E)    |
      dfs n (star r) = 
        let
          val (_,d1,f1,l1,t,w) = dfs n r;
          val follow = fold (update f1) (IntBinarySet.listItems l1) w;
        in
          (true, d1, f1, l1, t, follow)
        end |
      dfs n (cat(r,s)) =
        let
          val (n1,d1,f1,l1,t1,w1) = dfs n r;
          val (n2,d2,f2,l2,t2,w2) = dfs d1 s;
          val first = if n1 then IntBinarySet.union (f1,f2) else f1;
          val last  = if n2 then IntBinarySet.union (l1,l2) else l2;
          val follow= fold (update f2) (IntBinarySet.listItems l1) (comp (w1,w2));
        in
          (n1 andalso n2, d2, first, last, t1@t2, follow)
        end  |
      dfs n (alt(r,s)) =
        let
          val (n1,d1,f1,l1,t1,w1) = dfs n r;
          val (n2,d2,f2,l2,t2,w2) = dfs d1 s;
          val nullable = n1 orelse n2;
          val follow = comp (w1, w2);
        in
          (nullable, d2, IntBinarySet.union (f1,f2), IntBinarySet.union (l1,l2),t1@t2,follow)
        end  ;
 
    type NFA_Type = {
      start:  IntBinarySet.set,
      edges:  Leaf Vector.vector,
      trans:  IntBinarySet.set Vector.vector,
      final:  int
    };
 
    fun construct (_,b,c,_,e,f) : NFA_Type = {
      start = c,
      edges = Vector.tabulate (b, (fn x=>lookup (x,e))),
      trans = Vector.tabulate (b,f),
      final = (b-1)
    };
 
 
    fun next (s,a,NFA: NFA_Type) =
      let
        val M = #edges NFA;
        val N = #trans NFA;
        fun
          p (any, x) = true |
          p (y,   x) = (x=y);
 
        fun f (x,y) = if p(Vector.sub (M,x),a) then x::y else y;
        val pos = fold f (IntBinarySet.listItems s) [];
        fun g (x,y) = IntBinarySet.union (y, Vector.sub (N, x));
      in
        fold g pos IntBinarySet.empty
      end;
 
    local
      fun
        loop ({final=f, ...}, state, nil) = IntBinarySet.member(state,f) |
        loop (NFA,            state, h::t)=
          (not(IntBinarySet.equal(state, IntBinarySet.empty))) andalso 
          loop (NFA, (next(state,char h,NFA)), t);
    in
      fun interpret (NFA as {start=s, ...}, x) = 
          loop (NFA, s, (String.explode x));
    end;
 
    (*
       A tip from La Monte H Yarroll <piggy@hilbert.maths.utas.edu.au> on
       Mon Apr 18 08:04:02 CDT 1994.
 
            fun match pat obj = interpret (construct (dfs 0 (parse pat)), obj);
 
       is much less efficient than:

    *)

    fun match pat =
      let
        val nfa = construct (dfs 0 (parse pat))
      in
        fn obj => interpret (nfa, obj)
      end;

    
 
  end;  (* structure Rex *)





