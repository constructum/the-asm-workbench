(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/regExp/match.sig,v $
 
   Interface to matching-algorithms (globber-style, regExp-style).

   This is the njsml109 version with pattern matching on characters. 
 
   $Date: 2001/03/30 13:39:58 $
   $Revision: 3.0 $
 
  ************************************************************************** *)

signature MATCH =
  sig
    exception bad_expr;
    val match : string -> string -> bool
  end;
 

