(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/markup.sml,v $
 
   sml_tk Generic Markup Language: writing down annotated texts.

   This module allows one to write down texts with embedded annotations in an 
   SGML-like format. 

   See std_markup.sml for a full-fledged instantiation of this generic
   markup language, and tests+examples/markup_ex.sml for a wee example.

   $Date: 2001/03/30 13:39:46 $
   $Revision: 3.0 $

   Author: cxl (Last modification by $Author: 2cxl $)

   (C) 1996, 1998, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)



functor SmlTkMarkup (Tags: TAGS) : 
    SMLTK_MARKUP (* where type widgetinfo= Tags.widgetinfo *) =

struct

    open SmlTk BasicUtil

    (* 
     * This defines the abstract syntax of a text with annotations in it. 
     * I won't bore you with a BNF, but roughly the syntax is as follows:
     *
     * elemStart(nm, a1,... an) is  <nm a1 ... an> 
     *    -- start of an "element" in SGML-speak.
     *       Note there must be  _no space_ betweem
     *       the opening < and the name nm, and nm has to start with _letter_;
     *       a1 to an are the arguments of the element
     *       
     * elemEnd nm is <\nm>
     *    -- the end of an element
     *
     * escape e is  &str;   
     *    -- the escape e denoted by str
     *
     * quote str is just the string str
     *)

    datatype anTextEl =  elemStart of string* string list
                       | quote     of substring
		       | escape    of string
		       | elemEnd   of string
	
    type annotatedText = anTextEl list


   (********************************************************************
    *
    * The parser 
    * 
    * The parser is extremely tolerant; it doesn't generate any errors, and
    * if it can't decipher something it will just leave it as a verbal
    * quote.
    *)

   structure Parser = 

   struct

       open Substring

       (*
	* The lexical elements 
	*)
       
       datatype Lexem = openEl | openEndEl | openEsc 
                        (* corresponding to <, <\ and &;  plus > and ; 
			 * which only become a lexem after one of these
			 *)


       (* handy slices *)
       fun sliceFromTo(t, i, n)= slice(t, i, SOME(n-i+1))
       fun sliceToEnd(t, i)    = slice(t, i, NONE)

       (* convert string to lowercase *)
       fun toLowerStr substr = 
                  String.implode (map Char.toLower (Substring.explode substr))


       fun nextIsAlpha t i = 
	   Char.isAlpha (sub(t, i+1)) handle Subscript => false 

       fun errorContext(t, i)  = 
	   let val t= sliceToEnd(t, i)
	   in  "'"^(if (size t)<25 then (string t)
	            else (string(slice(t, 0, SOME(25)))^"..."))^"'"
	   end
	       
       (* find first valid occurence of a lexem in t, starting from index i *)
	       
       (* returns either the index into the string right after the lexem, and 
	* the lexem, if there is one; or NONE and the index to the end of the 
	* string 
	*)
	       
       fun scanNextLex t i =
	   (case sub(t, i) of	  
		(* & and < are only valid lexems if followed by a letter *)
		#"&" => if nextIsAlpha t i
			    then (i-1, i+1, SOME openEsc)
			else scanNextLex t (i+1)
	      | #"<" => if nextIsAlpha t i
			    then (i-1, i+1, SOME openEl)
			else  if (sub(t, i+1))= #"\\"
				  then if nextIsAlpha t (i+1)
					   then (i-1, i+2, SOME openEndEl)
				       else scanNextLex t (i+2)
			      else scanNextLex t (i+1)
	      |  _   => scanNextLex t (i+1))
		handle Subscript => (i-1, i-1, NONE)
       (* have passed end of string *)

       (* find next occurence of > *)
       fun scanCloseEl t i = 
	   case sub(t, i) of
	       #">" => SOME (i+1)
	     |   _  => scanCloseEl t (i+1)
		     handle Subscript => NONE
			 
       fun scanCloseEsc t i =
	   case sub(t, i) of 
	       #";" => SOME (i+1)
	     |   _  => scanCloseEsc t (i+1)
		     handle Subscript => NONE
			 
       (* parse an "element", i.e. a thingy enclosed in '<' ... '>' 
	* i is supposed to be the index into t right after the opening bracket
	* parseEl returns the representation of the rest of t 
	*)
       fun parseEl t i =
	   let val unto = scanCloseEl t i
	   in  case unto of
	       SOME n => 
		   let val elText = toLowerStr(sliceFromTo(t, i, n-2))
		       val els    = String.tokens (Char.isSpace) elText
			   (* can rely on els being non-empty since
			    * nextIsAlpha was true when calling parseEl *)
		   in  (elemStart(hd els, tl els)):: (parseMain t n)
		   end
	     | NONE   => 
		   raise Tags.error 
		       ("Can't find closing '>' after "^(errorContext(t, i-1))) 
	   end

       and parseEndEl t i =
	   let val unto = scanCloseEl t i
	   in  case unto of 
	       SOME n => 
		   let val elText = toLowerStr(sliceFromTo(t, i, n-2))
		       val els    = String.tokens Char.isSpace elText
			   (* again, els has to be non-empty.
			    * We  could check here if there is more than
			    * one element and generate a warning.
			    * Or we could even keep a list of arguments. 
			    *)
		   in  (elemEnd(hd els)):: (parseMain t n)
		end
	     | NONE   => 
		  raise Tags.error
		      ("Can't find closing '>' after "^(errorContext(t, i-2)))
	   end
       
       (* parse an escape sequence, starting with '&' ... ';'
	* i is supposed the index into t right after the ampersand
	*)
       and parseEsc t i =
	   let val unto = scanCloseEsc t i
	   in  case unto of
	       SOME n => 
		   escape (Substring.string(sliceFromTo(t, i, n-2)))
		   :: (parseMain t n)
	     | NONE => (* can't find closing ; *)
		   raise Tags.error 
		       ("Can't find closing ';' after "^(errorContext(t, i-1)))
	   end
       
       and parseMain t i =
	   let val (j, n, lex) = scanNextLex t i
	       val rest        = case lex of
		   NONE           => []
		 | SOME openEl    => parseEl t n
		 | SOME openEsc   => parseEsc t n
		 | SOME openEndEl => parseEndEl t n	  
	   in  if (i<= j) then
		   (quote(sliceFromTo(t, i, j))):: rest
	       else
		   rest
	   end
 
       fun parse t = parseMain (full t) 0
	   
   end


   (* count position within a string *)
    val addpos =
	let fun cntone (thischar, (line, char)) =
	    if (StringUtil.isLinefeed thischar) then (line+1, 0) 
			       else (line, char+1)
	in Substring.foldl cntone
	end

    (* like split, but stop after the first element satisfying p *)
    fun splitfirst p []     = (NONE, [])
      | splitfirst p (x::xs)= if (p x) then
	                          (SOME x, xs)
			      else
				  let val (f, r)= splitfirst p xs
				  in  (f, x:: r)
				  end


    (* the four components of the consEl's second argument are the following:
     * - the first is the stack of unprocessed open elements, along with their
     *   position within the text;
     * - the second is current position within the text;
     * - the third is the text content up to here;
     * - and the last is the list of annotations built up to here.
     * 
     * As it stands, opening elements with no matching close are discarded. 
     * This can be changed easily.
     *)

    fun consEl wid (quote q, (oe, c, s, al)) = 
	          (oe, addpos c q, s^(Substring.string q), al)
      | consEl wid (escape e, (oe, c, s, al)) = 
	          (case Tags.escape e of
		      SOME esc =>
			  let val estr= Tags.textForEsc esc
			      val nuc = addpos c (Substring.full estr)
			      val ean = Tags.annotationForEsc esc (Mark c,
								   Mark nuc)
			  in  (oe, nuc, s^estr,
			       case ean of SOME t=> t::al | NONE=> al)
			  end
		     | NONE => 
			   let val estr= 
			       case e of (* the three predefined escape seqs *)
				   "amp" => "&"
				 | "lt"  => "<"
                                 | "gt"  => ">" 
				 |  _    => 
			           (Tags.warning ("Unknown escape sequence '"^e^
						"' (left untouched).");
				    "&"^e^";")
			   in (oe, addpos c (Substring.full estr), s^estr, al)
			   end)
      | consEl wid (elemStart els, (oe, c, s, al)) =
	          ((els, c):: oe, c, s, al)
      | consEl wid (elemEnd el, (oe, c, s, al)) =
	          let val (m, rest)= splitfirst (fn ((nm, args), _)=>
						 nm= el) oe 
		  in  case m of
		          NONE =>
			      (Tags.warning ("Closing tag '<"^el^">' doesn't match any opening tag");
			       (oe, c, s, al))
			| SOME((tgnm, args), pos) => 
			      case Tags.matchingTag tgnm of
				  SOME tg =>
				      (let val nuan= Tags.annotationForTag 
					                  tg args wid
                                                            (Mark pos, Mark c)
				       in  (rest, c, s, nuan:: al)
				       end
				       handle (Tags.AnnotationError str) =>
					     (Tags.warning str; 
					      (rest, c, s, al)))
				| NONE =>
				      (Tags.warning ("Unknown tag <"^tgnm^"> ignored.");
				       (rest, c, s, al))
		  end

    type widgetinfo= Tags.widgetinfo
			  			  			      
    fun getAnnText wid str =
	let val (openEls, (cols, rows), text, anns)= foldl (consEl wid)
	                                        ([], (1,0), "", []) 
					        (Parser.parse str) 
            val _ = if ((length openEls)> 0)
		    then (Tags.warning ("Unclosed open elements found.")) 
		    else ()				       
	in  AnnoText{len= SOME(cols, rows), 
		     str= text, annotations= anns}
	end
	
end

