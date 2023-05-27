(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/markup.sig,v $
 
   The sml_tk Markup Language: writing down annotated texts.

   This module allows one to write down texts with embedded
   annotations in an SGML-like format. It supplies a functor which
   generates a parser for a given format. 

   $Date: 2001/03/30 13:39:46 $
   $Revision: 3.0 $
   Author: cxl (Last modification by $Author: 2cxl $)

   (C) 1996, 1997, 1998, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)


(*
 * In general, the parser recognizes SGML "elements" of the form
 * <elname arg1 ... argn> ... <\elname> 
 * where elname is the name of the element and arg1 ... argn is a list
 * of arguments. It further recognizes escape sequences of the form
 * &esc; where esc can be lt for "<" and amp for "&". Other escape
 * sequences are left as they are. The characters < and & only start
 * elements or escape sequences of succeeded with a letter. 
 *
 * An elements like above is called a "tag". Tags are translated into
 * sml_tk annotations, as defined by the argument signature of the
 * functor.
 *
 * See tests+examples/markup_ex.sml for an example of how to
 * instantiate this functor. 
 *)

signature TAGS =
    sig
	type tag

	type widgetinfo

	val matchingTag      : string-> tag option
	
	val annotationForTag : tag ->string list-> widgetinfo-> 
	                                     (SmlTk.Mark* SmlTk.Mark)-> 
	                                                    SmlTk.Annotation
              (* generate an annotation for a tag. The second argument is 
	       * the list of arguments given to the tag, the third argument
	       * is some "widgetinfo" (as passed to getAnnText),
	       * followed by the marks for the current annotation. widgetinfo
	       * can be any old type; most of the time, one will want to pass
	       * the WidId of the text widget, but there may be more infor-
	       * mation one needs to pass to the functions bound to the tags.
	       * 
	       * annotationForTag can raise the following exception if there
	       * is an error (eg. not enough arguments), where the argument
	       * is a warning messaged displayed with warning below.
	       *
	       * NB: All elements, their arguments and the escape sequences
	       *     below are uniformly converted to _lower case_. Hence,
	       *     matchingTag and escape only must match for lower case
	       *     arguments. 
	       *)

	exception AnnotationError of string

	(* Escape sequences -- analogous to the above, but escapes do not
	 * have arguments, and on the other hand generate a fixed text.
	 * So for example, textForEscape &alpha; might be "a", and the 
	 * annotation would be TATag[Font Symbolfont] to generate the greek
	 * letter alpha.
	 *
	 * In contrast to tags, escape sequences are _case sensitive_! 
	 * The reason for this is purely practical, it allows us to 
	 * conveniently and intuitively distinguish e.g. &omega; and &Omega;
	 *
	 * Lastly, the escape sequences &aml; and &lt; for & and < are 
	 * built-in. 
	 *)

	type escape

	val escape   : string-> escape option

	val textForEsc       : escape-> string    

        val annotationForEsc : escape-> (SmlTk.Mark* SmlTk.Mark)-> 
	                                       SmlTk.Annotation Option.option


        val warning          : string-> unit (* how to deal with warnings *)

        val error            : string-> exn  (* exception to be raised if a 
					      * parsing error occurs 
					      *)
    end

signature SMLTK_MARKUP =
    sig
	type widgetinfo (* same as above *)

	(* Given some widgetinfo (first argument), and a
	 * markup' text, generate an the string containing the text and a
	 * list of appropriate annotations as above.  
	 *) 
	val getAnnText : widgetinfo-> string-> SmlTk.AnnoText

    end 






