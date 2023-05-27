(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/std_markup.sml,v $
 
   The sml_tk Standard Markup Language.

   This module offers a standard markup language for use with sml_tk. It is 
   still generic with respect to the bindings, since these need to be 
   compiled rather than generated.

   This has the disadvantage that the standard tags implemented by the
   StdExMarkup module below (eg. em) can't have bindings, and on the
   other hand the binding tags will find it hard to use the
   font-changing tags provided by said module. On the other hand, it
   is a clear separation of concerns.

   $Date: 2001/03/30 13:39:50 $
   $Revision: 3.0 $

   Author: cxl (Last modification by $Author: 2cxl $)

   (C) 1996, 1998, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)



(* The standard Markup language, extendible *)
(* There is a ready-to-use version below    *)


signature BINDTAGS =
    sig       
	type bindTag
	type widgetinfo
	
	(* matchingBTag returns a bindTag matching the given string *)
	val matchingBTag      : string -> bindTag option


	(* annotationForBTag returns an annotation for a given BTag. 
	 * It may raise exception BTagError (below) if an error occurs
	 *)
	val annotationForBTag : bindTag ->string list-> widgetinfo->
	                                (SmlTk.Mark* SmlTk.Mark)-> 
	                                                    SmlTk.Annotation
							    
	exception BTagError of string


	(* additional customized escape sequences *)

	type escape

	val escape   : string-> escape option

	val textForEsc       : escape-> string    

        val annotationForEsc : escape-> (SmlTk.Mark* SmlTk.Mark)-> 
	                                       SmlTk.Annotation Option.option

	(* The exception to be raised by the parser if an error occurs *)
	val parsingError      : string-> exn

    end

functor StdTags(bindingTags : BINDTAGS) : 
    TAGS (* where type widgetinfo= bindingTags.widgetinfo *) = 

    struct

	open SmlTk BasicUtil

	val error= bindingTags.parsingError

	exception AnnotationError of string

	datatype tag = 
	    (* fonts *)
	    fontTag | 
	    (* raised/lowered boxes *)
	    raiseTag | boxTag  |
	    (* binding tags *)
	    bindTag of bindingTags.bindTag
	    (* derived tags *)
	    (* superTag | subTag *)
	    
	fun matchingTag "font"  = SOME fontTag
	  | matchingTag "raise" = SOME raiseTag
	  | matchingTag "box"   = SOME boxTag
	  (*		
	  | matchingTag "super" = SOME superTag
	  | matchingTag "sub"   = SOME subTag
	   *)
	  | matchingTag  str    = case bindingTags.matchingBTag str of
	                               SOME bt => SOME (bindTag bt)
				     | NONE    => NONE

        fun fontAnnotation args wHere = 

	    let exception NoConfFont
		
		fun fontConf "bf"    = Bold
		  | fontConf "bold"  = Bold
		  | fontConf "it"    = Italic
		  | fontConf "em"    = Italic
		  | fontConf "tiny"  = Tiny
		  | fontConf "small" = Small
		  | fontConf "large" = Large
		  | fontConf "huge"  = Huge 
		  | fontConf str     = 
		     if String.isPrefix "size=" str
		     then let val fstr  = substring(str, 5, (size str)-5)
			      val factor= case (Real.fromString fstr) of
			                       SOME f=> f
					     | NONE  => raise (AnnotationError "No argument for SCALE config")
			  in  Scale factor
			  end
		     else
			 raise NoConfFont	    
		    
		fun fontName "tt"    = Typewriter
		  | fontName "sf"    = SansSerif
		  | fontName "symb"  = Symbol
		  | fontName  _      = raise NoConfFont
		    
		fun foldConfig(c, r) =
		    (fontConf c)::r 
		    handle NoConfFont => r

		fun getFont a =
		    (fontName (List.last a)
		     handle NoConfFont => Normalfont
			  | Empty      => Normalfont)
	       
		val font = (getFont args) 
		           (foldr foldConfig ([]: FontConfig list) args)
	    in
		TATag{annId= newAnnotationId(), 
		      marks= [wHere], configs= [Font font], bindings= []}
	    end


	fun getFirstArg nm []     = raise (AnnotationError
					  ("No argument for "^nm^" tag"))
	  | getFirstArg _  (r::_) = r

	type widgetinfo= bindingTags.widgetinfo

	fun annotationForTag fontTag args wi wh = fontAnnotation args wh
	  | annotationForTag raiseTag r wi wh = 
	                     TATag{annId= newAnnotationId(), marks= [wh],
				   configs= [Offset (StringUtil.toInt 
						     (getFirstArg "RAISE" r))],

				   bindings= []}
	  | annotationForTag boxTag r wi wh = 
                             TATag{annId= newAnnotationId(), marks= [wh],
				   configs= [Relief Groove, Borderwidth 1], 
				   bindings= []}
	  | annotationForTag (bindTag btag) args wi wh  = 
	                     bindingTags.annotationForBTag btag args wi wh
	    
	    
	datatype escape = btEsc of bindingTags.escape 
	                | font  of Font* string 
			  
	fun mkchr fspec s = SOME (font(fspec, String.str(Char.chr s)))
	fun mkstr fspec s = SOME (font(fspec, String.implode (map Char.chr s)))
	val symbchr = mkchr (Symbol [])
	val symbstr = mkstr (Symbol [])
	val bigsymbchr = mkchr (Symbol [Huge])

        (* The following escape sequences by and large follow TeX's 
	 * naming, except where I find these very inappropriate (vee, wedge,
	 * cup and cap are called or, and, union and intersect, respectively);
	 * but in particular with respect to the greek letters (varphi, varrho,
	 * varepsislon etc.) 
	 * Also, I _know_ the following is not the greek alphabet -- it's the 
	 * order in which the letters appear in the symbol font.
	 *)

	fun (* grk letters, lowercase *)
	    escape "alpha" = symbchr 97
	  | escape "beta"  = symbchr 98
	  | escape "chi"   = symbchr 99
	  | escape "delta" = symbchr 100	    
	  | escape "epsilon"= symbchr 101
	  | escape "phi"   = symbchr 102
          | escape "gamma" = symbchr 103
          | escape "eta"   = symbchr 104
          | escape "varphi"= symbchr 106
          | escape "iota"  = symbchr 105
          | escape "kappa" = symbchr 107
          | escape "lambda"= symbchr 108
          | escape "mu"    = symbchr 109
          | escape "nu"    = symbchr 110
          | escape "omikron"= symbchr 111 
          | escape "pi"    = symbchr 112
          | escape "theta" = symbchr 113 
          | escape "vartheta"= symbchr 74 
          | escape "rho"   = symbchr 114
          | escape "sigma" = symbchr 115
          | escape "varsigma"= symbchr 86
          | escape "tau"   = symbchr 116
          | escape "upsilon" = symbchr 117
          | escape "varpi" = symbchr 118
          | escape "omega" = symbchr 119
          | escape "xi"    = symbchr 120
          | escape "psi"   = symbchr 121
          | escape "zeta"  = symbchr 122

	    (* grk letters, uppercase *)
	  | escape "Alpha" = symbchr 65
	  | escape "Beta"  = symbchr 66
	  | escape "Chi"   = symbchr 67
	  | escape "Delta" = symbchr 68
	  | escape "Eps"   = symbchr 69 
	  | escape "Phi"   = symbchr 70 
          | escape "Gamma" = symbchr 71 
          | escape "Eta"   = symbchr 72 
          | escape "Iota"  = symbchr 73 
          | escape "Kappa" = symbchr 75 
          | escape "Lambda"= symbchr 76 
          | escape "Mu"    = symbchr 77 
          | escape "Nu"    = symbchr 78 
          | escape "Omikron" = symbchr 79  
          | escape "Pi"    = symbchr 80 
          | escape "Theta" = symbchr 81   
          | escape "Rho"   = symbchr 82
          | escape "Sigma" = symbchr 83
          | escape "Tau"   = symbchr 84
          | escape "Upsilon" = symbchr 85 
          | escape "Omega" = symbchr 87
          | escape "Xi"    = symbchr 88
          | escape "Psi"   = symbchr 89
          | escape "Zeta"  = symbchr 90
	    
	    (* quantifiers and junctors *)
	  | escape "forall" = symbchr 34
	  | escape "exists" = symbchr 36
	  | escape "Forall" = bigsymbchr 34
	  | escape "Exists" = bigsymbchr 36
	  | escape "existsone" = symbstr [36, 33]
          | escape "not"    = symbchr 216
          | escape "and"    = symbchr 217
          | escape "or"     = symbchr 218

	    (* other operations *)
	  | escape "times"  = symbchr 180
          | escape "sum"    = symbchr 229 (* NB. not the same as            *)
	  | escape "prod"   = symbchr 213 (* &Pi; and &Sigma; respectively! *)
          | escape "comp"   = symbchr 183 (* fat dot, a wee dot is 215 *)
	  | escape "bullet" = symbchr 183 
	  | escape "tensor" = symbchr 196
	  | escape "otimes" = symbchr 196
	  | escape "oplus"  = symbchr 197

	  | escape "bot"    = symbchr 94

	    (* arrows *)
          | escape "rightarrow" = symbchr 174
          | escape "Rightarrow" = symbchr 222
          | escape "longrightarrow" = symbstr [190, 174]
          | escape "Longrightarrow" = symbstr [61, 222] (* looks ugly  *)
          | escape "leftrightarrow" = symbchr 171
          | escape "Leftrightarrow" = symbchr 219
	  | escape "Downarrow"      = symbchr 223
          | escape "Uparrow"        = symbchr 221
          | escape "vline"          = symbchr 189
          | escape "hline"          = symbchr 190

          | escape "rbrace1"     = symbchr 236
          | escape "rbrace2"     = symbchr 237  (* these three make a large *)
          | escape "rbrace3"     = symbchr 238  (* right brace *)

	    (* set operations *)
          | escape "emptyset" = symbchr 198
          | escape "in"       = symbchr 206
          | escape "notin"    = symbchr 207
          | escape "intersect" = symbchr 199
	  | escape "union"    = symbchr 200
          | escape "subset"   = symbchr 204
          | escape "subseteq" = symbchr 205
	  | escape "setminus" = symbchr 164
(*	  | escape "powerset" = mkchr (Normalfont [Bold]) 82 *)
          | escape "powerset" = symbchr 195
          | escape "inf"      = symbchr 165

          | escape "Intersect" = bigsymbchr 199
	  | escape "Union"    = bigsymbchr 200 
    
	    (* relations *)
	  | escape "equiv"    = symbchr 186
          | escape "neq"      = symbchr 185
          | escape "leq"      = symbchr 163
          | escape "grteq"    = symbchr 179

	  | escape "lsem"     = symbstr [91, 91] (* "semantic" *)
          | escape "rsem"     = symbstr [93, 93] (* brackets ''[[ ... ]]'' *)

	    (* misc other symbols *)
	  | escape "dots"     = symbchr 188
          | escape "copyright"= symbchr 227

          | escape str     = Option.map btEsc (bindingTags.escape str)

	fun textForEsc (font(_, s))= s
          | textForEsc (btEsc  e)  = bindingTags.textForEsc e

	fun annotationForEsc (font(fspec, _)) wh = 
	    SOME (TATag{annId= newAnnotationId(), 
			marks= [wh], configs= [Font fspec], bindings= []})
	  | annotationForEsc (btEsc s)  wh = bindingTags.annotationForEsc s wh

	    
	fun warning w = TextIO.output(TextIO.stdOut, 
				      "SmlTk Markup warning: "^w^"\n")
	                (* should use the warning window from utilwin
			 * -- no it bloody shouldn't, since parsing can 
			 *    occur at compile time.
			 *)

    end



structure StdMarkup =
    struct
	local structure S= 
	    SmlTkMarkup(StdTags(struct		 
		     type bindTag = unit
		     type widgetinfo= unit (* SmlTk.WidId *)

		     exception BTagError of string
		     exception StdMarkupParseError of string
		     val parsingError= StdMarkupParseError 

		     fun matchingBTag _ =  NONE
		     fun annotationForBTag () _ _ _ =
			 raise (BTagError 
				"Illegal annotation in annotationForBTag")

		     type escape = unit
		     fun  escape _ = NONE
		     fun  annotationForEsc () _ = NONE
		     fun  textForEsc () = ""
		 end))
	in 
	    val getAnnText= S.getAnnText ()
	end
    end

