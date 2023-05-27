(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/fonts.sig,v $

   Fonts for sml_tk -- signature file.
  
   This module tries to provide a wee bit more abstract approach to
   specifying fonts than as in "-*-bollocks-*-*-37-" X-style font
   description.
 
   $Date: 2001/03/30 13:39:11 $
   $Revision: 3.0 $
   Author: cxl (Last modification by $Author: 2cxl $)

   (C) 1997, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)


signature FONTS_SIG =
    sig

	datatype FontConfig =
	    Bold | Italic | 
	    Tiny | Small | NormalSize | Large | Huge |
	    Scale of real 


	datatype Font = 
	    XFont of string  
	  | Normalfont of FontConfig list      
	  | Typewriter of FontConfig list 
	  | SansSerif of  FontConfig list
	  | Symbol of     FontConfig list
       (*   could (should?) have more here ... *)


      (* selectors and update functions *)
      val selFontConf : Font -> FontConfig list
      val updFontConf : Font * FontConfig list -> Font
    
      val fontDescr : Font -> string  (* get X-style font description *)				  
      (* initialize fonts, check if all fonts exist etc.
       * The argument is the library path, which should point to a directory
       * where the xlsfonts script can be found *)
      val init : string -> unit       

      (* path to the xlsfonts executable. Exported here so we can check
       * before startup if it exists (ows init above will hang) *)
      val getTestfontPath : string-> string 

     (* Configurations. You can here set the base size of the fonts,
      * and the families from which the different fonts are chosen
      * The string should contain the fndry and the family, as in
      * Symbol = ref "-*-symbol" *)

      val Config :
	  {Normalfont : string ref,
	   Typewriter : string ref,
	   SansSerif  : string ref,
	   Symbol     : string ref,
	   BaseSize   : int    ref,
	   ExactMatch : bool   ref,
	   Resolution : int    ref
	  }
(*
      val InitConfig :
	  {NormalFn'     : (bool * bool -> string) ref ,
	   TypewriterFn' : (bool * bool -> string) ref ,
	   SansSerifFn'  : (bool * bool -> string) ref ,
	   SymbolFn'     : (bool * bool -> string) ref
	  }

      val FinalConfig :
	  {NormalFn     : (bool * bool * int -> string) ref ,
	   TypewriterFn : (bool * bool * int -> string) ref ,
	   SansSerifFn  : (bool * bool * int -> string) ref ,
	   SymbolFn     : (bool * bool * int -> string) ref
	  }
*)
    end
