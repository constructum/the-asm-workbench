(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/ann_texts.sig,v $
 
   Annotated texts for sml_tk.

   $Date: 2001/03/30 13:38:56 $
   $Revision: 3.0 $
   Author: cxl (Last modification by $Author: 2cxl $)

   (C) 1996, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

signature ANNOTATED_TEXT =
    sig
	(* This type represents annotated texts. *)
	(* type AnnoText *)

        (* selectors *)
        val selText  : BasicTypes.AnnoText -> string
        val selAnno  : BasicTypes.AnnoText -> BasicTypes.Annotation list
	val updAnno  : BasicTypes.AnnoText -> BasicTypes.Annotation list 
	                                             -> BasicTypes.AnnoText
	val selLength : BasicTypes.AnnoText -> {rows: int, cols: int}

	(* The empty annotated text *)
	val mtAt : BasicTypes.AnnoText 

	(* Concatenate annotated texts, keeping track of the annotations. *)
	val +++ : BasicTypes.AnnoText * BasicTypes.AnnoText -> BasicTypes.AnnoText

	(* count length (in rows/colums) *)
	val lenAT : string-> int* int

	(* add a new line at the end *)
	val nl : BasicTypes.AnnoText -> BasicTypes.AnnoText

	(* make a string into an annotated text with no annotations *)
	val mk : string   -> BasicTypes.AnnoText

	(* like concatWith from BasicUtil *)
	val concatAtWith : string -> BasicTypes.AnnoText list 
	                                                -> BasicTypes.AnnoText 

        (* adjust marks in the annotation by given offset *)
        val adjustMarks   : {rows: int, cols: int}-> 
	                       BasicTypes.Annotation list->
			            BasicTypes.Annotation list

    end
