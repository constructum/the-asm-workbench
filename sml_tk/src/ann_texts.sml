(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/ann_texts.sml,v $
 
   Annotated texts for the sml_tk markup language.

   $Date: 2001/03/30 13:38:57 $
   $Revision: 3.0 $
   Author: cxl (Last modification by $Author: 2cxl $)

   (C) 1996, 1998, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

structure AnnotatedText : ANNOTATED_TEXT = 

struct 

    infix 6 +++  (* see below -- fixity decl's not allowed inside signatures *)

    open BasicTypes BasicUtil


    (* The two int's are the index to the last character/last row of the 
     * text. This index is calculated `lazy'-- i.e. if it is not given 
     * explicitly, it is only calculated if another annotated text with
     * a non-empty list of annotations is appended to the text. (Yes, bloody
     * clever I know.)
     *)
	        			
    fun selText (AnnoText{str,...})= str
    fun selAnno (AnnoText{annotations,...})= annotations
    fun updAnno (AnnoText{len=x,str=t,...}) a = 
                 AnnoText{len=x,str=t,annotations=a}
    (* adding rows-- a wee bit funny, since counting starts at one
     * but we want to be graceful about line zero as well. *)
    fun addRows(r1, r2) = r1+ r2- (Int.min(r1, 1))

    fun add2Mark (r, c) (Mark(rm, cm)) = if (rm<= 1) then Mark(r, c+cm)
					 else Mark(addRows(r, rm), cm)
      | add2Mark (r, c) (MarkToEnd rm) = MarkToEnd(addRows(r, rm))
      | add2Mark (r, c) MarkEnd        = MarkEnd
	              (* a wee bit debatable-- we might want to adjust the 
		       * "End" etc. marks in the annotations of the first
		       * text. Then again, end means end *)

    fun pair f (x, y) = (f x, f y)  (* has gone into BasicUtil *)

    fun mapMark f (TATag{annId, marks, configs, bindings}) =
                   TATag{annId=annId, marks=map (pair f) marks, 
                         configs=configs, bindings=bindings}
      | mapMark f (TAWidget{annId, mark, widgets, configs,
			    bindings}) =
                    TAWidget{annId=annId, mark=f mark, widgets=widgets,
			     configs=configs, bindings=bindings}

    (* concatenate two annotated texts with explicit length *)
    fun cat ((rows, cols), s, a) ((rows0, cols0), t, b) =
	let val ann = a@(map (mapMark (add2Mark (rows, cols))) b)
	in  if rows0 <= 1 (* second text only has one line, we only add cols *)
	    then AnnoText{len=SOME(rows, cols+cols0), str=s^t,annotations=ann}
	    else AnnoText{len=SOME(addRows(rows, rows0), cols0), 
			  str=s^t, annotations=ann}
	end                    

    (* count length of annotated text *)
    fun lenAT t =
	   let fun cnt (thischar, (line, char)) =
	       if (StringUtil.isLinefeed thischar) 
		   then (line+1, 0)
	       else (line, char+1)
	       val (rows, cols) = Substring.foldl cnt (1, 0) (Substring.full t)
	   in (Int.max(rows, 1), cols)
	   end

    fun selLength(AnnoText{len=SOME (r, c), ...})= {rows= r, cols= c}
      | selLength(AnnoText{len=NONE, str=t, ...})= let val (r, c)= lenAT t 
						   in {rows= r, cols= c} 
						   end
  	 
    fun ((AnnoText{len=NONE, str=s, annotations=a}) +++ 
         (AnnoText{len=NONE, str=t, annotations=[]})) =
	 (AnnoText{len=NONE, str=s^t, annotations=a}) 
      | ((AnnoText{len=len1, str=s, annotations=a}) +++ 
         (AnnoText{len=len2, str=t, annotations=b})) =
	 let fun get_len (SOME(r, c), s, a) = ((r, c), s, a)
	       | get_len (NONE, s, a)       = (lenAT s, s, a)
	 in  cat (get_len(len1, s, a)) (get_len(len2, t, b))
	 end


    fun nl(AnnoText{len=NONE, str=s, annotations=a})       = 
           AnnoText{len=NONE, str=s^"\n", annotations=a}
      | nl(AnnoText{len=SOME(r, c), str=s, annotations=a}) = 
           AnnoText{len=SOME(r+1, 0), str=s^"\n", annotations=a}

    val mtAt = AnnoText{len=NONE, str="", annotations=[]}

    fun adjustMarks {rows, cols} annos = 
	map (mapMark (add2Mark (rows, cols))) annos

   (* convert a string to an annotated text with no annotations *)
    fun mk str = AnnoText{len=NONE, str=str, annotations=[]}

    fun concatAtWith str ls = 
	let val at = mk str
	    fun concatWith' []      = mtAt
	      | concatWith' [t]     = t
	      | concatWith' (t::ts) = t +++ at +++ (concatWith' ts)
	in  concatWith' ls
	end

end
