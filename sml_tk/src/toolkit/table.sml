(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/table.sml,v $

   SmlTk-Tables

   $Date: 2001/03/30 13:39:51 $
   $Revision: 3.0 $
   Author: ludi (Last modification by $Author: 2cxl $)

   (C) 1999, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)

structure Table : TABLE_SIG =
    struct
	open SmlTk

	fun width(AnnoText {str,...}) =
	    let
		fun maxwidth (l :: ls) n =
		    if size l > n then maxwidth ls (size l) else maxwidth ls n
		  | maxwidth _ n         = n
	    in
		maxwidth(String.tokens (fn c => c = #"\n") str) 0
	    end

	fun height(AnnoText {str,...}) =
	    length(String.tokens (fn c => c = #"\n") str)

	fun table (cnf : {constant_column_width : bool,
			  headline_relief       : SmlTk.RelKind,
			  headline_borderwidth  : int,
			  headline_foreground   : Color,
			  headline_background   : Color,
			  field_relief          : SmlTk.RelKind,
			  field_borderwidth     : int,
			  field_foreground      : Color,
			  field_background      : Color,
			  container_background  : Color}) txts =
	    let
		fun column_width n =
		    let
			fun column_width' (l :: ls) m =
			    let
				val w =
				    width(List.nth(l, n-1))
				    handle _ => 0
			    in
				column_width' ls (Int.max(w, m))
			    end
			  | column_width' _ m         = m
		    in
			column_width' txts 0
		    end

		fun line_height n =
		    let
			fun line_height' (f :: fs) m =
			    line_height' fs (Int.max(height f, m))
			  | line_height' _ m         = m
		    in
			line_height'(List.nth(txts, n - 1)) 0
		    end

		fun max_column_width() =
		    let
			fun single_line_maxwidth (f :: fs) n =
			    if width f > n then
				single_line_maxwidth fs (width f)
			    else single_line_maxwidth fs n
			  | single_line_maxwidth _ n         = n

			fun max_column_width' (l :: ls) n =
			    if single_line_maxwidth l 0 > n then
				max_column_width' ls (single_line_maxwidth l 0)
			    else max_column_width' ls n
			  | max_column_width' _ n         = n
		    in
			max_column_width' txts 0
		    end

		fun line (t :: ts) r c =
		    TextWid {widId      = newWidgetId(),
			     annotext   = t,
			     scrolltype = NoneScb,
			     packings   = [Row r, Column c],
			     configs    =
			       [Relief(if r = 1 then #headline_relief cnf
				       else #field_relief cnf),
				Borderwidth(if r = 1 then
						#headline_borderwidth cnf
					    else #field_borderwidth cnf),
				Width(if #constant_column_width cnf then
					  max_column_width()
				      else column_width c),
				Height(line_height r), Active false,
				Foreground(if r = 1 then
					       #headline_foreground cnf
					   else #field_foreground cnf),
				Background(if r = 1 then
					       #headline_background cnf
					   else #field_background cnf),
				Cursor(XCursor("left_ptr", NONE))],
			     bindings   = []} ::
		    line ts r (c + 1)
		  | line [] _ _        = []

		fun tab (l :: ls) r = line l r 1 @ tab ls (r + 1)
		  | tab [] _        = []

		val widgets = tab txts 1
	    in
		Frame {widId    = newWidgetId(),
		       widgets  = Grid widgets,
		       packings = [],
		       configs  = [Background(#container_background cnf)],
		       bindings = []}
	    end

	val std_conf =  {constant_column_width = true,
			 headline_relief       = Groove,
			 headline_borderwidth  = 1,
			 headline_foreground   = Black,
			 headline_background   = White,
			 field_relief          = Ridge,
			 field_borderwidth     = 1,
			 field_foreground      = Black,
			 field_background      = White,
			 container_background  = White}
    end
