(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/tests+examples/markup_ex.sml,v $
 
   sml_tk Markup Languages: an example.

   $Date: 2001/03/30 13:40:03 $
   $Revision: 3.0 $

   Author: cxl (Last modification by $Author: 2cxl $)

   (C) 1996, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)


structure MarkupEx : sig val go : unit-> unit end =

    struct

    open SmlTk SmlTk21

    structure ColourTags =
	struct

	    type widgetinfo= WidId
	    
	    exception AnnotationError of string

	    datatype tag = redTag | blueTag | greenTag | boxTag

	    fun matchingTag "red"   = SOME redTag
	      | matchingTag "blue"  = SOME blueTag
	      | matchingTag "green" = SOME greenTag
	      | matchingTag "box"   = SOME boxTag
	      | matchingTag  _      = NONE


            fun anno col wHere = TATag(newAnnotationId(), [wHere],
				       [Relief Raised, Foreground col], [])				  
            fun annotationForTag redTag _ _ marx   = anno Red marx
	      | annotationForTag blueTag _ _ marx  = anno Blue marx 
	      | annotationForTag greenTag _ _ marx = anno Green marx
	      | annotationForTag boxTag _ _ marx   = 
		   TATag(newAnnotationId(), [marx],
				       [Relief Raised, Borderwidth 2], [])

	    type escape  = unit

	    fun escape _     = NONE

	    fun annotationForEsc () marx = NONE
	    fun textForEsc ()            = ""

	    fun escapeSequence x = x

            fun warning w = Debug.warning ("SML Warning: "^w)

	    val error= Fail

	end

    structure Taggit = SmlTkMarkup(ColourTags)

    val sometext= 
	"In this <red>wonderful<\\red> test text,\n"^ 
	"You should see <blue>blue<\\blue> and <green>green<\\green> bits,\n"^
	"never "^"mind the <box>boxed<\\box> ones."^
	"\n\n\n1234<red>5<\\red>6789<blue>0<\\blue>12345.\n"^
	"Here's some special characters: +&<*! 1 &lt; 2 "^
	"Rock&amp;roll or what?\n"^
	"\n\n\n<red>Thank you for <blue>your<\\red> attention.<\\blue>\n"

(*	^"Here's some erroneous markup code:  Can you &see; this? <closing lt missing, &no semicolon, <blue>No closing tag, <\\closing lt missing." *)

    fun textWidget win =
	let val twid      = newWidgetId()
	    val ann       = Taggit.getAnnText twid sometext
	in  TextWid(twid, NoneScb, ann, [Fill X, Side Top], 
		    [Active false], [])
	end
		   

    fun quitButton win =
	Button(newWidgetId(),
                   [Side Bottom, Fill X, Expand true],
                   [Relief Ridge, Borderwidth 2,
                    Text "Quit", Command (fn ()=> closeWindow win)], []) 


   val mainWin =
        let val wid = newWinId()
        in mkWindow{winId= wid, 
		    config= [WinTitle "Colour Tag Test Window"], 
		    widgets= Pack [textWidget wid, quitButton wid],
		    bindings = [],
		    init= noAction}
        end
 
    fun go () = TextIO.output(TextIO.stdOut, SmlTk.startTclExn[mainWin])
                   
end
