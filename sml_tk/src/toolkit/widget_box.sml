(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/widget_box.sml,v $

   Widget boxes

   $Date: 2001/03/30 13:39:57 $
   $Revision: 3.0 $
   Author: ludi (Last modification by $Author: 2cxl $)
 
   (C) 1999, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

structure WidgetBox : WIDGET_BOX =
struct
    open SmlTk

    exception WidgetBox

    type WBoxItemId = AnnId

    fun widgetBox (boxDef : {widId      : WidId,
                             scrolltype : ScrollType,
                             widgets    : Widget list,
                             packings   : Pack list,
                             configs    : Configure list,
                             bindings   : Binding list}) =
	let
	    fun annos (w::ws) l = TAWidget {annId    = newAnnotationId(),
					    mark     = Mark(l,0),
					    widgets  = Pack [w],
					    configs = [],
					    bindings = []}
		                  :: annos ws (l+1)
	      | annos [] l      = []
	in
	    TextWid{widId      = #widId boxDef,
		    scrolltype = #scrolltype boxDef,
		    annotext   =
		      AnnoText {len         = NONE,
				str         = "",
				annotations = annos (#widgets boxDef) 1},
		    packings   = #packings boxDef,
		    configs    = [Cursor(XCursor("arrow", NONE)),
				  Active false] @
		                 #configs boxDef,
		    bindings   = #bindings boxDef}
	end

    fun insertWidgetBoxAt (id, l) w =
	let
	    val annID = newAnnotationId()
	in
	    (addConf id [Active true];
	     addAnnotation id (TAWidget {annId    = annID,
					 mark     = Mark(l, 0),
					 widgets  = Pack [w],
					 configs  = [],
					 bindings = []});
	     addConf id [Active false];
	     annID)
	    handle Errors => raise WidgetBox
	end

    fun insertWidgetBoxAtEnd id w =
	let
	    val annID = newAnnotationId()
	in
	    (addConf id [Active true];
	     addAnnotation id (TAWidget {annId    = annID,
					 mark     = MarkEnd,
					 widgets  = Pack [w],
					 configs  = [],
					 bindings = []});
	     addConf id [Active false];
	     annID)
	    handle Errors => raise WidgetBox
	end

    fun delWidgetBox id itID = delAnnotation id itID

    fun clearWidgetBox id =
	let
	    fun clear (ann::anns) = (delAnnotation (id ) (selAnnotationId ann);
				     clear anns)
	      | clear []          = ()
	in
	    (addConf id [Active true];
	     clear(selTextWidAnnotations(getWidget(id )));
	     clearText id;
	     addConf id [Active false])
	    handle Errors => raise WidgetBox
	end

    fun replaceWidgetBox(wid, nuwidgets)= 
      (clearWidgetBox wid; List.map (insertWidgetBoxAtEnd wid) nuwidgets)

end
