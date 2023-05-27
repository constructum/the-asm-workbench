(* ***************************************************************************

   $Source: /repository/sml/sml_tk/src/toolkit/gen_gui.sml,v $

   A generic graphical user interface. 
  
   See <a href=file:../../doc/manual.html>the documentation</a> for more
   details.  

   "tests+examples/simpleinst.sml" contains a small example of how to
   use this package.
 
   $Date: 2001/03/30 13:39:43 $
   $Revision: 3.0 $

   Author: cxl (Last modification $Author: 2cxl $)

   (C) 1996, 1998, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)




functor GenGUI(structure appl: APPL_SIG ) : GENGUI_SIG 

= 

struct 
    
    open SmlTk BasicUtil 




    val default_printmode={mode = Print.Long,
                           printdepth=100,
                           height=NONE,
                           width=NONE}  (* the value is temporary *)

    fun debugmsg msg = Debug.print 11 ("GenGUI: "^msg)



    (* the construction area frame widget id *)
    val caFrameId  = mkWidgetId("ca")
    (* the widget id of the canvas all the items are placed on *)

    (* flag indicating wether the construction area is currently open *)
    val caOpen     = ref (NONE: appl.object option)

    fun openConArea {win,obj,replace_object_action,outline_object_action} =
	let
	    (* id of the window holding the con/area widgets *)
	    val cawin = if appl.Conf.oneWindow then win 
			else newWinId()
	    (* bindings for the con area while open *)
	    fun caEnter wsp ev =
		let val dropobs = appl.cb_objects_rep (appl.CB.copy ev)()
		    val oot     = appl.objlist_type dropobs
		in  case oot of 
		    SOME ot => appl.area_ops ot wsp dropobs
		  | NONE => () 
		end handle appl.CB.Empty => ()
	    fun caBindings wsp = 
		[BindEv(Enter, caEnter wsp)]
	    (* bindings for the con/area while closed *)
	    val caClosedBindings =
		[BindEv(Enter, K0)]
	    fun closeConArea nuOb =
		(caOpen := NONE;
		 replace_object_action nuOb;
		 if appl.Conf.oneWindow then 
		     app (delWidget o selWidgetId) 
		         (selWidgets (getWidget caFrameId))
		 else 
		     closeWindow cawin
		)
	in
	    if (appl.is_constructed (appl.obj_type obj)) 
		andalso not (Option.isSome (!caOpen)) then
		let 
		    (* get the con/area widgets from the application: *)
		    val (wsp, wwidgs, init) = appl.area_open(cawin, obj, 
							     closeConArea)
		    (* add con/area bindings to widgets: *)
		    val wwidgs = map (fn w=> updWidgetBind w 
				               ((caBindings wsp)@
						(selWidgetBind w)))
                        			 wwidgs			 
		in 
		    (outline_object_action ();
		     (* set flag *)
		     caOpen := SOME obj;
		     if appl.Conf.oneWindow then
			 (app (addWidget win caFrameId) wwidgs;
			  addBind caFrameId (caBindings wsp);
			  init()
			 )
		     else openWindow(mkWindow{
			   winId=cawin, 
			   config= [WinTitle (appl.Conf.caTitle 
					      (appl.string_of_name
                                                      (appl.name_of obj)
                                                      (default_printmode))),
				    WinGeometry (SOME(appl.Conf.caWidth,
						      appl.Conf.caHeight),
						 appl.Conf.caXY),
				    WinGroup win], 
			   widgets= Pack wwidgs, 
			   bindings = [], init=init})
		     )
		end	    
	    else
		debugmsg "Not a primary object, or ConArea already open."
     end
 
    (* and a function to check that *)
    fun isOpen ob = case (!caOpen) of 
                       NONE => false 
                     | SOME ob2 => case appl.ord(ob,ob2) of
                                     EQUAL => true
                                   | _     => false


    
    structure notepadappl = 
         struct 
            open appl;
            
            val object_action = openConArea

            fun activate_action {pos=(x,y)} = ()
            
            val is_locked_object = isOpen

         end

    structure Notepad = Notepad(structure appl = notepadappl);

    open Notepad;


	
    fun main_wid win =
	let val assArea= Notepad.main_wid win
	in  if appl.Conf.oneWindow then
	    Frame{widId= newWidgetId(),
		  widgets=Pack [assArea,
			        Frame{widId=caFrameId, 
				      widgets=Pack [], 
				      packings= [Fill X, Side Bottom],
				      configs=  [Height appl.Conf.caHeight,
						 Width appl.Conf.caWidth],
				      bindings=[]}
			        ], 
		  packings= [], bindings= [], configs= []}
	   else
	       assArea
	end


    fun init state = 
	(Notepad.init state;
         caOpen   := NONE;
         appl.area_init()
	 )

    type cb_objects     = appl.cb_objects
    val  cb_objects_rep = appl.cb_objects_rep
    val  cb_objects_abs = appl.cb_objects_abs

end




