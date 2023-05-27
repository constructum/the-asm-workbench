(* this structure contains a lot of useful little functions
   which are used throughout the program and do not fit in any other place *)

structure GUI_Misc =
struct
  open SmlTk SmlTk21

  fun debug debug_on modulename s L = if debug_on then print (String_.replace (modulename^"."^s^"\n") L) else ()
			     
  fun mkWin4 (winId, config, widgets, init) =   (* old-style, i.e. smltk 2.1, window, using a tuple instead of a record - no bindings *)
    mkWindow { winId = winId, config = config, widgets = SmlTk.Pack widgets, bindings = [], init = init }
	     
  fun mkWin5 (winId, config, widgets, bindings, init) =   (* a similar one, but with possibility of defining bindings *)
    mkWindow { winId = winId, config = config, widgets = SmlTk.Pack widgets, bindings = bindings, init = init }
	     
  val pack_lfX = [ Side Left, Expand false, Fill X ]
  val pack_rfX = [ Side Right, Expand false, Fill X ]

  val noAction	  = fn () => ()    
  val noPosition = ~1

  fun replaceText wid_id text =
  ( setTextWidReadOnly wid_id false;
    clearText wid_id; insertTextEnd wid_id text;
    setTextWidReadOnly wid_id true )

  structure Label =
  struct
    fun redraw id text = addConf id [ Text text ]
  end

  structure TextWid =
  struct
    fun redraw id text = replaceText id text
  end

  structure Listbox =
  struct
    fun setPosition id =
    ( case (readCursor id) of Mark (pos, _) => pos
			    | _ => ~1 ) handle _ => ~1
    fun fill id items =
    ( clearText id; map (insertTextEnd id) items; () )

    fun stdBindings fct =
      [ BindEv (ButtonPress (SOME 1), fn ev => fct ()),
	BindEv (KeyPress "", fn ev => fct ()) ]

    fun stdConfig () =
      [ Relief Raised, Font ASM_GUI_Fonts.listboxFont ]
  end

  local open OS.FileSys
  in fun writable FileName = access (FileName, [A_WRITE])
     fun readable FileName = access (FileName, [A_READ])
  end

  local fun copy_all (in_str, out_str) =
	    if not (TextIO.endOfStream in_str) 
	    then ( TextIO.output (out_str, (TextIO.inputN (in_str, 100))); copy_all (in_str, out_str) )
            else ()
  in fun copy_file (in_str: TextIO.instream, out_str: TextIO.outstream) =
       ( copy_all (in_str, out_str);
	 TextIO.closeIn in_str;
	 TextIO.closeOut out_str )
  end

  fun openWindowIfNotOpen (window :Window) =
    if not (occursWin (selWindowWinId window))
    then openWindow window
    else focus (selWindowWinId window)

  (******************************************************************************)               

  fun stdTextWid (wid_id_str, conf_list) =
    TextWid ( wid_id_str, RightScb, AnnoText (NONE, "", []), [Side Top, Expand true, Fill Both],
	      conf_list @  [ Font ASM_GUI_Fonts.textWidFont ], [] )

  fun insertNewText wid_id text =
  ( clearText wid_id; insertTextEnd wid_id text )

  fun stdButtonBar button_list =
    Frame ( newWidgetId (), button_list, [Fill X], [Relief Ridge, Borderwidth 2], [] )

  fun LeftButton (text, comm) =
    Button (newWidgetId (), [Side Left, Fill X], [ Text text, Command comm ], [])
  fun RightButton (text, comm) =
    Button (newWidgetId (), [Side Right, Fill X], [ Text text, Command comm ], [])
  fun LeftExpandButton (text, comm) =
    Button (newWidgetId (), [Side Left, Fill X, Expand true], [ Text text, Command comm ], [])
  fun RightExpandButton (text, comm) =
    Button (newWidgetId (), [Side Right, Fill X, Expand true], [ Text text, Command comm ], [])
  fun ExpandButton (text, comm) =
    Button (newWidgetId (), [Fill X, Expand true], [ Text text, Command comm ], [])

  fun mkframe L =
    Frame (newWidgetId (), L, [ Side Top, Expand true, Fill Both ], [Relief Ridge, Borderwidth 2], [])
end



structure CONFIG = 
struct
  local open SmlTk in   
    fun msgWidth ()          = 600
    fun msgFont ()           = Normalfont []
    fun buttonRelief ()      = Groove
    fun buttonWidth ()       = 5
    fun buttonFont ()        = SansSerif []
    fun errorIconFilenm ()   = SmlTk.getLibPath () ^ "/images/stop.gif"
    fun warningIconFilenm () = SmlTk.getLibPath () ^ "/images/warning.gif"
    fun enterTextFont ()     = Typewriter [Large]
  end
end


               
structure ModalDialog =
struct
  open TkTypes SmlTk SmlTk21

  val ModalDialogs = ref ([] :Window list)

  fun openAndGrab (win :Window) =
    ( openWindow win; grab (selWindowWinId win) )

  fun openModal win =
  ( ModalDialogs := !ModalDialogs @ [ win ];
    case (!ModalDialogs) of [ win ] => openAndGrab win | _ => () )

  fun closeModalCont id cont =
  ( ModalDialogs := tl (!ModalDialogs);
    closeWindow id;
    cont ();
    case !ModalDialogs of [] => () | (win :: _) => openAndGrab win )
end



structure StdDialog =
struct
  open GUI_Misc ModalDialog

  fun buttonConf () =
    [ Width (CONFIG.buttonWidth()), Relief (CONFIG.buttonRelief()), Font ASM_GUI_Fonts.stdDialogFont ]
  fun textConf () =
    [ Width (CONFIG.msgWidth()), Font ASM_GUI_Fonts.stdDialogFont ]

  local
    fun OkCancelButtons win cont cancel =
      let fun cc () = (closeModalCont win cont)
          fun no () = (closeModalCont win cancel)
      in  Frame( newWidgetId(), 
                 [ Button(newWidgetId(), [Side Left], [Text "Cancel", Command no] @ (buttonConf()), []),
                   Button(newWidgetId(), [Side Right], [Text "OK", Command cc] @ (buttonConf()), []) ],
                 [ Side Bottom, Fill X ], [], [] )
      end

    fun YesNoCancelButtons win yesCont noCont cancelCont =
      let fun yes () = (closeModalCont win yesCont)
          fun no ()  = (closeModalCont win noCont)
          fun cancel () = (closeModalCont win cancelCont)
      in  Frame( newWidgetId(), 
                 [ Label (newWidgetId(), [Side Left], [Text "      "], []),
                   Button(newWidgetId(), [Side Left], [Text "Yes", Command yes] @ (buttonConf()), []),
		   Label (newWidgetId(), [Side Left], [Text "            "], []),
		   Button(newWidgetId(), [Side Left], [Text "No", Command no] @ (buttonConf()), []),
		   Label (newWidgetId(), [Side Left], [Text "            "], []),
                   Button(newWidgetId(), [Side Left], [Text "Cancel", Command cancel] @ (buttonConf()), []),
		   Label (newWidgetId(), [Side Left], [Text "      "], []) ],
                 [ Side Bottom, Fill X ], [], [] )
      end
  in
    fun confirm msg cc =
      let val wid = newWinId()
	  val pic = Label( newWidgetId(), [Side Left, Fill Y],
			   [ Icon (FileImage(CONFIG.warningIconFilenm(), newImageId())) ], [] )
	  val frm = Frame( newWidgetId(),
			   [ Message (newWidgetId(), [Side Top, Fill Both], [ Text msg ] @ textConf (), []),
			     OkCancelButtons wid cc noAction ], 
			   [ Side Top, Fill Both ], [], [])
      in openModal (mkWin4 (wid, [ WinTitle "" ], [pic, frm], noAction))
      end

    fun yesNoCancelDialog msg yes no cancel =
      let val wid = newWinId()
	  val pic = Label( newWidgetId(), [Side Left, Fill Y],
			   [ Icon (FileImage(CONFIG.warningIconFilenm(), newImageId())) ], [] )
	  val frm = Frame( newWidgetId(),
			   [ Message (newWidgetId(), [Side Top, Fill Both], [ Text msg ] @ textConf (), []),
			     YesNoCancelButtons wid yes no cancel ], 
			   [ Side Top, Fill Both ], [], [])
      in openModal (mkWin4 (wid, [ WinTitle "" ], [pic, frm], noAction))
      end

  end

  local
    fun errwrnwidgs iconpath msg cc = 
      let val icon = 
	    Label (newWidgetId(), [ Side Left, Fill Y ], [Icon (FileImage (iconpath, newImageId()))], [])
          val message =
	    Message (newWidgetId (), [ Side Top, Fill X ], [ Text msg ] @ textConf (),[])
          val continueButton =
	    Button (newWidgetId (), [ Side Right ], [ Text "Continue", Command cc ] @ buttonConf (), [])
      in [ icon, Frame (newWidgetId(), [ message, continueButton ], [Side Top, Fill Both], [], []) ]
      end

    fun errwrnwin title iconpath msg cc =
        let val wid      = newWinId()
            fun close () = closeModalCont wid cc
        in openModal (mkWin4 (wid, [ WinTitle title ], errwrnwidgs iconpath msg close, noAction))
        end
  in
    fun error_dialog  msg cc  = errwrnwin "Error" (CONFIG.errorIconFilenm()) msg cc
    fun warning_dialog msg cc = errwrnwin "Warning" (CONFIG.warningIconFilenm()) msg cc
  end

  val error_messages_enabled   = ref true
  val warning_messages_enabled = ref true

  fun error err_msg =
    if (!error_messages_enabled) then error_dialog err_msg noAction else ()
  fun warning warn_msg    =
    warning_dialog warn_msg noAction
end
