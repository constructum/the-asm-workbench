structure ErrorBox =
struct
  open GUI_Misc

  val debug = debug false "ErrorBox" 	
  fun debug0 s = debug s []


  val errorBox_id = mkWidgetId "errorBox"
  val sourceBrowser_id = SourceBrowser.sourceBrowser_id

  val errorPos = newAnnotationId ()
  val localizedError = newAnnotationId ()

  val currLine = ref 1

  fun simpleMsg s = 
    let val _ = debug "simpleMsg: $1" [ s ]
	      val s = (s^"\n")
        fun countLines (#"\n"::rest) = 1 + countLines rest
          | countLines (_::rest) = countLines rest
          | countLines _ = 0
        val lines = countLines (explode s)
    in setTextWidReadOnly errorBox_id false;
       insertTextEnd errorBox_id s;
       setTextWidReadOnly errorBox_id true;
       currLine := !currLine + lines
    end

  fun errorMsg ex =
    let fun errorMsg isLocalized =
          let val firstLine = !currLine
              val _ = simpleMsg (ASM.errorMessage ex)
          in  if isLocalized
	            then ( (delAnnotation errorBox_id localizedError) handle _ => ();
                      addAnnotation errorBox_id
                      ( TATag ( localizedError, [(Mark (firstLine,0), Mark (!currLine,0))],
			                [Background Blue], [] ) ) )
             else ()
          end
    in case ex of
	        ASM_Global.Error cont => errorMsg false
        | ASM_Signature.Error cont => errorMsg false
        | ASM_Lexer.Error cont  => errorMsg false
        | ASM_Parser.Error cont => errorMsg true
        | ASM_Check.Error cont  => errorMsg true
        | ASM_Eval.Error cont   => errorMsg true
        | ASM_Run.Error cont    => errorMsg true
        | ASM_Top.Error cont    => errorMsg false
        | _ => errorMsg false
    end


  fun removeOverlay () = 
  ( (delAnnotation sourceBrowser_id errorPos) handle _ => ();
    (delAnnotation errorBox_id    localizedError) handle _ => () )

  fun clearMessages () =
  ( debug0 "clearMessages";
    currLine := 1;
    setTextWidReadOnly errorBox_id false;
    clearText errorBox_id;
    setTextWidReadOnly errorBox_id true )

  fun mkframe L =
    Frame (newWidgetId (), L, [ Side Top, Expand true, Fill Both ], [Relief Ridge, Borderwidth 2], [])

  val ErrorBox =
    mkframe [ Frame ( newWidgetId (),
              [ Button (newWidgetId (), [Expand true, Fill Both], [Text "Clear Messages", Command clearMessages], [] ),
	        Button (newWidgetId (), [Expand true, Fill Both], [Text "Remove Overlay", Command removeOverlay], [] ) ],
		      [ Side Right, Fill Y ], [], [] ),
              TextWid ( errorBox_id, RightScb, AnnoText (NONE, "", []), [Side Top, Fill Both, Expand true],
	                [ Width 80, Height 5, Foreground Red, Font ASM_GUI_Fonts.errorBoxFont ], [] ) ]


  structure SDW = struct
    fun register () =
      StateDepWidget.register_widget (errorBox_id, fn id => redraw (), [ Filelist.gse_filelist, Filelist.gse_filelist_pos ])
                                        
    and unregister () =
      StateDepWidget.unregister_widget errorBox_id

    and redraw () =
    ( clearMessages ();
      let val error_messages = List.foldr (fn ((s, Filelist.Error e), msgs) => e::msgs | (_, msgs) => msgs) [] (Filelist.getFilelist ())
      in map errorMsg error_messages; ()
      end )
  end

end
