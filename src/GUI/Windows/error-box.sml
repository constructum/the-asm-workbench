structure ErrorBox =
struct
  open GUI_Misc 
  val errorBox_id = mkWidgetId "errorBox"
  val sourceBrowser_id = SourceBrowser.sourceBrowser_id

  val errorPos = newAnnotationId ()
  val localizedError = newAnnotationId ()

  val currLine = ref 1

  fun simpleMsg s = 
    let val s = (s^"\n")
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
          in if isLocalized
	     then ( (delAnnotation errorBox_id localizedError) handle _ => ();
                    addAnnotation errorBox_id
                      ( TATag ( localizedError, [(Mark (firstLine,0), Mark (!currLine,0))],
			        [Background Blue], [] ) ) )
             else ()
          end
        fun markErrorRegion (mark1, mark2) =
          let val _ = setTextWidReadOnly sourceBrowser_id false
              val _ = (delAnnotation sourceBrowser_id errorPos) handle _ => ()
              val _ = addAnnotation sourceBrowser_id
			( TATag ( errorPos, [(mark1, mark2)], [Foreground Red, Background Blue], [] ) )
	      val _ = setTextWidReadOnly sourceBrowser_id false
          in ()
	  end
        fun localizeError isParseError (SOME (ASM_Location.File s), optPos, _) =
            ( if Filelist.selectFile s
	      then case optPos of
		       SOME (ASM_Position.Pos { first = (l1,c1), last = (l2,c2) }) =>
                       ( if isParseError
			 then markErrorRegion (Mark (l1,c1-1), MarkToEnd l2)
			 else markErrorRegion (Mark (l1,c1-1), Mark (l2,c2));
			 true )
		     | NONE => false
	      else ( StdDialog.error (String_.replace "file '$1' not found in filelist" [ s ]);
		     false ) )
          | localizeError _ (_, _, _) = false
    in case ex of
	 ASM_Global.Error cont => errorMsg false
       | ASM_Signature.Error cont => errorMsg false
       | ASM_Lexer.Error cont  => errorMsg false
       | ASM_Parser.Error cont => (if localizeError true (#problem cont) then errorMsg true else errorMsg false)
       | ASM_Check.Error cont  => (if localizeError false (#problem cont) then errorMsg true else errorMsg false)
       | ASM_Eval.Error cont   => (if localizeError false (#problem cont) then errorMsg true else errorMsg false)
       | ASM_Run.Error cont    => (if localizeError false (#problem cont) then errorMsg true else errorMsg false)
       | ASM_Top.Error cont    => errorMsg false
       | _ => errorMsg false
    end


  fun removeOverlay () = 
  ( (delAnnotation sourceBrowser_id errorPos) handle _ => ();
    (delAnnotation errorBox_id    localizedError) handle _ => () )

  fun clearMessages () =
  ( currLine := 1;
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

end
