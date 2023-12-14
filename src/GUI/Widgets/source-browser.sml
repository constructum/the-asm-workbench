structure SourceBrowser =
struct
  open GUI_Misc
  val debug = debug false "SourceBrowser"

  val sourceBrowser_id = mkWidgetId "sourceBrowser"

  val errorPos = newAnnotationId ()

  fun errorMsg ex =
    let fun markErrorRegion (mark1, mark2) =
          let val _ = setTextWidReadOnly sourceBrowser_id false
              val _ = (delAnnotation sourceBrowser_id errorPos) handle _ => ()
              val _ = addAnnotation sourceBrowser_id
			                    ( TATag ( errorPos, [(mark1, mark2)], [Foreground Red, Background Blue], [] ) )
	            val _ = setTextWidReadOnly sourceBrowser_id true
          in ()
	        end
	      fun localizeError isParseError (SOME (ASM_Location.File s), optPos, _) =
            ( if Filelist.selectFile s
	            then  case optPos of
		                  SOME (ASM_Position.Pos { first = (l1,c1), last = (l2,c2) }) =>
                        ( if isParseError
			                    then markErrorRegion (Mark (l1,c1-1), MarkToEnd l2)
			                    else markErrorRegion (Mark (l1,c1-1), Mark (l2,c2));
			                    true )
		                  | NONE => false
	            else  ( StdDialog.error (String_.replace "file '$1' not found in filelist" [ s ]);
		                  false ) )
          | localizeError _ (_, _, _) = false
    in case ex of
	        ASM_Global.Error cont => ()
        | ASM_Signature.Error cont => ()
        | ASM_Lexer.Error cont  => ()
        | ASM_Parser.Error cont => (localizeError true (#problem cont); () )
        | ASM_Check.Error cont  => (localizeError false (#problem cont); () )
        | ASM_Eval.Error cont   => (localizeError false (#problem cont); () )
        | ASM_Run.Error cont    => (localizeError false (#problem cont); () )
        | ASM_Top.Error cont    => ()
        | _ => ()
    end



  fun create () =
  ( debug "create ()" [];
    Browser.create (sourceBrowser_id, "", [ Height 32, Width 80 ]) )

  fun redraw widId =
  ( let val filename = Filelist.getSelectedFile ()
    in  Browser.load (widId, filename);
        if filename <> ""
        then  case Filelist.getFileState (filename) of
                Filelist.Error err => errorMsg err
              | _ => ()
        else  ()
    end )
  
  structure SDW = MakeStateDepWidget (
    val widget = create ()
    val redraw = redraw
    val dependencies = [ Filelist.gse_filelist, Filelist.gse_filelist_pos ]
  )
end
