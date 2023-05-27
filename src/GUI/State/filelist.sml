structure Filelist =
struct
  val debug = GUI_Misc.debug false "Filelist"
  fun debug0 s = debug s []

  datatype FILE_STATE =
    NotLoaded
  | Error of exn
  | Loaded of string list   (* list of names defined in the file *)

  exception UndefinedPosition

  local
    val full_gui_update = StateDepWidget.full_gui_update

    val filelist = ref ([] :(string * FILE_STATE) ref list)
    val selected_file_pos = ref ~1
  in
    val gse_filelist = GUI_StateElem.register_elem "filelist"
    val gse_filelist_pos = GUI_StateElem.register_elem "filelist_pos"

    fun getFilenames () = map (#1 o !) (!filelist)
    fun getFileState f = (#2 o !) (valOf (List.find (fn (ref (f_, state)) => f = f_) (!filelist)))

    fun getFilelist () = map ! (!filelist)

    fun filelistToString () =
	let fun pairToString (ref (s, NotLoaded)) = String_.replace "($1, NotLoaded)" [ s ]
	      | pairToString (ref (s, Error _))   = String_.replace "($1, Error)" [ s ]
	      | pairToString (ref (s, Loaded _))  = String_.replace "($1, Loaded)" [ s ]
	in String_.replace "[ $1 ]" [ List_.output ", " (map pairToString (!filelist)) ]
	end

    fun resetFilelist newFilenames =
    ( debug "resetFilelist [ $1 ]" [ List_.output ", " newFilenames ];
      ASM.reset ();
      VisualOptions.load_visual_file ();
      ASM_GUI_State.asm_state_changed ();
      filelist := map (fn f => ref (f, NotLoaded)) newFilenames;
      GUI_StateElem.set_updated gse_filelist;
      selected_file_pos := ( if not (null (getFilenames ())) then 0 else ~1 );
      GUI_StateElem.set_updated gse_filelist_pos )

    fun reset () = resetFilelist []

    fun loadFiles () =
      let val _ = debug0 "loadFiles ()"
	  fun load1 (cell as ref (file, state)) =
	    let val definedNames =
	          ASM_Interface.catchAndForward ASM.loadFile' file
	    in cell := (file, Loaded definedNames); true
	    end handle ex => ( cell := (file, Error ex); false )
	  fun iterate i [] = ()
	    | iterate i ((cell as ref (file, state)) :: rest) =
	        if load1 cell then iterate (i+1) rest else ()
      in iterate 0 (!filelist);
	 GUI_StateElem.set_updated gse_filelist; full_gui_update ()
      end handle _ => ( GUI_StateElem.set_updated gse_filelist; full_gui_update () )

    fun loadNewFiles newFilenames =
    ( debug "loadNewFiles [ $1 ]" [ List_.output ", " newFilenames ];
      resetFilelist newFilenames;
      loadFiles ();
      full_gui_update () )

    fun reloadOldFiles () =
    ( debug0 "reloadOldFiles ()";
      resetFilelist (getFilenames ());
      loadFiles ();
      full_gui_update ())
      
    fun addFile newFilename =
    ( debug "addFile";
      loadNewFiles (getFilenames () @ [ newFilename ]) )

    fun validPosition () =
      (!selected_file_pos) >= 0 andalso (!selected_file_pos) < length (getFilenames ())
				     
    fun insertFileAt (newFilename, pos) =
    ( debug "insertFileAt ($1, $2)" [ newFilename, Int.toString pos ];
      if pos < 0 orelse pos >= length (getFilenames ())
      then raise UndefinedPosition
      else loadNewFiles (List_.insert_at (newFilename, getFilenames (), pos)) )

    fun insertFile newFilename =
    ( (* insert file at selected position *)
      debug "insertFile ($1)" [ newFilename ];
      insertFileAt (newFilename, !selected_file_pos) )
	
    fun removeFileAt pos =
    ( debug "removeFileAt ($1)" [ Int.toString pos ];
      if pos < 0 orelse pos >= length (getFilenames ())
      then raise UndefinedPosition
      else loadNewFiles (List_.remove_at (getFilenames (), pos)) )

    fun removeFile () =
    ( (* remove file at selected position *)
      debug0 "removeFile ()";
      removeFileAt (!selected_file_pos)	 )
	
    fun getSelectedFile () =
      let val file =
            if !selected_file_pos >= 0 andalso !selected_file_pos < length (getFilenames ())
	    then List.nth (getFilenames (), !selected_file_pos)
	    else ""
      in debug "getSelectedFile () = '$1'" [ file ];
	 file
      end
	       
    fun setSelectedPosition i =
    ( debug "setSelectedPosition ($1)" [ Int.toString i ];
      if i >= 0 andalso i < length (getFilenames ())
      then selected_file_pos := i
      else selected_file_pos := ~1;
      GUI_StateElem.set_updated gse_filelist_pos )
      
    fun getSelectedPosition () =
      let val pos = (!selected_file_pos)
      in debug "getSelectedPosition () = $1" [ Int.toString pos ];
	 pos
      end

    fun selectFile filename =
    ( debug "selectFile ($1)" [ filename ];
      case List_.indexOf (filename, getFilenames ()) of
	  NONE => ( setSelectedPosition (~1); false )
	| SOME i => ( setSelectedPosition i; true ) )

    (* !!! only temporary for compatibility *)
    fun getSpecificationNames () =
      List.concat (map ((fn Loaded names => names | _ => []) o getFileState)
		       (getFilenames ()))
  end
end
