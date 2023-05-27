structure Browser =
struct
(*  open GUI_LIB*)
  open TkTypes SmlTk
  open GUI_Misc
  val debug = debug true "Browser"
		
  exception Browser
  type BROWSER =
    WidId				(* initial browser widget id *)
    * Widget ref			(* the browser widget (ref.) *)
    * string				(* filename *)
(*  * (Mark * Mark) list ref		(* mark list *) *)

  val browser_list = ref ([] :BROWSER list)

  fun get_wid_id ((wid_id, _, _) :BROWSER) = wid_id
  fun get_widget ((_, widget_ref, _) :BROWSER) = widget_ref
  fun get_filename ((_, _, filename) :BROWSER) = filename

  fun find_browser wid_id =
    let val L = List.filter (fn browser => get_wid_id browser = wid_id) (!browser_list)
    in (hd L)  handle Hd => raise Browser
    end

  fun current_wid_id wid_id =
    let val (wid_id, widget_ref, _) = find_browser wid_id
    in selWidgetId (!widget_ref)
    end

  fun update_browser_list (wid_id, filename) =
    browser_list := 
      map
        ( fn (b as (wid_id, widget_ref, _)) =>
	    if (get_wid_id b = wid_id) then (wid_id, widget_ref, filename) else b )
	(!browser_list)

  fun statewid (widgetId: WidId, appearance) = 
    TextWid (widgetId, RightScb, AnnoText (NONE, "", []), [Side Left, Side Top, Fill Both, Expand true],
	     [Relief Raised, Font ASM_GUI_Fonts.browserFont] @ appearance, [])

  fun more (str: string, stateId: WidId) = 
    let val in_str = TextIO.openIn str 
    in ( while not (TextIO.endOfStream in_str) do
	 insertTextEnd stateId (TextIO.inputN (in_str,100));
	 TextIO.closeIn in_str
       ) end 

  fun load (wid_id, filename :string) =
  ( let val _ = debug "load ('$1', '$2')" [ SmlTk.mkWidgetString wid_id, filename ]
	val actual_wid_id = current_wid_id wid_id
	val _ = setTextWidReadOnly actual_wid_id false
    in clearText actual_wid_id;
       if filename <> ""
       then if readable filename
	    then ( more (filename, actual_wid_id);
		   update_browser_list (wid_id, filename) )
	    else ( StdDialog.error ("open_in  " ^ filename ^ " : \nopenf failed, permission denied");
		   update_browser_list (wid_id, "") )
       else ();
       setTextWidReadOnly actual_wid_id true
    end ) handle Browser => ()

  fun clear wid_id =
  ( let val actual_wid_id = current_wid_id wid_id
    in clearText actual_wid_id
    end ) handle Browser => ()

  fun create (wid_id, filename :string, appearance) =
    let	val widget = statewid (wid_id, appearance)
	val new_browser = (wid_id, ref widget, filename)
    in browser_list := !browser_list @ [ new_browser ];
       widget
    end

  fun redisplay wid_id =
  ( let val _ = debug "redisplay '$1'" [ SmlTk.mkWidgetString wid_id ]
	val browser = find_browser wid_id
    in load (wid_id, get_filename browser)
    end ) handle Browser => ()

  exception AccessDenied
  fun save_string (s :string, out_filename :string) =
    let fun savestr (out_file, i) =
	  let val how_many = Int.min (size s - i, 128)
	  in if how_many > 0
	     then ( TextIO.output (out_file, substring (s, i, how_many)); savestr (out_file, i + how_many) )
	     else TextIO.closeOut out_file
	  end
    in copy_file (TextIO.openIn out_filename, TextIO.openOut (out_filename ^ ".bak")) handle _ => ();
       savestr (TextIO.openOut out_filename, 0)
    end

  fun save wid_id =
    let exception IO_ERROR
        val browser = find_browser wid_id
	val filename = get_filename browser
	val actual_wid_id = current_wid_id wid_id
        val new_contents = readTextAll actual_wid_id
    in ( if not (writable filename) then raise AccessDenied else ();
	 save_string (new_contents, filename) handle _ => raise IO_ERROR;
	 redisplay wid_id )
	handle AccessDenied => StdDialog.error (filename ^ ": \nPermission denied") 
	     | IO_ERROR	=> StdDialog.error (filename ^ ": \nI/O ERROR") 
    end
end
