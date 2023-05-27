functor CHILD_WINDOWS (
  ChildWindowsInfo :sig
    type ELEM
    val get_win_id     :ELEM -> SmlTk.WinId
    val contents       :ELEM -> SmlTk.Widget list
    val init           :ELEM -> unit
    val postprocess    :ELEM -> unit
    val terminate      :ELEM -> unit
    val closeWindowRef :(ELEM -> unit) ref
  end ) =
struct
  open GUI_Misc ChildWindowsInfo

  val children = ref ([] :ELEM list)

  fun create (config, elem) =
    let val win_id = get_win_id elem
    in ( openWindow ( mkWin5 ( win_id, config, contents elem,
			       [ BindEv (Destroy, fn _ => close elem) ],
			       fn () => (init elem; postprocess elem) ) );
	 children := !children @ [ elem ] )
    end

  and update elem =
    if occursWin (get_win_id elem)
    then let val win_id = get_win_id elem
	     val win    = getWindow win_id
	     val config = selWindowConfigures win
	 in postprocess elem
	 end
    else ()

  and updateAll () =
  ( children := List.filter (occursWin o get_win_id) (!children);
    map update (!children); () )

  and close elem =
    let fun select id = (occursWin id) andalso (id <> get_win_id elem)
    in terminate elem;
       children := List.filter (select o get_win_id) (!children);
       closeWindow (get_win_id elem)
    end

  and closeAll () =
      ( map close (!children) )

  val _ = ( closeWindowRef := close )
end
