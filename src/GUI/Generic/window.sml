functor MakeWindow (
  WindowInfo :sig
    val win_id_str  : SmlTk.WinId option
    val window      : SmlTk.WinId -> SmlTk.WinConfigure list * SmlTk.Widget list
    val init        : unit -> unit
    val postprocess : unit -> unit
    val terminate   : unit -> unit
    val closeWindowRef : (unit -> unit) ref
  end
) =
struct
  open GUI_Misc WindowInfo
  fun debug s L = GUI_Misc.debug true "MakeWindow" s L

  val id =
    case win_id_str of SOME win_id => win_id
		     | NONE   => newWinId ()

  fun select () =
  ( debug "select ()" [];
    if not (occursWin id)
    then let val (title, contents) = window id
         in openWindow ( mkWin5 ( id, title, contents,
		                  [ BindEv ( Destroy,
                                             fn _ => ( debug "Destroy\noccursWin $1\n" [ Bool.toString (occursWin id) ];
                                                       terminate () ) ) ],
		             fn _ => ( init (); postprocess () ) ) )
         end
    else () )
      
  and update () =
  ( debug "update ()" [];
    if occursWin id then postprocess () else () )

  and close ()  =
  ( debug "close ()" [];
    if occursWin id then ( terminate (); closeWindow id ) else () )

  val _ =  ( closeWindowRef := close )
end
