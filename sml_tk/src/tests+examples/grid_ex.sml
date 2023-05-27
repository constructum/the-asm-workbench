(* ***************************************************************************

   $Source: /repository/sml/sml_tk/src/tests+examples/grid_ex.sml,v $

   Grid geometry example

   $Date: 2001/03/30 13:39:32 $
   $Revision: 3.0 $
   Author: ludi (Last modification by $Author: 2cxl $)

   (C) 1999, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)


structure GridEx : sig
                     val go : unit -> unit
                   end =
struct
  open SmlTk

  fun label i b =
  let
    val words = ["Press", "mouse", "button", "to", "leave"]
  in
    Label {widId    = newWidgetId(),
           packings = [Column(if b then i else 5-i), Row i],
           configs  = [Text(List.nth(words, i-1)), Foreground White,
                       Background Blue, Width 8, Height 2],
           bindings = []}
  end

  val frames =
  let
    fun frame c r b =
      Frame {widId    = newWidgetId(),
             widgets  = Grid [label 1 b, label 2 b, label 3 b, label 4 b,
                              label 5 b],
             packings = [Column c, Row r],
             configs  = [Background Red],
             bindings = []}
  in
    [frame 1 1 true, frame 2 2 false, frame 3 3 true, frame 1 3 true,
     frame 3 1 true]
  end

  val mainWin =
    mkWindow {winId    = newWinId(),
              widgets  = Grid frames,
              config   = [WinTitle "grid geometry demonstration"],
              bindings = [BindEv(ButtonPress(SOME 1), fn _ => exitTcl())],
              init     = noAction}

  fun go() = startTcl [mainWin]
end
