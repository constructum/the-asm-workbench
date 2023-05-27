(* ***************************************************************************

   $Source: /repository/sml/sml_tk/src/tests+examples/wbind_ex.sml,v $

   Window bindings example

   $Date: 2001/03/30 13:39:35 $
   $Revision: 3.0 $
   Author: ludi (Last modification by $Author: 2cxl $)

   (C) 1999, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)


structure WBindEx : sig
                      val go : unit -> unit
                    end =

struct
  open SmlTk

  val exWinId  = newWinId()
  val secWinId = newWinId()
  val txtId    = newWidgetId()
  val labId    = newWidgetId()

  val txt =
      TextWid {widId      = txtId,
	       scrolltype = RightScb,
	       annotext   = mtAT,
	       packings   = [PadX 30, PadY 20],
	       configs    = [Width 60, Height 50, Background White,
			     Relief Raised, Active false],
	       bindings = []}

  fun clear _ =
      (addConf txtId [Active true];
       clearText txtId;
       addConf txtId [Active false])

  fun ins s _ =
      (addConf txtId [Active true];
       insertTextEnd txtId s;
       addConf txtId [Active false])

  val secWin =
      mkWindow {winId    = secWinId,
		config   = [WinTitle "non-init Window"],
		widgets  = Pack [Label {widId    = labId,
					packings = [PadX 20, PadY 30],
					configs  =
                                          [Background Blue, Foreground White,
					   Height 3, Width 70,
                     Text "Try to destroy this window (or even main window)!"],
					bindings = []}],
		bindings =
                  [BindEv(FocusIn, ins "Second window: Focus received\n"),
		   BindEv(FocusOut, ins "Second window: Focus lost\n"),
		   BindEv(Configure, ins "Second window: Window configured\n"),
		   BindEv(Map, ins "Second window: Window mapped (opened)\n"),
		   BindEv(Unmap,
			  ins "Second window: Window unmapped (iconified)\n"),
		   BindEv(Visibility,
			  ins "Second window: Visibility changed\n"),
		   BindEv(Destroy, ins "Second window closed!\n"),
		   BindEv(KeyPress "F2", clear),
		   BindEv(KeyPress "F3", fn _ => closeWindow secWinId)],
		init    = noAction}

  fun opSec _ =
      if occursWin secWinId then
	  insertTextEnd txtId "allready open!\n"
      else
	  openWindow secWin

  fun exitmsg _ =
      print "\nThank you for using the Window Bindings Example!\n\n"

  val buttons =
      Frame {widId    = newWidgetId(),
	     widgets  =
               Pack [Button {widId    = newWidgetId(),
			     packings = [Side Left, PadX 5],
			     configs  =
                               [Text "<F1> Open second Window",
				Background Blue, Foreground White,
				Command opSec],
			     bindings = []},
	             Button {widId    = newWidgetId(),
			     packings = [Side Right, PadX 5],
			     configs  =
                               [Text "<F3> Close", Width 8, Background Blue,
				Foreground White,
				Command(fn _ => (exitmsg();
						 closeWindow exWinId))],
			     bindings = []},
		     Button {widId    = newWidgetId(),
			     packings = [Side Right],
			     configs  =
                               [Text "<F2> Clear", Width 8, Background Blue,
				Foreground White, Command clear],
			     bindings = []}],
	     packings = [Side Bottom, Fill X, PadY 5],
	     configs  = [],
	     bindings = []}

  val testWin =
      mkWindow {winId    = exWinId,
		config   = [WinTitle "Window bindings example"],
		widgets  = Pack [txt, buttons],
		bindings =
                  [BindEv(FocusIn, ins "Focus received\n"),
		   BindEv(FocusOut, ins "Focus lost\n"),
		   BindEv(Configure, ins "Window configured\n"),
		   BindEv(Map, ins "Window mapped (opened)\n"),
		   BindEv(Unmap, ins "Window unmapped (iconified)\n"),
		   BindEv(Visibility, ins "Visibility changed\n"),
		   BindEv(Destroy, exitmsg),
		   BindEv(KeyPress "F1", opSec),
		   BindEv(KeyPress "F2", clear),
		   BindEv(KeyPress "F3", fn _ => closeWindow exWinId)],
		init     = noAction}

  fun go() = startTcl [testWin]
end
