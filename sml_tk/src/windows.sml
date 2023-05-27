(* *********************************************************************** *)
(*									   *)
(* Project: sml/Tk: an Tk Toolkit for sml	 			   *)
(* Author: Burkhart Wolff, University of Bremen	 			   *)
(* Date: 25.7.95				 			   *)
(* Purpose of this file: Abstract data Type Window			   *)
(*									   *)
(* *********************************************************************** *)


structure Window : WINDOW = 
struct

open BasicUtil BasicTypes GuiState


(* *********************************************************************** *)
(*									   *)
(* IMPLEMENTATION: WINDOWS			 			   *)
(*									   *)
(* *********************************************************************** *)


(* functions related to Window Bindings *)

fun selWindowBinding (_, _, _, bds, _) = bds

fun selectBindKeyPath win name =
    Bind.getActionByName name (selWindowBinding(GuiState.getWindowGUI win))


(* I'm not sure if it could be called before the window is added to   *)
(* the internal GUI state. Therefore True as well if no window is     *) 
(* present as if it really is the first in the GUI state.             *)
(* isInitWin :: WinId -> GUI s -> (bool, GUI s) *)

(* Moved To BASIC_TYPES for visibility reasons
fun   isInitWin w = 
     (fn ([], _) => true | (win::wins, _) => (w = (winId win))) (!GUI_state);
*)


(* CHECKING the INTEGRITY of a WINDOW *)

(* window title may contain alphanumerical characters only *)

fun checkWinId "" = false
  | checkWinId s  = 
    (Char.isLower(String.sub(s, 0))) andalso (StringUtil.all Char.isAlphaNum s)

val checkTitle = StringUtil.all Char.isPrint 

fun check (win as (w, wcnfgs, wids, _, _)) = 
    let
	val mbt = Config.selWinTitle win
	val bb  = checkWinId w
    in
	case mbt of
	    NONE   => bb
	  | SOME t => checkTitle t andalso bb
    end;

fun appendGUI w = updWindowsGUI(getWindowsGUI() @ [w]);

fun addGUI (w as (winId,wcnfgs,widgs,binds,act)) = 
    if check w then
	if Paths.occursWindowGUI (selWindowWinId w) then
	    raise WINDOWS ("Two identical window names not allowed: " ^ 
			   (selWindowWinId w))
	else 
	    let
		val tmpWin = (winId,wcnfgs,
                              if isWindowGrid w then Grid [] else Pack [],
                              binds,act)
	    in
		(appendGUI tmpWin;
		 WidgetTree.addWidgetsGUI winId "" (selRawWids widgs))
	    end
    else 
	raise WINDOWS ("Definition of window " ^ selWindowWinId w ^ " is not OK");

fun deleteGUI w  = 
    let
	val wins  = getWindowsGUI()
	val ass   = getPathAssGUI()
	val nwins = List.filter ((fn x => not (w=x)) o selWindowWinId) wins
	val nass  = Paths.deleteWindow w ass
    in 
	updGUI(nwins,nass)
    end;

val deleteAllGUI = updGUI([], [])


(* 2F. EXPORTED FUNCTIONS *)

fun openW (w as (win, wconfigs, widgets, bindings, init_action)) =
    (addGUI w;
     if isInitWin win then
	 (Com.putTclCmd (concat (map (Config.packWinConf ".")
				     wconfigs) ^
			 concat (Bind.packWindow win bindings) ^
			 "bind . <Destroy> {if {\"%W\" == \".\"} {" ^
			 Com.commToTcl ^ " \"Destroy " ^
			 win ^ " <Destroy> " ^ TkEvent.show() ^ " \"}}\n" ^
			 WidgetTree.packWidgets true "" (win, "") NONE
			                        (selRawWids widgets)))
     else
	 (Com.putTclCmd ("toplevel ." ^ win ^ "\n" ^
			 concat (map (Config.packWinConf ("." ^ win))
				     wconfigs) ^
			 concat (Bind.packWindow win bindings) ^
			 "bind ." ^ win ^ " <Destroy> {if {\"%W\" == \"." ^
			 win ^ "\"} {" ^ Com.commToTcl ^
			 " \"Destroy " ^ win ^ " <Destroy> " ^
			 TkEvent.show() ^ " \"}}\n" ^
			 (WidgetTree.packWidgets true ("." ^ win) (win, "")
			                         NONE (selRawWids widgets))));
     init_action())

fun close win =
    if isInitWin win then
	(Com.exitTcl();
         deleteAllGUI)
    else
	(Com.putTclCmd ("destroy ." ^ win);
         deleteGUI win)

fun changeTitle winid title =
    let 
	val win   = getWindowGUI winid
	val wc    = selWindowConfigures win
	val wc'   = Config.addWinConf wc [WinTitle title]
	val win'  = updWindowConfigures win wc'
    in
	if checkTitle title then
	    (updWindowGUI winid win';
	     if isInitWin winid then
		 Com.putTclCmd (Config.packWinConf "." (WinTitle title))
	     else 
		 Com.putTclCmd (Config.packWinConf ("."^winid) (WinTitle title)))
	else 
	    raise WINDOWS ("Title " ^ title ^ " for window " ^ winid ^ " is not OK")
    end;

end;
