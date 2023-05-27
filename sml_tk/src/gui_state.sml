
structure GuiState:GUISTATE =
struct 

open BasicTypes 

    type GUI		= ((Window) list * (PathAss list))

    type TclAnswer  = string

    val  GUI_state  = ref([]:(Window) list,[]:PathAss list) 

local open BasicUtil
in

fun getWindowsGUI () =
    let
	val (windows,_) = !GUI_state
    in
	windows
    end

fun getPathAssGUI () =
    let 
	val (_,pathAss) = !GUI_state
    in 
	pathAss 
    end

fun getWindowGUI w = 
    ListUtil.getx ((eq w) o selWindowWinId) 
                  (getWindowsGUI()) 
                  (WINDOWS ("getWindowGUI with windowId \"" ^ w ^ "\""))


(* 2C. UPDATING WINDOWS *)

(* updateWin :: WinId -> Window s -> GUI s -> ((), GUI s) *)
fun updWindowGUI win newwin =
    let 
	val (wins, ass) = !GUI_state
    in  
	GUI_state := (ListUtil.updateVal ((eq win) o selWindowWinId) 
		      newwin wins, ass) 
    end

end  (* local open BasicUtil *)

fun updWindowsGUI nwins =
    let 
	val (wins, ass) = !GUI_state
    in  
	GUI_state := (nwins, ass) 
    end

fun updPathAssGUI nass =
    let 
	val (wins, ass) = !GUI_state
    in  
	GUI_state := (wins, nass) 
    end


fun updGUI (nwins,nass) = GUI_state := (nwins,nass)

(* I'm not sure if it could be called before the window is added to   *)
(* the internal GUI state. Therefore True as well if no window is     *) 
(* present as if it really is the first in the GUI state.             *)
(* isInitWin :: WinId -> GUI s -> (bool, GUI s) *)
fun isInitWin w = 
    (fn ([], _)    => true 
  | (win::wins, _) => (w = (selWindowWinId win))) (!GUI_state)


fun initGuiState() = (GUI_state:=([],[]));

end


