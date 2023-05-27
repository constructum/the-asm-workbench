(* *********************************************************************** *)
(*									   *)
(* Project: sml/Tk: an Tk Toolkit for sml	 			   *)
(* Author: Burkhart Wolff, University of Bremen	 			   *)
(* Date: 25.7.95				 			   *)
(* Purpose of this file: Functions related to "Tk-Bindings"	 	   *)
(*									   *)
(* *********************************************************************** *)

structure Bind : BIND = 
struct

local open BasicTypes in 

infix 1 bindElem;


fun bindEq (BindEv(k1,c1)) (BindEv(k2,c2)) = k1 = k2;

fun bindElemH(b,[]) = false
  | bindElemH(b,(x::xs)) = bindEq b x orelse bindElemH(b,xs);

val op bindElem = bindElemH;

fun noDblP [] = true
  | noDblP (x::xs) = not (x bindElem xs) andalso noDblP xs;


(* ***********************************************************************

   Convert Events to strings 

   *********************************************************************** *)

fun spToStr NONE    = ""
  | spToStr (SOME i)= "-"^(Int.toString i)

fun evName FocusIn            = "FocusIn"
  | evName FocusOut           = "FocusOut"
  | evName Configure          = "Configure"
  | evName Map                = "Map"
  | evName Unmap              = "Unmap"
  | evName Visibility         = "Visibility"
  | evName Destroy            = "Destroy"
  | evName (KeyPress str)     = "KeyPress-"^str
  | evName (KeyRelease str)   = "KeyRelease-"^str
  | evName (ButtonPress sp)   = "ButtonPress"^(spToStr sp)
  | evName (ButtonRelease sp) = "ButtonRelease"^(spToStr sp)
  | evName Enter              = "Enter"
  | evName Leave              = "Leave"
  | evName Motion             = "Motion"
  | evName (UserEv str)       = str
  | evName (Shift(KeyPress s))= "Shift-"^s
  | evName (Shift e)          = "Shift-"^(evName e)
  | evName (Ctrl (KeyPress s))= "Control-"^s
  | evName (Ctrl e)           = "Control-"^(evName e)
  | evName (Lock e)           = "Lock-"^(evName e)
  | evName (Any e)            = "Any-"^(evName e)
  | evName (Double e)         = "Double-"^(evName e)
  | evName (Triple e)         = "Triple-"^(evName e)
  | evName (ModButton(i, e))  = "Button"^(Int.toString i)^"-"^(evName e)
  | evName (Meta e)           = "Meta-"^(evName e)
  | evName (Alt  e)           = "Alt-"^(evName e)
  | evName (Mod3 e)           = "Mod3-"^(evName e)
  | evName (Mod4 e)           = "Mod4-"^(evName e)
  | evName (Mod5 e)           = "Mod5-"^(evName e)

fun eventName ev = "<"^(evName ev)^">"


(* ***********************************************************************

   selectors on Binding's

   *********************************************************************** *)


fun selEvent (BindEv(k,c)) = k;

fun selAction (BindEv(k,c)) = c;


fun getActionByName name [] = (fn e => ())
  | getActionByName name (x::xs) = 
                   if eventName(selEvent x) = name then selAction x 
		   else getActionByName name xs

(* ***********************************************************************

   defaults for Binding's

   *********************************************************************** *)

(* defaultBindPack : WidgetType -> Key -> string *)
fun defaultBindPack _ _ = "";

(* ***********************************************************************

   updating Binding's

   *********************************************************************** *)


fun addOneBind (c, [])    = [c]
  | addOneBind (c, x::xs) = if bindEq x c then c::xs else x::addOneBind(c, xs)

fun add old new = List.foldr addOneBind old new


fun deleteOneBind cs c = List.filter (not o(bindEq c)) cs;

fun delete old new = map selEvent (foldl (BasicUtil.twist (BasicUtil.uncurry (deleteOneBind))) old new);


(* ***********************************************************************

   Binding's  ==>  Tcl

   *********************************************************************** *)

(* packOneWindowBind : WinId -> Binding -> string *)
fun packOneWindowBind w (BindEv(e, _)) =
    if GuiState.isInitWin w then
	"bind . " ^ (eventName e) ^ " {if {\"%W\" == \".\"} {" ^
	Com.commToTcl ^ " \"WinBinding " ^ w ^ " " ^ (eventName e) ^ " " ^
	TkEvent.show() ^ " \"}}\n"
    else
	"bind ." ^ w ^ " " ^ (eventName e) ^ " {if {\"%W\" == \"." ^ w ^
	"\"} {" ^ Com.commToTcl ^ " \"WinBinding " ^ w ^ " " ^ (eventName e) ^
	" " ^ TkEvent.show() ^ " \"}}\n"

(* packWindow : WinId -> Binding list -> string list *)
fun packWindow w bs =
    map (packOneWindowBind w) bs

(* unpackOneWindowBind : TclPath -> Event -> string *)
fun unpackOneWindowBind tp e =
    "bind " ^ tp ^ " " ^ (eventName e) ^ " {}"

(* unpackWindow : TclPath -> Event list -> string list *)
fun unpackWindow tp es =
    map (unpackOneWindowBind tp) es

(* packOneWidgetBind : TclPath -> IntPath -> Binding -> string *)
fun packOneWidgetBind tp (w, p) (BindEv(e,com)) =
    "bind " ^ tp ^ " " ^ (eventName e) ^ " {" ^ Com.commToTcl ^
    " \"WBinding " ^ w ^ " " ^ p ^ " " ^ (eventName e) ^ " " ^
    TkEvent.show() ^ " \"}\n"

(* packWidget : TclPath -> IntPath -> Binding list -> string list *)
fun packWidget tp ip bs =
    map (packOneWidgetBind tp ip) bs

(* packOneCanvasBind : TclPath -> IntPath -> CItemId -> Binding -> string *)
fun packOneCanvasBind tp (w, p) cid (BindEv(e,com)) =
    tp ^ " bind " ^ cid ^ " " ^ (eventName e) ^ " {" ^ Com.commToTcl ^
    " \"CBinding " ^ w ^ " " ^ p ^ " " ^ cid ^ " " ^ (eventName e) ^
    " " ^ TkEvent.show() ^ " \"}\n"

(* packCanvas : TclPath -> IntPath -> Binding list -> string list *)
fun packCanvas tp ip cid bs =
    map (packOneCanvasBind tp ip cid) bs

(* packOneTagBind : TclPath -> IntPath -> AnnId -> Binding -> string *)
fun packOneTagBind tp (w, p) aid (BindEv(e,com)) =
    tp ^ " tag bind " ^ aid ^ " " ^ (eventName e) ^ " {" ^ Com.commToTcl ^
    " \"TBinding " ^ w ^ " " ^ p ^ " " ^ aid ^ " " ^ (eventName e) ^
    " " ^ TkEvent.show() ^ " \"}\n"

(* packTag : TclPath -> IntPath -> Binding list -> string list *)
fun packTag tp ip tn bs =
    map (packOneTagBind tp ip tn) bs

(* unpackOneBind : TclPath -> WidgetType -> Event -> string *)
fun unpackOneWidgetBind tp wt e =
    "bind " ^ tp ^ " " ^ (eventName e) ^ " {" ^ defaultBindPack wt e ^ "}" ^
    "\n"

(* unpackWidget : TclPath -> WidgetType -> Event list -> string list *)
fun unpackWidget tp wt es =
    map (unpackOneWidgetBind tp wt) es
end

end
