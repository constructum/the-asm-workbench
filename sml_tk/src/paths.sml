(* *********************************************************************** *)
(*									   *)
(* Project: sml/Tk: an Tk Toolkit for sml	 			   *)
(* Author: Burkhart Wolff, University of Bremen	 			   *)
(* Date: 25.7.95				 			   *)
(* Purpose of this file: Functions related to Path-Management		   *)
(*									   *)
(* *********************************************************************** *)

(*
To enhance efficiency, the GUI data structure contains an association 
list from widget-id's to their internal paths. The internal path is a 
pair of the window-id of the window that contains the widget, and the 
widget-part of the path, i.e., the path without the window name.

Tcl/Tk, on the other hand, regards windows and widgets the same, 
except for the main window which has name ".". So here we need 
conversion from the internal path to the Tcl path.
*)

structure Paths : PATHS = 
struct

    open BasicTypes GuiState BasicUtil

(*
fun fstWidPath "" = ("","")
*)

fun fstWidPath s  = 
    let val (m1, m2) = Substring.splitl (not o StringUtil.isDot) 
	                                (Substring.triml 1 (Substring.full s))
    in  (Substring.string m1, Substring.string m2)
    end	

fun lastWidPath p = 
    let val (rp, rw) = Substring.splitr (not o StringUtil.isDot)
	                                (Substring.full p)
    in  
	if (Substring.size rp)= 0 then
	    ("", Substring.string rw)
	else
	    (Substring.string (Substring.trimr 1 rp), Substring.string rw)
    end


fun occursWindowGUI w = 
    List.exists (eq w) (map selWindowWinId (GuiState.getWindowsGUI()))

fun occursWidgetGUI w = 
    List.exists (eq w) (map fst (GuiState.getPathAssGUI()))


fun addWidget wid winid path ass =  ass @ [(wid,(winid,path))]

(* gwp :: WidId -> [PathAss] -> WidPath *)
(*
fun gwp w ass = snd (gip w ass);
*)
(* delWidPaths :: WidId -> [PathAss] -> [PathAss] *)
(*
fun delWidPaths w ass = 
    let val p = gwp w ass
    in  filter ((prefix p) o snd o snd) ass end;
*)

fun deleteWidget w ass = 
    List.filter ( (fn x => not (x= w)) o fst) ass;

fun deleteWidgetPath (wi,wp) ass = 
    List.filter ( (fn (x,y) =>not (x=wi andalso y=wp)) o snd) ass;


fun deleteWindow w ass = List.filter ((fn x => not (x= w)) o fst o snd) ass;


fun getTclPathGUI(w,p) = if GuiState.isInitWin w then p else ("." ^ w ^ p);

(* gip :: WidId -> [PathAss] -> IntPath *)
fun gip w ((x, y)::ass) = if w = x then y else gip w ass
  | gip w _             = raise WIDGET ("Error in function gip: WidId " ^ w 
					^ " undeclared.");

(* getIntPath :: WidId -> GUI s -> (IntPath, GUI s) *)
(* "assoc"; search in the assoc-list *)
fun getIntPathGUI w =  gip w (GuiState.getPathAssGUI());

fun getWidPathGUI wid = snd(getIntPathGUI(wid));   

fun getIntPathFromTclPathGUI tp = 
   let 
       val (front,r) = lastWidPath tp;
       val (front2,r2) = lastWidPath front;
       val wid = if (r="txt") andalso (occursWidgetGUI r2) then r2 else
	         if (r="cnv") andalso (occursWidgetGUI r2) then r2 else
                 if (r="box") andalso (occursWidgetGUI r2) then r2 else r
   in  
       (fst(getIntPathGUI wid), wid) 
   end;


(* ************************************************************************ *)
(* 									    *)
(* Anonymous WidId Manager						    *)
(* Purpose: Creates anonymous names for widgets, starting with "anowid"     *)
(* and a unique number.                                                     *)
(* 									    *)
(* ************************************************************************ *)

    val ANOWID_NR = ref(0)

    fun newWidgetId() = (inc(ANOWID_NR);
			 "anowid"^Int.toString(!ANOWID_NR))


end





