(* ***********************************************************************

   Project: sml/Tk: an Tk Toolkit for sml
   Author: Stefan Westmeier, University of Bremen
  $Date: 2001/03/30 13:39:22 $
  $Revision: 3.0 $
   Purpose of this file: Operations on Widgets Contents

   *********************************************************************** *)

structure WidgetOps : WIDGET_OPS = 
struct

open BasicTypes BasicUtil 


(* selectText has to look for the actual text in case of an 
   Entry- TextWid or Listbox widget *)
fun selectText wid (m1,m2) = 
    let 
	val gvf = Com.readTclVal
	val ip =  Paths.getIntPathGUI wid;
        val w  =  WidgetTree.getWidgetGUIPath ip
    in 
	case w of
	    Entry _   => 
		gvf((Paths.getTclPathGUI ip) ^ " get")
	  (* Absolute Notloesung. Unklar wie man Selektionen findet !!! *)
	  | TextWid _ => 
		(Com.putLine (Com.writeMToTcl^" ["^(Paths.getTclPathGUI ip) ^ 
			      ".txt get "^(Mark.show m1)^ " " ^
			      (Mark.show m2)^"]") ;
		 Com.getLineM())
	  | Listbox _ => 
		let 
		    val (mt1,_)=StringUtil.breakAtDot (Mark.show m1)
		    val (mt2,_)=StringUtil.breakAtDot (Mark.show m2)
		in  
		    gvf((Paths.getTclPathGUI ip)^".box get "^mt1^" "^mt2)
		end
	  | _  => Config.selText w
    end

fun selectTextAll wid = selectText wid (Mark(0,0),MarkEnd);


fun selectSelRange wid =
    let 
	val gvf = Com.readTclVal
	val ip =  Paths.getIntPathGUI wid
        val w  =  WidgetTree.getWidgetGUIPath ip
	fun mkMark str = 
	    let val (x, y)= StringUtil.breakAtDot str
	    in  Mark(StringUtil.toInt x, StringUtil.toInt y)
	    end  
	fun group []  = [] 
          | group (a::[])   = []  (* hmmm ... ? *)
	  | group (a::b::s) = (a,b)::group(s) 
    in 
	case w of
	    TextWid _ => 
		let
		    val s = gvf((Paths.getTclPathGUI ip)^".txt tag ranges sel")
		in
		    group(map mkMark (StringUtil.words s))
		end
	  | Listbox _ => 
		 let 
		     val t = gvf((Paths.getTclPathGUI ip)^".box curselection")
		 in  
		     if t="" then 
			 []
		     else 
			 [(Mark(StringUtil.toInt t,0),MarkEnd)]
		 end
          (* Entry ????????? *)
	  | _            => []
    end


fun selectSelWindow () = 
    let 
	val gvf = Com.readTclVal
	val t   = gvf("selection own")
    in  
	if t = "" then NONE else SOME (Paths.getIntPathFromTclPathGUI t)
    end


(* selectCursor has to look for the actual cursor position in Listboxes  *)
(* and TextWidgets *)

fun selectCursor wid =
    let 
	val gvf = Com.readTclVal
	val ip =  Paths.getIntPathGUI wid;
        val w  =  WidgetTree.getWidgetGUIPath ip
    in 
	case w of
	    TextWid _ => 
		 let 
		     val t = gvf((Paths.getTclPathGUI ip)^".txt index insert")
		     val (m1,m2)= StringUtil.breakAtDot t
		 in 
		     Mark(StringUtil.toInt m1, StringUtil.toInt m2) 
		 end
	  | Listbox _ =>
		 let
		     val t = gvf((Paths.getTclPathGUI ip)^".box curselection")
(*		     val _ = Debug.print 2 ("SelectCursor: t= >"^t^"<") *)
		 in
		     if ( t = "" ) then
			 raise WIDGET "WidgetOps.selectCursor: no selection"
		     else
			 Mark(Option.getOpt(Int.fromString t, 0),0)
		 end
          (* Entry ????????? *)
	  | _            => 
		Mark(0,0)
    end


fun readTextWidState wid =
    let 
	val widg = WidgetTree.getWidgetGUI wid
	val tp   = (Paths.getTclPathGUI o Paths.getIntPathGUI) wid

    in
	if ( (selWidgetWidgetType widg) = Tex ) then
	    case Com.readTclVal (tp ^ ".txt cget -state") of
		"normal"   => false (* TextWidStateNormal *)
	      | "disabled" => true  (* TextWidStateDisabled *)
	else
	    raise WIDGET "WidgetOps.readTextWidState: applied to non-TextWidget"
    end

fun setTextWidReadOnly wid st =
    let 
	val widg = WidgetTree.getWidgetGUI wid
	val tp   = (Paths.getTclPathGUI o Paths.getIntPathGUI) wid
    in
	if ( (selWidgetWidgetType widg) = Tex ) then
	    Com.putTclCmd (tp ^ ".txt configure -state " ^ (Config.showState st))
	else
	    raise WIDGET "WidgetOps.setTextWidState: applied to non-TextWidget"
    end

(* wrapper for functions doing things to text widgets: if it is read-only
 * we need to temporarily make it writable, otherwise nothing happens
 * (and the programmer is mightily confused).
 *)
fun doTextWid f wid =     
    let val oldSt = readTextWidState wid
    in
	setTextWidReadOnly wid false;
	f wid;
	if oldSt then setTextWidReadOnly wid oldSt else () 
    end


fun clearAnnoText wid =
    let
	val widg  = WidgetTree.getWidgetGUI wid
	val anl   = AnnotatedText.selAnno (Annotation.selTextWidAnnoText widg)
    in
	doTextWid (fn w=> (app (fn an => AnnotationTree.delete w 
				(Annotation.selAnnotationId an)) anl;
			   WidgetTree.clearText w)) wid
    end

fun replaceAnnoText wid ats =
    (clearAnnoText wid;
     doTextWid (fn w=> (WidgetTree.insertTextEnd w (AnnotatedText.selText ats);
			app (fn an => AnnotationTree.add w an)
			    (AnnotatedText.selAnno ats))) wid)

fun deleteAnnoText wid marks =
    (* TBD: delete annotations as well !! *)
    doTextWid (fn w=> WidgetTree.deleteText w marks) wid

(* insert annotated text into text widgets *)
fun ins_at wid at (r, c) =
    let val str   = AnnotatedText.selText at
	(* have to adjust annotations of the AT we want to insert *)
	val annos = AnnotatedText.adjustMarks {rows=r, cols=c} 
	                                      (AnnotatedText.selAnno at)
    in  doTextWid (fn w=> (WidgetTree.insertText w str (Mark (r, c));
			   app (AnnotationTree.add w) annos)) wid
    end
	
fun insertAnnoText wid at (Mark (r, c))= ins_at wid at (r, c)
  | insertAnnoText wid at (MarkToEnd r)= ins_at wid at (r, 0) (* WRONG! *)
  | insertAnnoText wid at (MarkEnd)    = 
    (* very inefficient as it counts the length of the whole text-- yuck *)
    let val (r, c)= AnnotatedText.lenAT (selectTextAll wid)
    in  ins_at wid at (r, c)
    end

fun insertAnnoTextEnd wid at = insertAnnoText wid at MarkEnd


(* No check that this variable is really defined!!! *)

fun selectVarValue var = 
    Com.readTclVal("global "^var^"; set "^ var)


fun setVarValue var value = 

    ignore (Com.readTclVal("global "^var^"; set " ^ var ^ " " ^ value))


fun createAndPopUpMenu widg index co =
    let
	val winid = Paths.newWidgetId()
	val frmid = Paths.newWidgetId()
	val frm   = Frame{widId=frmid, widgets=Pack[widg], packings=[],
                          configs=[], bindings =[]}; 
	val wid   = selWidgetWidId widg
    in
	Window.openW(winid, [], Pack [frm], [], fn()=> ());
	WidgetTree.popUpMenu wid index co
    end

fun setScaleValue wid r =
let 
  val widg = WidgetTree.getWidgetGUI wid
  val tp   = (Paths.getTclPathGUI o Paths.getIntPathGUI) wid
in
  Com.putTclCmd(tp ^ " set " ^ Config.showReal r)
end

end






