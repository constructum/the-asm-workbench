(* ***********************************************************************

   Project: sml/Tk: an Tk Toolkit for sml
   Author: Stefan Westmeier, University of Bremen

  $Date: 2001/03/30 13:38:58 $
  $Revision: 3.0 $

   Purpose of this file: Functions related to Text Widget Annotations

   *********************************************************************** *)

structure Annotation : ANNOTATION = 
struct

(*
nonfix prefix;
*)

local open BasicTypes BasicUtil in


exception ANNOTATION of string


type widgetPackFun = bool -> TclPath -> IntPath -> bool option -> Widget ->
                     string

type widgetAddFun  = Widget list -> Widget -> WidPath           -> Widget list
type widgetDelFun  = Widget list -> WidId  -> WidPath           -> Widget list
type widgetUpdFun  = Widget list -> WidId  -> WidPath -> Widget -> Widget list

type widgetDelFunc = WidId -> unit
type widgetAddFunc = WinId -> WidPath -> Widget -> unit


fun selTextWidWidId (TextWid{widId,...})  = widId
  | selTextWidWidId _                        = 
    raise WIDGET "Annotation.selTextWidWidId applied to non-Text Widget"

fun selTextWidScrollType (TextWid{scrolltype,...}) = scrolltype
  | selTextWidScrollType _                       =
    raise WIDGET "Annotation.selTextWidScrollType applied to non-Text Widget"

fun selTextWidAnnoText (TextWid{annotext=a,...}) = a
  | selTextWidAnnoText _                         =
    raise WIDGET "Annotation.selTextWidText applied to non-Text Widget"

fun selTextWidPack (TextWid{packings,...}) = packings
  | selTextWidPack _                        =
    raise WIDGET "Annotation.selTextWidPack applied to non-Text Widget"

fun selTextWidConfigure (TextWid{configs,...}) = configs
  | selTextWidConfigure _                       =
    raise WIDGET "Annotation.selTextWidConfigure applied to non-Text Widget"

fun selTextWidBinding (TextWid{bindings,...}) = bindings
  | selTextWidBinding _                       =
    raise WIDGET "Annotation.selTextWidBinding applied to non-Text Widget"



fun updTextWidWidId (TextWid{scrolltype,annotext,
                             packings,configs,bindings,...}) wid = 
TextWid{widId=wid, scrolltype=scrolltype, annotext=annotext, 
        packings=packings, configs=configs, bindings=bindings}
  | updTextWidWidId _ _ = 
    raise WIDGET "Annotation.updTextWidWidId applied to non-Text Widget"

fun updTextWidScrollType (TextWid{widId=wid,annotext=at,packings=p,
                                  configs=c,bindings=b,...}) st = 
                          TextWid{widId=wid,annotext=at,packings=p,
                                  configs=c,bindings=b,scrolltype=st}
  | updTextWidScrollType _ _ = 
    raise WIDGET "Annotation.updTextWidScrollType applied to non-Text Widget"

fun updTextWidAnnoText (TextWid{widId=wid,scrolltype=st,packings=p,
                                configs=c,bindings=b,...}) at = 
                        TextWid{widId=wid,scrolltype=st,packings=p,
                                configs=c,bindings=b,annotext=at}
  | updTextWidAnnoText _ _ = 
    raise WIDGET "Annotation.updTextWidAnnoText applied to non-Text Widget"

fun updTextWidPack (TextWid{widId,scrolltype=st,annotext=at,configs=c,
                            bindings=b,...}) p = 
                    TextWid{widId=widId,scrolltype=st,annotext=at,configs=c,
                            bindings=b,packings=p}
  | updTextWidPack _ _ = 
    raise WIDGET "Annotation.updTextWidPack applied to non-Text Widget"

fun updTextWidConfigure (TextWid{widId=wid,scrolltype=st,annotext=at,
                                 packings=p,bindings=b,...}) c = 
                         TextWid{widId=wid,scrolltype=st,annotext=at,
                                 packings=p,bindings=b,configs=c}
  | updTextWidConfigure _ _ = 
    raise WIDGET "Annotation.updTextWidConfigure applied to non-Text Widget"

fun updTextWidBinding (TextWid{widId=wid,scrolltype=st,annotext=at,
                               packings=p,configs=c,...}) b = 
                       TextWid{widId=wid,scrolltype=st,annotext=at,
                               packings=p,configs=c,bindings=b}
  | updTextWidBinding _ _ = 
    raise WIDGET "Annotation.updTextWidBinding applied to non-Text Widget"

fun selAnnotationType (TATag _)    = ATTag
  | selAnnotationType (TAWidget _) = ATWidget

fun selAnnotationId (TATag{annId=tn,...})          = tn
  | selAnnotationId (TAWidget{annId=tn,...}) = tn

fun selAnnotationConfigure (TATag{configs,...})    = configs
  | selAnnotationConfigure (TAWidget{configs,...}) = configs

fun selAnnotationBinding (TATag{bindings,...}) = bindings
  | selAnnotationBinding  _               =
    raise ANNOTATION ("Annotation.selAnnotationBinding applied to non TATag")

fun selAnnotationMarks (TATag{marks,...})         = marks
  | selAnnotationMarks (TAWidget{mark,...}) = [(mark,mark)]

fun selAnnotationWidgets (TAWidget{widgets,...}) = selRawWids widgets
  | selAnnotationWidgets _                       =
    raise ANNOTATION ("Annotataion.selAnnotationWidgets applied to non TAWidget")

fun isAnnotationGrid (TAWidget {widgets,...}) =
    (case widgets of Pack _ => false
                   | _      => true)
  | isAnnotationGrid _                        =
    raise ANNOTATION "Annotation.isGrid applied to non TAWidget"

fun updAnnotationConfigure (TAWidget{annId=tn,mark=i,widgets=wids,
                                     bindings=b,...}) c = 
                            TAWidget{annId=tn,mark=i,widgets=wids,
                                     configs=c,bindings=b}
  | updAnnotationConfigure (TATag{annId=tn,marks=i,bindings=b,...}) c =
    TATag{annId=tn,marks=i,bindings=b,configs=c}

fun updAnnotationBinding (TAWidget{annId=tn,mark=i,widgets=wids,
                                   configs=c,...}) b = 
                          TAWidget{annId=tn,mark=i,widgets=wids,
                                   configs=c,bindings=b}
  | updAnnotationBinding (TATag{annId=tn,marks=i,configs=c,...}) b =
                          TATag{annId=tn,marks=i,configs=c,bindings=b}

fun updAnnotationWidgets (TAWidget{annId=tn,mark=i,configs=c,
                                   bindings=b,widgets=oldwids})
                         newwids =
    let
	val wids = case oldwids of Pack _ => Pack newwids
                                 | Grid _ => Grid newwids
    in
	TAWidget{annId=tn,mark=i,configs=c,bindings=b,widgets=wids}
    end
  | updAnnotationWidgets _ _ =
    raise ANNOTATION ("Annotation.updAnnotationWidgets applied to non TAWidget")


val selTextWidAnnotations = AnnotatedText.selAnno o selTextWidAnnoText
fun updTextWidAnnotations w a = updTextWidAnnoText w 
                                (AnnotatedText.updAnno (selTextWidAnnoText w) a)

val selTextWidText        = AnnotatedText.selText o selTextWidAnnoText 



fun get wid tn =
    let
	val anots = selTextWidAnnotations wid
	val item  = ListUtil.getx
	               (fn an => ((selAnnotationId an) = tn)) anots 
	                (ANNOTATION ("Annotation.get: " ^ tn ^ " not found"))
    in
	item
    end

fun getBindingByName wid tn name =
    let
	val anot = get wid tn
	val bis  = selAnnotationBinding anot
	val bi   = Bind.getActionByName name bis
    in
	bi
    end

fun upd widg tn nan =
    let
	val at    = selTextWidAnnoText widg
	val ans   = AnnotatedText.selAnno at
	val an    = ListUtil.getx (fn an => ((selAnnotationId an) = tn))
	                                   ans
					   (ANNOTATION ("annotation: " ^ tn ^ " not found"))
	val nans  = ListUtil.updateVal (fn an => ((selAnnotationId an) = tn))
	                                         nan
	                                         ans
	val nwidg = updTextWidAnnoText widg (AnnotatedText.updAnno at nans)
    in
	nwidg
    end


fun getTextWidWidgets (TextWid{widId=wid,scrolltype=st,annotext=at,packings=p,configs=c,bindings=b}) =
    let
	val widans = List.filter (fn an => (selAnnotationType an = ATWidget))
	                         (AnnotatedText.selAnno at)
	val wids   = map selAnnotationWidgets widans
	val wids'  = List.concat wids
    in
	wids'
    end
  | getTextWidWidgets _ =
    raise WIDGET "Annotation.getTextWidWidgets applied to non-Text Widget"

fun getTextWidAnnotationWidgetAssList (TextWid{widId=wid,scrolltype=st,annotext=at,packings=p,configs=c,bindings=b}) =
    let
  	val widans = List.filter (fn an => (selAnnotationType an = ATWidget)) 
	                         (AnnotatedText.selAnno at)
 	val wids   = map selAnnotationWidgets widans
    in
 	ListPair.zip(widans,wids)
    end
  | getTextWidAnnotationWidgetAssList _ =
    raise WIDGET "Annotation.getTextWidAnnotationWidgetAssList applied to non-Text Widget"


fun addTextWidWidget af (w as (TextWid _)) widg wp =
    let
	val _ = Debug.print 4 ("addTextWidWidget "^(selWidgetWidId w)^" "^(selWidgetWidId widg)^" "^wp)
	val (wId,nwp)     = Paths.fstWidPath wp      (* strip ".txt" *)
	val (wId',nwp')   = Paths.fstWidPath nwp      (* strip ".tfr" *)
    in
	if ( nwp' = "" ) then
	    raise ANNOTATION "Annotation.addTextWidWidget called for TAWidget-Toplevel"
	else
	    let
		val (wId'',nwp'') = Paths.fstWidPath nwp'
		val anwidass      = getTextWidAnnotationWidgetAssList w
		val (an,swidgs)   = ListUtil.getx
		                      (fn (c,(ws:Widget list)) => 
				       foldr
				         (fn (w,t) => ((selWidgetWidId w) = wId'') orelse t)
					 false ws)
				      anwidass 
				      (ANNOTATION ("Annotation.addTextWidWidget: subwidget " ^ wId'' ^ " not found" ))
		val _ = Debug.print 4 ("addTextWidWidget(ass): "^(selAnnotationId an)^" ["^
			       (StringUtil.concatWith ", " (map (selWidgetWidId) swidgs))^"]")

		val nswidgs       = af swidgs widg nwp'
		val nan           = updAnnotationWidgets an nswidgs
		val nwidg         = upd w (selAnnotationId nan) nan
	    in
		nwidg
	    end
    end
  | addTextWidWidget _ _ _ _ =
    raise WIDGET "Annotation.addTextWidWidget applied to non-Text Widget"

fun deleteTextWidWidget df (w as (TextWid _)) wid wp =
    let
	val _ = Debug.print 4 ("deleteTextWidWidget "^(selWidgetWidId w)^" "^wp)
	val (wId,nwp)     = Paths.fstWidPath wp         (* strip ".tfr" *)
	val (wId',nwp')   = Paths.fstWidPath nwp
	val anwidass      = getTextWidAnnotationWidgetAssList w
	val (an,swidgs)   = ListUtil.getx
	                       (fn (c,(ws:Widget list)) => 
				  foldr
				  (fn (w,t) => ((selWidgetWidId w) = wId') orelse t)
				  false ws)
			       anwidass 
			       (ANNOTATION ("Annotation.deleteTextWidWidget: subwidget " ^ wId' ^ " not found"))

	val nswidgs       = df swidgs wId' nwp'
	val nan           = updAnnotationWidgets an nswidgs
	val nwidg         = upd w (selAnnotationId nan) nan
    in
	nwidg
    end
  | deleteTextWidWidget _ _ _ _ =
    raise WIDGET "Annotation.deleteTextWidWidget applied to non-Text Widget"

fun updTextWidWidget uf (w as (TextWid _)) wid wp neww =
    let
	val _ = Debug.print 4 ("updTextWidWidget "^(selWidgetWidId w)^" "^wp)
	val (wId,nwp)     = Paths.fstWidPath wp         (* strip ".tfr" *)
	val (wId',nwp')   = Paths.fstWidPath nwp
	val anwidass      = getTextWidAnnotationWidgetAssList w
	val (an,swidgs)   = ListUtil.getx
	                       (fn (c,(ws:Widget list)) => 
				  foldr
				  (fn (w,t) => ((selWidgetWidId w) = wId') orelse t)
				  false ws)
			       anwidass 
			       (ANNOTATION ("Annotation.updTextWidWidget did not find Subwidget " ^ wId'))

	val nswidgs       = uf swidgs wId' nwp' neww
	val nan           = updAnnotationWidgets an nswidgs
	val nwidg         = upd w (selAnnotationId nan) nan
    in
	nwidg
    end
  | updTextWidWidget _ _ _ _ _ =
    raise WIDGET "Annotation.updTextWidWidgets applied to non-Canvas Widget"


fun pack pf tp (ip as (win, pt)) (TATag {annId = nm, marks = il,
					 configs = c, bindings = b}) =
    let
	val is   = Mark.showL il
	val conf = Config.pack ip c
    in
	(tp ^ " tag add " ^ nm ^ " " ^ is ^ "\n" ^
	 tp ^ " tag configure " ^ nm ^ " " ^ conf ^ "\n" ^
	 concat (Bind.packTag tp ip nm b))
    end
  | pack pf tp (ip as (win, pt)) (TAWidget {annId = nm, mark = i, widgets = ws,
					    configs = c, bindings = b}) =
    let
	val widId = nm
	val it    = Mark.show i
	val conf  = Config.pack ip c
	val frw   = Frame {widId = widId, widgets = ws, packings = [],
			   configs = [], bindings = []}
	val frtp  = tp ^ "." ^ widId
    in
	(pf true tp ip (SOME true) frw ^
	 tp ^ " window create " ^ it ^ " " ^ conf ^ " -window " ^ frtp ^ "\n")
(* 
 *	 ^ (Bind.packTag tp ip cid b)) )
 *)
    end


fun add pf widg an =
    let
	val ip as (win,pt) = Paths.getIntPathGUI (selWidgetWidId widg)
	val tp             = Paths.getTclPathGUI ip
	val nip            = (win,pt ^ ".txt")
	val ntp            = tp ^ ".txt"
	val ans            = selTextWidAnnotations widg
	val nans           = ans@[an]
	val nwidg          = updTextWidAnnotations widg nans
    in
	(Com.putTclCmd (pack pf ntp nip an);
	 nwidg)
    end

fun delete dwf widg tn =
    let
	fun delete' dwf widg (an as (TAWidget{annId=tn,widgets=ws,...})) =
	    let
		val wi             = tn
		val ip as (win,pt) = Paths.getIntPathGUI (selWidgetWidId widg)
		val tp             = Paths.getTclPathGUI ip
		val nip            = (win,pt ^ ".txt")
		val ntp            = tp ^ ".txt"
		val ans            = selTextWidAnnotations widg
		val nans           = List.filter (fn an => not ((selAnnotationId an) = tn)) ans
		val nwidg          = updTextWidAnnotations widg nans
	    in
		(app (dwf o selWidgetWidId) (selRawWids ws);
		 Com.putTclCmd ("destroy " ^ ntp ^ "." ^ wi);
(*		 Com.putTclCmd (ntp ^ " delete " ^ cid); 
 *)
		 nwidg)
	    end
	  | delete' dwf widg (an as (TATag{annId=tn,...})) =
	    let
		val ip as (win,pt) = Paths.getIntPathGUI (selWidgetWidId widg)
		val tp             = Paths.getTclPathGUI ip
		val nip            = (win,pt ^ ".txt")
		val ntp            = tp ^ ".txt"
		val ans            = selTextWidAnnotations widg
		val nans           = List.filter (fn an => not ((selAnnotationId an) = tn)) ans
		val nwidg          = updTextWidAnnotations widg nans
	    in
		(Com.putTclCmd (ntp ^ " tag delete " ^ tn);
		 nwidg)
	    end
	val an = get widg tn
    in
	delete' dwf widg an
    end


fun addAnnotationConfigure widg tn cf =
    let
	fun cmdText (TAWidget _) = " window configure "
	  | cmdText (TATag _)    = " tag configure "
	    
	val ip as (win,pt) = Paths.getIntPathGUI (selWidgetWidId widg)
	val tp             = Paths.getTclPathGUI ip
	val nip            = (win,pt ^ ".txt")
	val ntp            = tp ^ ".txt"
	val ans            = selTextWidAnnotations widg
	val an             = ListUtil.getx (fn an => ((selAnnotationId an) = tn))
	                                   ans 
					   (ANNOTATION ("annotation: " ^ tn ^ " not found"))
	val conf           = selAnnotationConfigure an
	val nconf          = Config.add conf cf
	val nan            = updAnnotationConfigure an nconf
	val nans           = ListUtil.updateVal (fn an => ((selAnnotationId an) = tn))
	                                         nan
	                                         ans
	val nwidg          = updTextWidAnnotations widg nans
    in
	(Com.putTclCmd (ntp ^ (cmdText an) ^ tn ^ " " ^
			Config.pack nip cf);
	 nwidg)
    end


fun addAnnotationBinding widg tn bi =
    let
	fun cmdText (TAWidget _) _ _ _ _     =
	    raise ANNOTATION "Annotation.addAnnotationBinding applied to non TATag"
	  | cmdText (TATag _) ntp nip tn bi = 
	    Bind.packTag ntp nip tn bi

	val ip as (win,pt) = Paths.getIntPathGUI (selWidgetWidId widg)
	val tp             = Paths.getTclPathGUI ip
	val nip            = (win,pt ^ ".txt")
	val ntp            = tp ^ ".txt"
	val ans            = selTextWidAnnotations widg
	val an             = ListUtil.getx (fn an => ((selAnnotationId an) = tn))
	                                   ans 
					   (ANNOTATION ("annotation: " ^ tn ^ " not found"))
	val bind           = selAnnotationBinding an
	val nbind          = Bind.add bind bi
	val nan            = updAnnotationBinding an nbind
	val nans           = ListUtil.updateVal (fn an => ((selAnnotationId an) = tn))
	                                         nan
	                                         ans
	val nwidg          = updTextWidAnnotations widg nans
    in
	(Com.putTclCmd (concat (cmdText an ntp nip tn bi));
	 nwidg)
    end


fun readSelection wid =
    let
	val ip   = Paths.getIntPathGUI (selWidgetWidId wid)
	val tp   = Paths.getTclPathGUI ip

	val ms   = Com.readTclVal (tp^ ".txt tag ranges sel")
    in
	Mark.readL ms
    end

fun readMarks wid tn =
    let
	val ip   = Paths.getIntPathGUI (selWidgetWidId wid)
	val tp   = Paths.getTclPathGUI ip

	val an   = get wid tn
    in
	case (selAnnotationType an) of
	    ATTag    => 
		Mark.readL (Com.readTclVal (tp^ ".txt tag ranges "^tn))
	  | ATWidget => 
		raise ANNOTATION ("Annotation.readMarks applied to non TATag")
    end



(* ************************************************************************ *)
(* 								            *)
(* Anonymous AnnotationId Manager					    *)
(* 									    *)
(* ************************************************************************ *)

val ANOTAGN_NR = ref(0);
fun newId() = (inc(ANOTAGN_NR);"anotagn"^Int.toString(!ANOTAGN_NR));

val ANOFRID_NR = ref(0);
fun newFrId() = (inc(ANOFRID_NR);"tfr"^Int.toString(!ANOFRID_NR));

end;

end;

