(* ***********************************************************************

   Project: sml/Tk: an Tk Toolkit for sml
   Author: Stefan Westmeier, University of Bremen

  $Date: 2001/03/30 13:39:03 $
  $Revision: 3.0 $

   Purpose of this file: Functions related to Canvas Items

   *********************************************************************** *)

structure CItem : C_ITEM = 
struct

local open BasicTypes BasicUtil in


exception CITEM of string

type widgetPackFun = bool -> TclPath -> IntPath -> bool option -> Widget ->
                     string

type widgetAddFun  = Widget list -> Widget -> WidPath           -> Widget list
type widgetDelFun  = Widget list -> WidId  -> WidPath           -> Widget list
type widgetUpdFun  = Widget list -> WidId  -> WidPath -> Widget -> Widget list

type getValFun     = string -> string

type widgetDelFunc = WidId -> unit
type widgetAddFunc = WinId -> WidPath -> Widget -> unit



fun selCanvasWidId (Canvas{widId,...}) = widId
  | selCanvasWidId _                       = 
    raise WIDGET "CItem.selCanvasId applied to non-Canvas Widget"

fun selCanvasScrollType (Canvas{scrolltype,...}) = scrolltype
  | selCanvasScrollType _                      =
    raise WIDGET "CItem.selCanvasScrollType applied to non-Canvas Widget"

fun selCanvasItems (Canvas{citems,...}) = citems
  | selCanvasItems _                       =
    raise WIDGET "CItem.selCanvasItems applied to non-Canvas Widget"

fun selCanvasPack (Canvas{packings,...}) = packings
  | selCanvasPack _                     =
    raise WIDGET "CItem.selCanvasPack applied to non-Canvas Widget"

fun selCanvasConfigure (Canvas{configs,...}) = configs
  | selCanvasConfigure _                     =
    raise WIDGET "CItem.selCanvasConfigure applied to non-Canvas Widget"

fun selCanvasBinding (Canvas{bindings,...}) = bindings
  | selCanvasBinding _                       =
    raise WIDGET "CItem.selCanvasBinding applied to non-Canvas Widget"



fun updCanvasWidId (Canvas{widId,scrolltype,citems,
                           packings,configs,bindings}) wid = 
    Canvas{widId=wid,scrolltype=scrolltype,citems=citems,
           packings=packings,configs=configs,bindings=bindings}
  | updCanvasWidId _                        _   = 
    raise WIDGET "CItem.updCanvasWidId applied to non-Canvas Widget"

fun updCanvasScrollType (Canvas{widId,scrolltype,citems,
                         packings,configs,bindings}) st = 
    Canvas{widId=widId,scrolltype=st,citems=citems,
           packings=packings,configs=configs,bindings=bindings}
  | updCanvasScrollType _                        _   = 
    raise WIDGET "CItem.updCanvasScrollType applied to non-Canvas Widget"

fun updCanvasItems (Canvas{widId,scrolltype,citems,
                           packings,configs,bindings}) its =  
    Canvas{widId=widId,scrolltype=scrolltype,citems=its,
           packings=packings,configs=configs,bindings=bindings}
  | updCanvasItems _                        _   = 
    raise WIDGET "CItem.updCanvasItems applied to non-Canvas Widget"

fun updCanvasPack (Canvas{widId,scrolltype,citems,
                          packings,configs,bindings}) p = 
    Canvas{widId=widId,scrolltype=scrolltype,citems=citems,
           packings=p,configs=configs,bindings=bindings}
  | updCanvasPack _                        _   = 
    raise WIDGET "CItem.updCanvasPack applied to non-Canvas Widget"

fun updCanvasConfigure (Canvas{widId,scrolltype,citems,
                               packings,configs,bindings}) c = 
    Canvas{widId=widId,scrolltype=scrolltype,citems=citems,
           packings=packings,configs=c,bindings=bindings}
  | updCanvasConfigure _                        _   = 
    raise WIDGET "CItem.updCanvasConfigure applied to non-Canvas Widget"

fun updCanvasBinding (Canvas{widId,scrolltype,citems,
                             packings,configs,bindings}) b = 
    Canvas{widId=widId,scrolltype=scrolltype,citems=citems,
           packings=packings,configs=configs,bindings=b}
  | updCanvasBinding _                        _   = 
    raise WIDGET "CItem.updCanvasBinding applied to non-Canvas Widget"


fun selItemType (CRectangle cr)  = CTRectangle
  | selItemType (COval co)       = CTOval
  | selItemType (CLine cl)       = CTLine
  | selItemType (CPoly cp)       = CTPoly
  | selItemType (CText ct)       = CTText
  | selItemType (CIcon ci)       = CTIcon
  | selItemType (CWidget cw)     = CTWidget
  | selItemType (CTag ct)        = CTTag

fun selItemId (CRectangle{citemId,...})  = citemId
  | selItemId (COval{citemId,...})       = citemId
  | selItemId (CLine{citemId,...})       = citemId
  | selItemId (CPoly{citemId,...})       = citemId
  | selItemId (CText{citemId,...})       = citemId
  | selItemId (CIcon{citemId,...})       = citemId
  | selItemId (CWidget{citemId,...})     = citemId
  | selItemId (CTag{citemId,...})        = citemId
(*
  | selItemId _                         =
    raise CITEM ("CItem.selItemId not yet fully implemented")
*)

fun selItemConfigure (CRectangle{configs,...}) = configs
  | selItemConfigure (COval{configs,...})      = configs
  | selItemConfigure (CLine{configs,...})      = configs
  | selItemConfigure (CPoly{configs,...})      = configs
  | selItemConfigure (CText{configs,...})      = configs
  | selItemConfigure (CIcon{configs,...})      = configs
  | selItemConfigure (CWidget{configs,...})    = configs
  | selItemConfigure (CTag _)                  =
    raise CITEM ("CItem.selItemConfigure: CTag has no Configure")
(*
  | selItemConfigure _                       =
    raise CITEM ("CItem.selItemConfigure not yet fully implemented")
*)

fun selItemBinding (CRectangle{bindings,...})  = bindings
  | selItemBinding (COval{bindings,...})       = bindings
  | selItemBinding (CLine{bindings,...})       = bindings
  | selItemBinding (CPoly{bindings,...})       = bindings
  | selItemBinding (CText{bindings,...})       = bindings
  | selItemBinding (CIcon{bindings,...})       = bindings
  | selItemBinding (CWidget{bindings,...})     = bindings
  | selItemBinding (CTag _)                    =
    raise CITEM ("CItem.selItemBinding: CTag has no Binding")
(*
  | selItemBinding _                       =
    raise CITEM ("CItem.selItemBinding not yet fully implemented")
*)

fun selItemCoords (CRectangle{coord1,coord2,...}) = [coord1, coord2]
  | selItemCoords (COval{coord1,coord2,...})      = [coord1, coord2]
  | selItemCoords (CLine{coords,bindings,...})         = coords
  | selItemCoords (CPoly{coords,bindings,...})         = coords
  | selItemCoords (CText{coord,bindings,...})        = [coord]
  | selItemCoords (CIcon{coord,bindings,...})        = [coord]
  | selItemCoords (CWidget{coord,...})  = [coord]
  | selItemCoords (CTag _)                  =
    raise CITEM ("CItem.selItemCoords: CTag has no Coords")
(*
  | selItemCoords _                         =
    raise CITEM ("CItem.selItemCoords not yet fully implemented")
*)

fun selItemWidgets (CWidget{widgets,...}) = selRawWids widgets
  | selItemWidgets _                      =
    raise CITEM ("CItem.selItemWidgets applied to non CWidget")

fun selItemItems (CTag{citemIds,...}) = citemIds
  | selItemItems _              =
    raise CITEM ("CItem.selItemItems applied to non CTag")

fun selItemIcon (CIcon{iconkind,...}) = iconkind
  | selItemIcon _                   =
    raise CITEM ("CItem.selItemIcon applied to non CIcon")


fun updItemConfigure (CRectangle{citemId,coord1,coord2,bindings,...}) cf = 
        CRectangle {citemId=citemId,coord1=coord1,coord2=coord2,
                    configs=cf,bindings=bindings}
  | updItemConfigure (COval{citemId,coord1,coord2,bindings,...})    cf = 
        COval{citemId=citemId,coord1=coord1,coord2=coord2,
              configs=cf,bindings=bindings}
  | updItemConfigure (CLine{citemId,coords,bindings,...})        cf = 
        CLine{citemId=citemId,coords=coords,configs=cf,bindings=bindings}
  | updItemConfigure (CPoly{citemId,coords,bindings,...})        cf = 
        CPoly{citemId=citemId,coords=coords,configs=cf,bindings=bindings}
  | updItemConfigure (CText{citemId,coord,bindings,...})         cf =
        CText{citemId=citemId,coord=coord,configs=cf,bindings=bindings}
  | updItemConfigure (CIcon{citemId,coord,iconkind,bindings,...})cf = 
        CIcon{citemId=citemId,coord=coord,iconkind=iconkind,
              configs=cf,bindings=bindings}
  | updItemConfigure (CWidget{citemId, coord, widgets, configs,
			      bindings,...}) cf =
    CWidget{citemId = citemId, coord = coord, widgets = widgets, configs = cf,
	    bindings = bindings}
  | updItemConfigure (CTag _)                       cf =
    raise CITEM ("CItem.updItemConfigure: CTag has no Configure")
(*
  | updItemConfigure _                              _  =
    raise CITEM ("CItem.updItemConfigure not yet fully implemented")
*)

fun updItemBinding (CRectangle {citemId,coord1,coord2,configs,...}) b = 
                    CRectangle {citemId=citemId,coord1=coord1,
                                coord2=coord2,configs=configs,bindings=b}
  | updItemBinding (COval      {citemId,coord1,coord2,configs,...}) b = 
                    COval      {citemId=citemId,coord1=coord1,
                                coord2=coord2,configs=configs,bindings=b}
  | updItemBinding (CLine      {citemId,coords,configs,...})        b = 
                    CLine      {citemId=citemId,coords=coords,
                                configs=configs,bindings=b}
  | updItemBinding (CPoly      {citemId,coords,configs,...})        b = 
                    CPoly      {citemId=citemId,coords=coords,
                                configs=configs,bindings=b}
  | updItemBinding (CText      {citemId,coord,configs,...})         b =
                    CText      {citemId=citemId,coord=coord,
                                configs=configs,bindings=b}
  | updItemBinding (CIcon      {citemId,coord,iconkind,configs,...}) b = 
                    CIcon      {citemId=citemId,coord=coord,
                                iconkind=iconkind,configs=configs,bindings=b}
  | updItemBinding (CWidget    {citemId, coord, widgets, configs,...}) b =
    CWidget {citemId = citemId, coord = coord, widgets = widgets,
	     configs = configs, bindings = b}
  | updItemBinding (CTag _)                           b =
    raise CITEM ("CItem.updItemBinding: CTag has no Binding")
(*
  | updItemBinding _                               _ =
    raise CITEM ("CItem.updItemBinding not yet fully implemented")
*)

fun updItemCoords (CRectangle{citemId,configs,bindings,...})    c = 
                   CRectangle{citemId=citemId,coord1=(hd c),coord2=(hd (tl c)),
                              configs=configs, bindings=bindings}
  | updItemCoords (COval{citemId,configs,bindings,...})         c = 
                   COval{citemId=citemId, coord1=(hd c), coord2=(hd (tl c)), 
                         configs=configs, bindings=bindings}
  | updItemCoords (CLine{citemId,configs,bindings,...})         c = 
                   CLine{citemId=citemId,coords=c,
                         configs=configs,bindings=bindings}
  | updItemCoords (CPoly{citemId,configs,bindings,...})         c = 
                   CPoly{citemId=citemId,coords=c,
                         configs=configs,bindings=bindings}
  | updItemCoords (CText{citemId,configs,bindings,...})         c =
                   CText{citemId=citemId,coord=hd c,
                         configs=configs,bindings=bindings}
  | updItemCoords (CIcon{citemId,iconkind,configs,bindings,...})c = 
                   CIcon{citemId=citemId, coord=hd c, iconkind=iconkind, 
                         configs=configs, bindings=bindings}
  | updItemCoords (CWidget{citemId, widgets, configs, bindings,...}) c =
    CWidget{citemId = citemId, coord=(hd c), widgets = widgets,
	    configs = configs, bindings = bindings}
  | updItemCoords (CTag _)                       c = 
    raise CITEM ("CItem.updItemCoords: CTag has no Coords")
(*
  | updItemCoords _ _ =
    raise CITEM ("CItem.updItemCoords not yet fully implemented")
*)

fun updItemWidgets (CWidget {citemId, coord, configs, bindings,
			     widgets}) newwids =
    let
	val wids = case widgets of Pack _ => Pack newwids
                                 | Grid _ => Grid newwids
    in
	CWidget {citemId = citemId, coord = coord, widgets = wids,
		 configs = configs, bindings = bindings}
    end
  | updItemWidgets _ _                                   =
       raise CITEM ("CItem.updItemWidgets applied to non CWidget")

fun updItemItems (CTag{citemId,...}) cids = 
            CTag{citemId=citemId, citemIds=cids} 
  | updItemItems _             cids =
            raise CITEM ("CItem.updItemItems applied to non CTag")

fun updItemIcon (CIcon{citemId,coord,configs,bindings,...}) ic = 
        CIcon{citemId=citemId,coord=coord,iconkind=ic,
              configs=configs,bindings=bindings}
  | updItemIcon _                     ic =
    raise CITEM ("CItem.updItemIcon applied to non CIcon")



(* ### muss noch implementiert werden *)
fun check (_:CItem) = true


fun get wid cid =
    let
	val cits = selCanvasItems wid
	val item = ListUtil.getx
	               (fn it => ((selItemId it) = cid)) cits 
	                (CITEM ("CItem.get: " ^ cid ^ " not found"))
    in
	item
    end

fun getBindingByName wid cid name =
    let
	val item = get wid cid
	val bis  = selItemBinding item
	val bi   = Bind.getActionByName name bis
    in
	bi
    end

fun upd widg cid ncit =
    let
	val cits           = selCanvasItems widg
	val cit            = ListUtil.getx 
	                         (fn cit => ((selItemId cit) = cid))
	                         cits 
				 (CITEM ("item: " ^ cid ^ " not found"))
	val ncits          = ListUtil.updateVal (fn cit => ((selItemId cit) = cid))
	                                         ncit
	                                         cits
	val nwidg          = updCanvasItems widg ncits
    in
	nwidg
    end


fun getCanvasWidgets (Canvas{widId,scrolltype,citems,packings,
                             configs,bindings}) =
    let
	val widits = List.filter (fn cit => (selItemType cit = CTWidget)) citems
	val wids   = map selItemWidgets widits
	val wids'  = List.concat wids
    in
	wids'
    end
  | getCanvasWidgets _             =
    raise WIDGET "CItem.getCanvasWidgets applied to non-Canvas Widget"


fun getCanvasCItemWidgetAssList (Canvas{widId,scrolltype,
                                        citems,packings,configs,bindings}) =
    let
  	val widits = List.filter (fn cit => (selItemType cit = CTWidget)) citems
 	val wids   = map selItemWidgets widits
    in
 	ListPair.zip(widits,wids)
    end
  | getCanvasCItemWidgetAssList _ =
    raise WIDGET "CItem.getCanvasCItemWidgetAssList applied to non-Canvas Widget"


fun addCanvasWidget af (w as (Canvas _)) widg wp =
    let
	val _ = Debug.print 3 ("addCanvasWidget "^(selWidgetWidId w)^" "^(selWidgetWidId widg)^" "^wp)
	val (wId,nwp)     = Paths.fstWidPath wp      (* strip ".cnv" *)
	val (wId',nwp')   = Paths.fstWidPath nwp      (* strip ".cfr" *)
    in
	if ( nwp' = "" ) then
	    raise CITEM "CItem.addCanvasWidgets called for CWidget-Toplevel"
	else
	    let
		val (wId'',nwp'') = Paths.fstWidPath nwp'
		val citwidass     = getCanvasCItemWidgetAssList w

		val (cit,swidgs)  = ListUtil.getx
		                      (fn (c,(ws:Widget list)) => 
				       foldr
				         (fn (w,t) => ((selWidgetWidId w) = wId'') orelse t)
					 false ws)
				      citwidass 
				      (CITEM ("CItem.addCanvasWidget: subwidget " ^ wId'' ^ " not found" ))
		val _ = Debug.print 3 ("addCanvasWidget(ass): "^(selItemId cit)^" ["^
			       (concat (map (selWidgetWidId) swidgs))^"]")

		val nswidgs       = af swidgs widg nwp'
		val ncit          = updItemWidgets cit nswidgs
		val nwidg         = upd w (selItemId ncit) ncit
	    in
		nwidg
	    end
    end
  | addCanvasWidget _ _ _ _                  =
    raise WIDGET "CItem.addCanvasWidgets applied to non-Canvas Widget"

fun deleteCanvasWidget df (w as (Canvas _)) wid wp =
    let
	val _ = Debug.print 3 ("deleteCanvasWidget "^(selWidgetWidId w)^" "^wp)
	val (wId,nwp)     = Paths.fstWidPath wp         (* strip ".cfr" *)
	val (wId',nwp')   = Paths.fstWidPath nwp
	val citwidass     = getCanvasCItemWidgetAssList w

	val (cit,swidgs)  = ListUtil.getx 
	                       (fn (c,(ws:Widget list)) => 
				  foldr
				  (fn (w,t) => ((selWidgetWidId w) = wId') orelse t)
				  false ws)
			       citwidass 
			       (CITEM ("CItem.deleteCanvasWidget: subwidget " ^ wId' ^ " not found"))

	val nswidgs       = df swidgs wId' nwp'
	val ncit          = updItemWidgets cit nswidgs
	val nwidg         = upd w (selItemId ncit) ncit
    in
	nwidg
    end
  | deleteCanvasWidget _ _ _ _                  =
    raise WIDGET "CItem.deleteCanvasWidgets applied to non-Canvas Widget"

fun updCanvasWidget uf (w as (Canvas _)) wid wp neww =
    let
	val _ = Debug.print 3 ("updCanvasWidget "^(selWidgetWidId w)^" "^wp)
	val (wId,nwp)     = Paths.fstWidPath wp         (* strip ".cfr" *)
	val (wId',nwp')   = Paths.fstWidPath nwp
	val citwidass     = getCanvasCItemWidgetAssList w

	val (cit,swidgs)  = ListUtil.getx
	                       (fn (c,(ws:Widget list)) => 
				  foldr
				  (fn (w,t) => ((selWidgetWidId w) = wId') orelse t)
				  false ws)
			       citwidass 
			       (CITEM ("CItem.updCanvasWidget did not find Subwidget " ^ wId'))

	val nswidgs       = uf swidgs wId' nwp' neww
	val ncit          = updItemWidgets cit nswidgs
	val nwidg         = upd w (selItemId ncit) ncit
    in
	nwidg
    end
  | updCanvasWidget _ _ _ _ _ =
    raise WIDGET "CItem.updCanvasWidgets applied to non-Canvas Widget"

fun printCanvasWidget CanvasId ConfigList =
      let
        val ctp  = "." ^ CanvasId ^ ".cnv";
      in
        (Com.putTclCmd (ctp ^ " postscript " ^
                        (Config.showAllPrintConf ConfigList)))
      end

fun pack pf tp (ip as (win, pt))
         (COval {citemId, coord1, coord2, configs, bindings}) =
    let
	val coords = Coord.show [coord1, coord2]
	val conf   = Config.pack ip configs
    in
	(tp ^ " create oval " ^ coords ^ " " ^ conf ^ " -tags " ^
	 citemId ^ "\n" ^
	 concat (Bind.packCanvas tp ip citemId bindings))
    end
  | pack pf tp (ip as (win, pt))
         (CRectangle {citemId, coord1, coord2, configs, bindings}) =
    let
	val coords = Coord.show [coord1, coord2]
	val conf   = Config.pack ip configs
    in
	(tp ^ " create rectangle " ^ coords ^ " " ^ conf ^  " -tags " ^
	 citemId ^ "\n" ^
	 concat (Bind.packCanvas tp ip citemId bindings))
    end
  | pack pf tp (ip as (win, pt)) (CLine {citemId, coords, configs, bindings}) =
    let
	val coords = Coord.show coords
	val conf   = Config.pack ip configs
    in
	(tp ^ " create line " ^ coords ^ " " ^ conf ^  " -tags " ^
	 citemId ^ "\n" ^
	 concat (Bind.packCanvas tp ip citemId bindings))
    end

  | pack pf tp (ip as (win, pt)) (CIcon {citemId, coord, iconkind, configs,
					 bindings}) = 
    let
	val coords = Coord.show [coord]
	val conf   = Config.pack ip configs
	val icon   = Config.showIconKind iconkind
	val ictype = 
	    case iconkind of 
		NoIcon       => "bitmap"
	      | TkBitmap _   => "bitmap"
	      | FileBitmap _ => "bitmap"
(*	      | FilePixmap _ => "bitmap"   *)
	      | FileImage _  => "image"
    in
        (tp ^ " create " ^ ictype ^" " ^ coords ^ " " ^ 
	 icon ^ " " ^ conf ^  " -tags " ^ citemId ^ "\n" ^
	 concat (Bind.packCanvas tp ip citemId bindings))
    end
  | pack pf tp (ip as (win, pt))
         (CWidget {citemId, coord, widgets, configs, bindings}) =
    let
	val widId  = citemId
	val coords = Coord.show [coord]
	val conf   = Config.pack ip configs
	val frw    = Frame {widId = widId, widgets = widgets,
			    packings = [], configs = [], bindings = []}
	val frtp   = tp ^ "." ^ widId
    in
	(pf false tp ip NONE frw ^
	 tp ^ " create window " ^ coords ^ " " ^ conf ^
	 " -window " ^ frtp ^  " -tags " ^ citemId ^ "\n" ^
	 concat (Bind.packCanvas tp ip citemId bindings))
    end
  | pack pf tp (ip as (win, pt)) (CTag _) = ""
(* Added by E.L.Gunter 14 July 1998 *)
  | pack pf tp (ip as (win, pt)) (CPoly {citemId, coords, configs, bindings}) =
    let
	val coords = Coord.show coords
	val conf   = Config.pack ip configs
    in
	(tp ^ " create polygon " ^ coords ^ " " ^ conf ^ " -tags " ^
	 citemId ^ "\n" ^
	 concat (Bind.packCanvas tp ip citemId bindings))
    end
  | pack pf tp (ip as (win, pt)) (CText {citemId, coord, configs, bindings}) =
    let
	val coords = Coord.show [coord]
	val conf   = Config.pack ip configs
    in
	(tp ^ " create text " ^ coords ^ " " ^ conf ^ " -tags " ^
	 citemId ^ "\n" ^
	 concat (Bind.packCanvas tp ip citemId bindings))
    end
(*  | pack _ _ _ _ =
    raise CITEM ("CItem.pack not yet fully implemented")
*)

fun add pf widg cit =
    let
	val ip as (win,pt) = Paths.getIntPathGUI (selWidgetWidId widg)
	val tp             = Paths.getTclPathGUI ip
	val nip            = (win,pt ^ ".cnv")
	val ntp            = tp ^ ".cnv"
	val cits           = selCanvasItems widg
	val ncits          = cits@[cit]
	val nwidg          = updCanvasItems widg ncits
    in
	(Com.putTclCmd (pack pf ntp nip cit);
	 nwidg)
    end

fun delete dwf widg cid =
    let
	fun delete' dwf widg (cit as (CWidget{citemId,widgets,...})) =
	    let
		val ip as (win,pt) = Paths.getIntPathGUI (selWidgetWidId widg)
		val tp             = Paths.getTclPathGUI ip
		val nip            = (win,pt ^ ".cnv")
		val ntp            = tp ^ ".cnv"
		val cits           = selCanvasItems widg
		val ncits          = List.filter (fn cit => not ((selItemId cit) = citemId)) cits
		val nwidg          = updCanvasItems widg ncits
	    in
		(app (dwf o selWidgetWidId) (selRawWids widgets);
		 Com.putTclCmd ("destroy " ^ ntp ^ "." ^ citemId);
		 Com.putTclCmd (ntp ^ " delete " ^ citemId);
		 nwidg)
	    end
	  | delete' dwf widg cit =
	    let
		val ip as (win,pt) = Paths.getIntPathGUI (selWidgetWidId widg)
		val tp             = Paths.getTclPathGUI ip
		val nip            = (win,pt ^ ".cnv")
		val ntp            = tp ^ ".cnv"
		val cits           = selCanvasItems widg
		val ncits          = List.filter (fn cit => not ((selItemId cit) = cid)) cits
		val nwidg          = updCanvasItems widg ncits
	    in
		(Com.putTclCmd (ntp ^ " delete " ^ cid);
		 nwidg)
	    end
	val cit = get widg cid
    in
	delete' dwf widg cit
    end


fun addItemConfigure widg cid cf =
    let
	val ip as (win,pt) = Paths.getIntPathGUI (selWidgetWidId widg)
	val tp             = Paths.getTclPathGUI ip
	val nip            = (win,pt ^ ".cnv")
	val ntp            = tp ^ ".cnv"
	val cits           = selCanvasItems widg
	val cit            = ListUtil.getx (fn cit => ((selItemId cit) = cid))
	                                    cits 
					    (CITEM ("item: " ^ cid ^ " not found"))
	val conf           = selItemConfigure cit
	val nconf          = Config.add conf cf
	val ncit           = updItemConfigure cit nconf
	val ncits          = ListUtil.updateVal (fn cit => ((selItemId cit) = cid))
	                                         ncit
	                                         cits
	val nwidg          = updCanvasItems widg ncits
    in
	(Com.putTclCmd (ntp ^ " itemconfigure " ^ cid ^ " " ^
			Config.pack nip cf);
	 nwidg)
    end

 fun addItemBinding widg cid bi =
    let
	val ip as (win,pt) = Paths.getIntPathGUI (selWidgetWidId widg)
	val tp             = Paths.getTclPathGUI ip
	val nip            = (win,pt ^ ".cnv")
	val ntp            = tp ^ ".cnv"
	val cits           = selCanvasItems widg
	val cit            = ListUtil.getx (fn cit => ((selItemId cit) = cid))
	                                    cits 
					    (CITEM ("item: " ^ cid ^ " not found"))
	val bind           = selItemBinding cit
	val nbind          = Bind.add bind bi
	val ncit           = updItemBinding cit nbind
	val ncits          = ListUtil.updateVal (fn cit => ((selItemId cit) = cid))
	                                         ncit
	                                         cits
	val nwidg          = updCanvasItems widg ncits
    in
	(Com.putTclCmd (concat(Bind.packCanvas ntp nip cid bi));
	 nwidg)
    end


fun getCoords wid cid =
    let
	val cit = get wid cid
    in  case cit of
	CTag {citemIds=[],...}   => raise CITEM ("CItem.getCoords: CTag(_, [])")
      | CTag {citemIds=x::_,...} => getCoords wid x
      | _ => let 
		 val ip   = Paths.getIntPathGUI (selWidgetWidId wid)
		 val tp   = Paths.getTclPathGUI ip
		 val cid' = selItemId cit
		 val cos  = Com.readTclVal (tp^ ".cnv coords "^cid')
	     in
		 Coord.read cos
	     end
    end


fun setCoords wid cid cos =
    let
	fun setCoords' wid (CTag _) cos =
	    raise CITEM ("CItem.setCoords is not to be used for CTag")
	  | setCoords' wid cit cos =
	    let 
		val ip   = Paths.getIntPathGUI (selWidgetWidId wid)
		val tp   = Paths.getTclPathGUI ip
		val cid' = selItemId cit
	    in
		Com.putTclCmd (tp ^ ".cnv coords " ^ cid' ^ " " ^ (Coord.show cos))
	    end
	val cit = get wid cid
    in
	setCoords' wid cit cos
    end


fun getIconWidth (NoIcon) =
       0
  | getIconWidth (TkBitmap _) =
       raise CITEM ("CItem.getIconWidth: don't know how to get width of TkBitmaps")
  | getIconWidth (FileBitmap _) =
       raise CITEM ("CItem.getIconWidth: don't know how to get width of FileBitmaps")
  | getIconWidth (FileImage(f,imid)) =
       StringUtil.toInt (Com.readTclVal ("image width " ^ imid))

fun getWidth wid cid =
    let
	fun min xs = foldl Int.min (hd xs) xs
	fun max xs = foldl Int.max (hd xs) xs

	fun getWidth' wid (CRectangle _) ((x1,_)::(x2,_)::nil) =
	    x2-x1
	  | getWidth' wid (COval _) ((x1,_)::(x2,_)::nil) =
	    x2-x1
	  | getWidth' wid (CLine _) (cos as (co::cos'))  = 
	    let
		val xs = map fst cos
		val ma = max xs
		val mi = min xs
	    in
		ma-mi
	    end
	  | getWidth' wid (CPoly _) (cos as (co::cos')) = 
	    let
		val xs = map fst cos
		val ma = max xs
		val mi = min xs
	    in
		ma-mi
	    end
          | getWidth' wid (CText _) _ =
            raise CITEM ("CItem.getWidth not yet implemented for CText")
	  | getWidth' wid (CIcon{iconkind,...}) _ =
	    getIconWidth iconkind
	  | getWidth' wid (CWidget _) _ =
	    raise CITEM ("CItem.getWidth not yet implemented for CWidget")
	  | getWidth' wid (CTag _) _ =
	    raise CITEM ("CItem.getWidth not yet implemented for CTag")

	val cit = get wid cid
	val cos = getCoords wid cid
    in
	getWidth' wid cit cos
    end


fun getIconHeight (NoIcon) =
       0
  | getIconHeight (TkBitmap _) =
       raise CITEM ("CItem.getIconHeight: don't know how to get width of TkBitmaps")
  | getIconHeight (FileBitmap _) =
       raise CITEM ("CItem.getIconHeight: don't know how to get width of FileBitmaps")
  | getIconHeight (FileImage(f,imid)) =
       StringUtil.toInt (Com.readTclVal ("image height " ^ imid))

fun getHeight wid cid =
    let
	fun min xs = foldl Int.min (hd xs) xs
	fun max xs = foldl Int.max (hd xs) xs

	fun getHeight' wid (CRectangle _) ((_,y1)::(_,y2)::nil) =
	    y2-y1
	  | getHeight' wid (COval _) ((_,y1)::(_,y2)::nil) =
	    y2-y1
	  | getHeight' wid (CLine _) (cos as (co::cos'))  = 
	    let
		val ys = map BasicUtil.snd cos
		val ma = max ys
		val mi = min ys
	    in
		ma-mi
	    end
	  | getHeight' wid (CPoly _) (cos as (co::cos')) = 
	    let
		val ys = map BasicUtil.snd cos
		val ma = max ys
		val mi = min ys
	    in
		ma-mi
	    end
          | getHeight' wid (CText _) _ =
            raise CITEM ("CItem.getHeight not yet implemented for CText")
	  | getHeight' wid (CIcon{iconkind,...}) _ =
	    getIconHeight iconkind
	  | getHeight' wid (CWidget _) _ =
	    raise CITEM ("CItem.getHeight not yet implemented for CWidget")
	  | getHeight' wid (CTag _) _ =
	    raise CITEM ("CItem.getHeight not yet implemented for CTag")

	val cit = get wid cid
	val cos = getCoords wid cid
    in
	getHeight' wid cit cos
    end

fun move wid cid co =
    let
	fun move' wid (CTag{citemId,citemIds}) co =
	    app (fn cid => move wid cid co) citemIds
	  | move' wid cit (co as (x,y)) =
	    let 
		val ip   = Paths.getIntPathGUI (selWidgetWidId wid)
		val tp   = Paths.getTclPathGUI ip
		val cid' = selItemId cit
	    in
		Com.putTclCmd (tp ^ ".cnv move " ^ cid' ^ " " ^ (Coord.show [co]))
	    end
	val cit = get wid cid
    in
	move' wid cit co
    end

(* ************************************************************************** *)
(* 									      *)
(* Ananymous CItemId Manager						      *)
(* Purpose: Creates anonymous names for Canvas items, starting                *)
(* with "anocid" and a unique number					      *)
(* 									      *)
(* ************************************************************************** *)

val ANOCID_NR = ref(0);
fun newId() = (inc(ANOCID_NR);"anocid"^Int.toString(!ANOCID_NR));

val ANOFRID_NR = ref(0);
fun newFrId() = (inc(ANOFRID_NR);"cfr"^Int.toString(!ANOFRID_NR));

end;

end;

