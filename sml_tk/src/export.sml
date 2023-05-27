(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/export.sml,v $
 
   sml_tk Export Signature.  ``All you ever wanted to know about sml_tk''

   Part II: Functions
  
   $Date: 2001/03/30 13:39:11 $
   $Revision: 3.0 $

   Author: bu/cxl (Last modification by $Author: 2cxl $)

   (C) 1996, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

signature SML_TK =
sig
    include TK_TYPES
(* 1. Identifiers *)
    val newWinId             : unit -> WinId
    val newImageId           : unit -> ImageId
    val newWidgetId          : unit -> WidId
    val newCItemId           : unit -> CItemId
    val newCItemFrameId      : unit -> WidId
    val newAnnotationId      : unit -> AnnId

    (* to generate more meaningful names *)
    val mkWinId     : string -> WinId
    val mkImageId   : string -> ImageId
    val mkCItemId   : string -> CItemId
    val mkWidgetId  : string -> WidId

    val mkTitle : string -> Title
    val mkSimpleAction : (unit -> unit) -> SimpleAction
    val noAction       : SimpleAction 
    val mkAction : (TkEvent -> unit) -> Action	  	
    val mkWid : string -> WidId  

    (* reconvert to string - needed by TextIO to handle output *)
    val mkWinString : WinId -> string
    val mkWidgetString : WidId -> string
    val mkCItemString : CItemId -> string
    val mkAnnIdString : AnnId -> string

    (* conversion between different id's *)
    val WidIdToCItemId   : WidId -> CItemId
    val CItemIdToWidId   : CItemId -> WidId

    (* to generate dependent identifiers *)
    val subWinId    : WinId* string-> WinId
    val subWidId    : WidId* string-> WidId
    val subCItemId  : CItemId* string-> CItemId

(* 2. Control *)

    val startTcl    : Window list -> unit
    val startTclExn : Window list -> string

    (* same as closing the main window with closeWindow *)
    val exitTcl : unit -> unit

    val reset : unit -> unit

    val setFontBasesize : int -> unit


(* 3. Windows *)

    val mkWindow : {winId   : WinId,
		    config  : WinConfigure list, 
		    widgets : Widgets, 
                    bindings: Binding list,
		    init    : SimpleAction} -> Window

    val getWindow     : WinId -> Window
    val getAllWindows : unit  -> Window list

    val openWindow  : Window -> unit
    val closeWindow : WinId -> unit
    val changeTitle : WinId -> Title -> unit
    val occursWin   : WinId -> bool


(* 4. Widgets *)

(* 4.1 General Operations *)

    val occursWidget : WidId-> bool
    val getWidget    : WidId -> Widget

(*      -- the second argument is the Frame into which the widget is inserted *)
    val addWidget : WinId -> WidId -> Widget -> unit 
    val delWidget : WidId -> unit

    val addBind : WidId -> Binding list   -> unit
    val addConf : WidId -> Configure list -> unit

    val setBind : WidId -> Binding list   -> unit 
    val setConf : WidId -> Configure list -> unit


    val getTextWidWidgets : Widget -> Widget list
    val getCanvasWidgets  : Widget -> Widget list

(* 4.2. Configure, Binding, ... for Widgets *)

    (* are all derived from selectWidget *)
    val getConf     : WidId -> Configure list
    val getRelief   : WidId -> RelKind
    val getCommand  : WidId -> SimpleAction
    val getBindings : WidId -> Binding list
    val getWidth    : WidId -> int
    val getHeight   : WidId -> int
    val getMCommand : WidId -> int list -> SimpleAction


(* 4.3 Operations for Widget containing text (TextWid, Listbox, Entry) *)

(* 4.3.2 Manipulation of Text *)

    (* Low-level access: no annotation, fails for read-only text widgets.
     * On the other hand, works for list boxes etc. as well, but for
     * text widgets, better use ..annoText below *)   
    val insertText    : WidId -> string -> Mark -> unit
    val insertTextEnd : WidId -> string -> unit

    val clearText     : WidId -> unit
    val deleteText    : WidId -> Mark * Mark -> unit

    (* not for Entry *)
    val readText    : WidId -> Mark * Mark -> string
    val readTextAll : WidId -> string

    val readTextWidState   : WidId -> bool
    val setTextWidReadOnly : WidId -> bool -> unit

    (* Recommended functions to manipulate text widgets. Handles
     * read-only text widgets correctly *)
    val clearAnnoText      : WidId -> unit
    val replaceAnnoText    : WidId -> AnnoText-> unit
    val deleteAnnoText     : WidId -> Mark* Mark-> unit
    val insertAnnoText     : WidId -> AnnoText-> Mark-> unit
    val insertAnnoTextEnd  : WidId -> AnnoText-> unit (* use discouraged-- 
			  			       * very inefficient! *)


(* 4.3.3 Selection of postions and ranges *)

    val readCursor   : WidId -> Mark
    val readSelRange : WidId -> (Mark * Mark) list

(* 4.4 Annotated texts" *)
    val mkAT      : string -> AnnoText
    val mtAT      : AnnoText 

    (* infix 6 ++  *)
    val ++        : AnnoText * AnnoText -> AnnoText

    val nlAT         : AnnoText -> AnnoText 
    val concatATWith : string -> AnnoText list -> AnnoText 

(* 4.5 Annotation *)
    val getAnnotation     : WidId -> AnnId -> Annotation

    val addAnnotation     : WidId -> Annotation -> unit
    val delAnnotation     : WidId -> AnnId    -> unit

    val getAnnotationBind : WidId -> AnnId -> Binding list
    val getAnnotationConf : WidId -> AnnId -> Configure list

    val addAnnotationBind : WidId -> AnnId -> Binding list -> unit
    val addAnnotationConf : WidId -> AnnId -> Configure list -> unit

    val readAnnotationMarks : WidId -> AnnId -> (Mark * Mark) list

    val readSelection     : WidId -> (Mark * Mark) list

(* 4.6 Canvases incl. Canvas Items *)
    val getCItem : WidId -> CItemId -> CItem

    val addCItem : WidId -> CItem   -> unit
    val delCItem : WidId -> CItemId -> unit

    val getCItemBind : WidId -> CItemId -> Binding list
    val getCItemConf : WidId -> CItemId -> Configure list

    val addCItemBind : WidId -> CItemId -> Binding list -> unit
    val addCItemConf : WidId -> CItemId -> Configure list -> unit

    val CanvasToPostscript : TkTypes.CItemId -> TkTypes.Configure list -> unit

    val readCItemCoords  : WidId -> CItemId -> Coord list
    val readCItemHeight  : WidId -> CItemId -> int
    val readCItemWidth   : WidId -> CItemId -> int

    val moveCItem       : WidId -> CItemId -> Coord -> unit
    val setCItemCoords  : WidId -> CItemId -> Coord list -> unit

(* 4.7 Menues *)
    val popUpMenu          : WidId -> int Option.option -> Coord -> unit

    val createAndPopUpMenu : Widget-> int Option.option -> Coord -> unit 
      (********** still buggy ??? *********** siehe Popup_ex *)


(* 4.8 Buttons and Tcl Vaues *)

    val setVarValue  : string -> string -> unit
    val readVarValue : string -> string
    val setScale     : WidId -> real -> unit

(* 4.9 Coord *)

    val mkCoord    : int * int -> Coord
    val addCoord   : Coord -> Coord -> Coord
    val subCoord   : Coord-> Coord -> Coord
    val smultCoord : Coord-> int-> Coord
    val showCoord  : Coord list -> string
    val convCoord  : string -> Coord list


    type Rect = Coord* Coord 
        
    val inside   : Coord -> Rect -> bool
    val intersect :  Rect-> Rect-> bool
    val moveRect : Rect -> Coord -> Rect
    val showRect : Rect-> string

(* 4.10. Checks *)

(*  val check       : Window -> bool *)
(*  val checkMItem  : MItem -> bool  *)
    val checkWidId  : WidId -> bool
(*  val checkWidget : Widget -> bool *)

(*  val checkWin : Window -> bool    *)
    val checkWinId : WinId -> bool
    val checkWinTitle : Title -> bool

(* 4.11. Focus and Grabs *)

    val focus   : WinId -> unit
    val deFocus : WinId -> unit
    val grab    : WinId -> unit
    val deGrab  : WinId -> unit

(* 4.12. Selection *)

    val readSelWindow : unit -> (WinId * WidId) Option.option


(* 4.13. Interrupt handling *)

    type intr_listener
	
    val registerIntrListener   : (unit-> unit)-> intr_listener
    val deregisterIntrListener : intr_listener-> unit


(* 4.14. GUI state *)

    val init       : unit -> unit
    val initFonts  : unit -> unit

    (* get/update the library path (SMLTK_LIB) *)
    val getLibPath : unit -> string
    val updLibPath : string -> unit

    (* get/update the wish path (SMLTK_TCL) *)
    val getTclPath : unit -> string
    val updTclPath : string-> unit
	
    (* get/update the logfile path (SMLTK_LOGFILE *)
    val getLogfilePath : unit-> string
    val updLogfilePath : string-> unit

(* 4.15. Miscellenea *)

    val showMarkL : (Mark * Mark) list -> string

    (* Produce dumped image *)
	
    val xSmlTk : {banner : string, imagefile : string} -> unit

   (* Environment variable settings (can be overriden from the cmd line) *)

    val getEnvSetting : string-> string Option.option

	
(* 7. Debugging *)

    structure Debug : DEBUG

(* These are needed for debugging as well, if you want to print an id *)
    val CItemIdToString  : CItemId -> string
    val WidIdToString    : WidId -> string
    val WinIdToString    : WinId -> string
    val AnnIdToString    : AnnId -> string
  
    val mkCursorName     : string -> CursorName
    val mkRect           : ((int* int)* (int* int)) -> Rect
 
end

structure SmlTk :> SML_TK =
    struct
	open BasicUtil
        open ComState
	open Com 
	open Coord 
	open CItem
	open CItemTree
	open Annotation
	open AnnotationTree
	open Paths
	open Config
	open TkEvent
	open AnnotatedText
	open WidgetTree
	open Window
	open Eventloop
	open WidgetOps
        open TkTypes 

	val getLibPath = ComState.getLibPath
	val updLibPath = ComState.updLibPath

	val getTclPath = ComState.getWishPath
	val updTclPath = ComState.updWishPath

	fun getLogfilePath () = Option.getOpt(ComState.getLogfilename(), "")
	fun updLogfilePath "" = ComState.updLogfilename  NONE
          | updLogfilePath p  = ComState.updLogfilename (SOME p)

	val occursWin     = occursWindowGUI;

	val changeTitle   = Window.changeTitle;
	val checkWin      = Window.check;
	val checkWinId    = Window.checkWinId;
	val checkWinTitle = Window.checkTitle;
	val openWindow    = Window.openW
	val closeWindow   = Window.close

	val getWindow     = GuiState.getWindowGUI
	val getAllWindows = GuiState.getWindowsGUI

        fun mkCoord (x,y) = (x,y)
	val addCoord  = Coord.add
	val subCoord  = Coord.sub
	val smultCoord = Coord.smult
	val showCoord = Coord.show
	val convCoord = Coord.read
	fun CoordToTuple (x,y) = (x,y);
	fun mkRect r = r;

	val showMark  = Mark.show
	val showMarkL = Mark.showL

	val occursWidget = Paths.occursWidgetGUI
	val delWidget   = WidgetTree.deleteWidget
	val addConf     = WidgetTree.configure
	val addBind     = WidgetTree.addBindings
	val setBind     = WidgetTree.newBindings
	val setConf     = WidgetTree.newconfigure

	val getWidget   = selectWidget
	val getConf     = select
	val getRelief   = selectRelief
	val getCommand  = selectCommand
	val getWidth    = selectWidth
	val getHeight   = selectHeight
	val getBindings = selectBindings
	val getMCommand = selectMCommand

	val addCItem = CItemTree.add
	val delCItem = CItemTree.delete

	val addCItemBind = CItemTree.addBinding
        val addCItemConf = CItemTree.addConfigure

	val getCItem  = CItemTree.get

	val getCItemBind = CItemTree.getBinding
	val getCItemConf = CItemTree.getConfigure
	val CanvasToPostscript = CItemTree.printCanvas
	    
	val moveCItem = CItemTree.move
	val setCItemCoords = CItemTree.setCoords

	val updCItem = CItemTree.upd

	val mkAT = mk
	val nlAT = nl
	val mtAT = mtAt
	val concatATWith = concatAtWith
	val ++ = +++

	val getAnnotation = AnnotationTree.get
	val updAnnotation = AnnotationTree.upd
	val addAnnotation = AnnotationTree.add
	val delAnnotation = AnnotationTree.delete

	val getAnnotationBind = AnnotationTree.getBinding
	val getAnnotationConf = AnnotationTree.getConfigure
	val addAnnotationBind = AnnotationTree.addBinding
	val addAnnotationConf = AnnotationTree.addConfigure

	val readAnnotationMarks = AnnotationTree.readMarks


	val readSelWindow = selectSelWindow
	val readVarValue  = selectVarValue
	val readText      = selectText
        val readTextAll   = selectTextAll

        val readCursor    = WidgetOps.selectCursor
        val readSelRange  = WidgetOps.selectSelRange
        val setScale      = WidgetOps.setScaleValue

	val readCItemCoords = CItemTree.getCoords 
        val readCItemHeight = CItemTree.getHeight
        val readCItemWidth  = CItemTree.getWidth

	val readIconHeight  = getIconHeight
	val readIconWidth   = getIconWidth

	val newCItemId           = CItem.newId
        val newCItemFrameId      = newFrId
	val newAnnotationId      = Annotation.newId
	val newAnnotationFrameId = Annotation.newFrId
        val newWinId             = newWidgetId  (* dodgy *)
	val newImageId           = newWidgetId  (* me too *)


	(* these also have to check their arguments for non-alphanumerical
	 * characters etc. *)
	fun mkWinId str     = str ^ newWinId()
	fun mkImageId str   = str ^ newImageId()
        fun mkFrameId str   = str ^ newFrId()
	fun mkCItemId str   = str ^ CItem.newId()
        fun mkWidgetId str  = str ^ newWidgetId()
	   
	fun mkWindow {winId, widgets, config, bindings, init} = 
	    (winId, config, widgets, bindings, init)

	fun mkTitle str = str
        fun mkSimpleAction f = f
	val noAction = fn _=> ()

	fun mkAction f = f

  	fun mkQuitAction f = f
  	fun mkCallBack f = f
	fun mkWid w = w       
        fun mkAppId str = str
        fun mkProgram p = p
        fun mkProtName p = p

        fun mkWinString w = w
        fun mkWidgetString w = w
        fun mkCItemString c = c
        fun mkAnnIdString a = a        

	fun subWinId(w, str)   = w ^ str
	fun subWidId(w, str)   = w ^ str
	fun subCItemId(c, str) = c ^ str 

	fun WidIdToCItemId c = c
	fun CItemIdToWidId c = c  

	fun init() = (resetTcl (); SysInit.initSmlTk ())
        val reset  = resetTcl
	    
	val initFonts  = Fonts.init o getLibPath

	fun setFontBasesize x= (#BaseSize(Fonts.Config) := x; initFonts())

	fun xSmlTk{imagefile, banner} 
	    = SysDep.exportML {init= SysInit.initSmlTk,
			       banner= banner, imagefile=imagefile}

	val getEnvSetting = SysInit.getEnvSetting

	structure Debug= Debug

	(* These con-/destructors and converters are needed 
           for the new version *)

	fun WidIdToString c=c
	fun WinIdToString c=c
	fun CItemIdToString c = c
	fun AnnIdToString c = c

	fun mkCursorName c = c

    end


