(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/tk_types.sml,v $
 
   sml_tk Export Signature.  ``All you ever wanted to know about sml_tk''
  
   Part I: Types, type constructors, selectors etc. 

   $Date: 2001/03/30 13:39:21 $
   $Revision: 3.0 $
   Author: cxl (Last modification by $Author: 2cxl $)

   (C) 1996, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

signature TK_TYPES =
  sig

    (*
     * Exceptions 
     *)
    exception CITEM of string
    exception WIDGET of string    
    exception TCL_ERROR of string
    exception CONFIG of string
    exception WINDOWS of string


    type AnnId 

    type Title
    type WidPath


    (* all these should be paths, as in OS.Path *)
    type BitmapName  = string 
(*  type PixmapFile *)
    type CursorName  = string
    type BitmapFile  = string 
    type ImageFile   = string
    type CursorFile  = string

    (* Identifiers for sml_tk's entities: windows, CItems, Images, Widgets *)
    eqtype WinId
    eqtype CItemId
    eqtype ImageId
    eqtype WidId

    type Coord = int* int

    datatype TkEvent =
      TkEvent of int                       (* %b  Button number     *)
	      *  string                    (* %s  state field       *)
              *  int                       (* %x  x field           *)
              *  int                       (* %y  y field           *)
              *  int                       (* %X  x_root field      *)
              *  int                       (* %Y  y_root field      *)       

    (* -- selectors *)
    val selButton   : TkEvent -> int
    val selState    : TkEvent -> string
    val selXPos     : TkEvent -> int
    val selXRootPos : TkEvent -> int
    val selYPos     : TkEvent -> int
    val selYRootPos : TkEvent -> int

    type SimpleAction = unit -> unit
    type ScaleAction  = real -> unit
    type Action       = TkEvent -> unit

    datatype Event =
        (* window events *)
        FocusIn
      | FocusOut
      | Configure
      | Map
      | Unmap
      | Visibility
      | Destroy
	(* Key press/release events *)
      | KeyPress   of string
      | KeyRelease of string
	(* Button press/release events, NONE means any old Button *)
      | ButtonPress   of int Option.option
      | ButtonRelease of int Option.option
	(* Cursor events *)
      | Enter  | Leave  | Motion      
	(* user-defined events, or explicitly given events *)
      | UserEv of string
	(* event modifiers  *)
      | Shift of Event  | Ctrl of Event | Lock of Event   | Any of Event 
      | Double of Event | Triple of Event
      | ModButton of int* Event
      | Alt of Event    | Meta of Event 
      | Mod3 of Event   | Mod4 of Event | Mod5 of Event 
	(* Not all combinations make sense, eg.
	 * modifiying a Button event with a different Button will cast
	 * doubt on either your sanity or understanding of these events *)


    datatype Binding = BindEv of Event * Action

    datatype ScrollRegion = Region of int * int * int * int

    datatype RelKind =  
	Flat | Groove | Raised | Ridge | Sunken

    datatype Color	= 
	NoColor | Black | White | Grey | Blue | Green | Red | Brown | Yellow
      | Purple  | Orange | Mix of {red : int, blue : int, green : int}

    datatype ArrowPos   = 
	NoneAP | FirstAP | LastAP | BothAP

    datatype CapstyleKind = 
	Butt | Projecting | Round
	
    datatype JoinstyleKind = 
	Bevel | Miter | RoundJoin

    datatype AnchorKind =
	North | NorthEast | 
	East  | SouthEast | 
	South | SouthWest | 
	West  | NorthWest |
	Center

    datatype IconKind =
        NoIcon
      |	TkBitmap    of BitmapName            (* -bitmap <tk bitmap>     *)
      | FileBitmap  of BitmapFile            (* -bitmap @<filename>     *)
(*    | FilePixmap  of PixmapFile * ImageId                             *)
      | FileImage   of ImageFile  * ImageId

    datatype CursorKind =
	NoCursor
      |	XCursor     of CursorName * ((Color * (Color option )) option )
      | FileCursor  of CursorFile * Color * ((CursorFile * Color) option )
	
    datatype Justify = JLeft | JRight | JCenter

    datatype WrapMode = NoWrap | WrapChar | WrapWord

    datatype FontConfig =
	Bold | Italic
      | Tiny | Small | NormalSize | Large | Huge
      | Scale of real
 
    datatype Font =   XFont of string  
      | Normalfont of FontConfig list      
      | Typewriter of FontConfig list 
      | SansSerif of  FontConfig list
      | Symbol of     FontConfig list

    datatype colorMode = PRINTCOLOR | PRINTGREY | PRINTMONO

    datatype ColorMapEntry = CME of string * string * string * string

    datatype FontMapEntry = FME of string * string *int

    datatype Orientation = Horizontal | Vertical

    datatype Configure =
	Width of int
      | Height of int
      | Borderwidth of int
      | Relief of RelKind
      | Foreground of Color
      | Background of Color
      | MUnderline of int               (* -underline ... for menus *)
      | Accelerator of string           (* -accelerator "bla" *)
      | Text of string			(* -Label "bla" *)
      | Font of Font			(* -font "bla" *)
      | Variable of string		(* -variable "bla" *)
      | Value of string			(* -value "bla" *)
      | Icon of IconKind                (* -bitmap or -image ... *)
      | Cursor of CursorKind            (* -cursor ... *)
      | Command of SimpleAction
      | Anchor of AnchorKind
      (* configuration options for tags in text widgets *)
      | FillColor    of Color
      | Outline      of Color
      | Offset of int
      | Underline
      | Justify of Justify
      | Wrap of WrapMode
      | OutlineWidth of int
(*    | Stipple *)
      | Smooth    of bool
      | Arrow     of ArrowPos
      | ScrollRegion of int * int * int * int
      | Capstyle  of CapstyleKind
      | Joinstyle of JoinstyleKind
      | ColorMap of ColorMapEntry list
      |	ColorMode of colorMode
      | File of string
      | FontMap of FontMapEntry list
      | PrintHeight of string
      | PageAnchor of AnchorKind
      | PageHeight of string
      | PageWidth of string
      | PageX of string
      | PageY of string
      | Rotate of bool
      | PrintWidth of string
      | PrintX of string
      | PrintY of string
      | Orient of Orientation
      | SLabel of string
      | Length of int
      | SliderLength of int
      | From of real
      | To of real
      | Resolution of real
      | Digits of int
      | BigIncrement of real
      | TickInterval of real
      | ShowValue of bool
      | SliderRelief of RelKind
      | Active of bool
      | SCommand of ScaleAction
      | RepeatDelay of int
      | RepeatInterval of int
      | ThroughColor of Color
      | InnerPadX of int
      | InnerPadY of int
      | Show of char
      | Tearoff of bool

    datatype UserKind =
	User
      | Program

    datatype WinConfigure =
	WinAspect       of int * int * int *int     (* xthin/ythin xfat/yfat *)
      |	WinGeometry     of ((int * int) Option.option)     (* width x height *)
	                 * ((int * int) Option.option)     (* xpos  x ypos   *)
(*
      | WinIcon         of IconKind
      | WinIconMask     of IconKind
      | WinIconName     of string
 *)
      | WinMaxSize      of int * int       (* width * height *)
      | WinMinSize      of int * int
      | WinPositionFrom of UserKind
      | WinSizeFrom     of UserKind
      | WinTitle        of string
      | WinGroup        of WinId                          (* window / leader *)
      | WinTransient    of WinId Option.option
      | WinOverride     of bool

    datatype Edge = Top | Bottom | Left | Right

    datatype Style = X | Y | Both

    datatype StickyKind =
	N | S | E | W | NS | NE | NW | SE | SW | EW | NSE | NSW | NEW | SEW
      | NSEW

    datatype ScrollType	=
	NoneScb | LeftScb | RightScb | TopScb | BotScb
      | LeftTopScb | RightTopScb | LeftBotScb | RightBotScb

    datatype Pack =
	Expand of bool
      | Fill of Style
      | PadX of int
      | PadY of int
      | Side of Edge
      | Column of int
      | Row of int
      | Sticky of StickyKind

    datatype Mark =
        Mark      of int        (* line number [1..] *)
                   * int        (* char number [0..] *)
      | MarkToEnd of int        (* end of line i     *)
      | MarkEnd                 (* end of text       *)


    (* main datatypes: widgets, text annotations, Canvas items, menu items *)

    datatype MItem =
	MCheckbutton of Configure list 
      | MRadiobutton of Configure list 
      | MCascade of MItem list * Configure list
      | MSeparator
      | MCommand of Configure list

    datatype CItem  = 
	CRectangle  of {citemId : CItemId, coord1 : Coord, coord2 : Coord,
		        configs : Configure list, bindings : Binding list}
      | COval       of {citemId : CItemId, coord1 : Coord, coord2 : Coord,
		        configs : Configure list, bindings : Binding list}
      | CLine       of {citemId : CItemId, coords : Coord list,
		        configs : Configure list, bindings : Binding list}
      | CPoly       of {citemId : CItemId, coords : Coord list,
		        configs : Configure list, bindings : Binding list}
      | CText       of {citemId : CItemId, coord : Coord,
                        configs : Configure list, bindings : Binding list}
      | CIcon       of {citemId : CItemId, coord : Coord, iconkind : IconKind,
		        configs : Configure list, bindings : Binding list}
      | CWidget     of {citemId : CItemId, coord : Coord, widgets : Widgets,
			configs : Configure list, bindings : Binding list}
      | CTag        of {citemId : CItemId, citemIds : CItemId list}

    and AnnoText =
	AnnoText of {len : (int* int) Option.option, str : string,
		     annotations :  Annotation list}

    and Annotation =
        TATag       of {annId : AnnId, marks : (Mark * Mark) list, 
                        configs : Configure list, bindings : Binding list}
      | TAWidget    of {annId : AnnId, mark : Mark, widgets : Widgets,
			configs : Configure list, bindings : Binding list}

    and Widget =
        Frame       of {widId : WidId, widgets : Widgets, packings : Pack list,
			configs : Configure list, bindings : Binding list}
      | Message     of {widId : WidId, packings : Pack list, 
                        configs : Configure list, bindings : Binding list}
      | Label	    of {widId : WidId, packings : Pack list, 
                        configs : Configure list, bindings : Binding list}
      | Listbox     of {widId : WidId, scrolltype : ScrollType, 
                        packings : Pack list, configs : Configure list, 
                        bindings : Binding list}
      | Button      of {widId : WidId, packings : Pack list, 
                        configs : Configure list, bindings : Binding list} 
      | Radiobutton of {widId : WidId, packings : Pack list, 
                        configs : Configure list, bindings : Binding list} 
      | Checkbutton of {widId : WidId, packings : Pack list, 
                        configs : Configure list, bindings : Binding list} 
      | Menubutton  of {widId : WidId, mitems : MItem list, 
                        packings : Pack list, configs : Configure list, 
                        bindings : Binding list} 
      | Entry       of {widId : WidId, packings : Pack list, 
                        configs : Configure list, bindings : Binding list}
      | TextWid     of {widId : WidId, scrolltype : ScrollType, 
                        annotext : AnnoText, packings : Pack list, 
                        configs : Configure list, bindings : Binding list}
      | Canvas      of {widId : WidId, scrolltype : ScrollType,
                        citems : CItem list, packings : Pack list, 
                        configs : Configure list, bindings : Binding list}
      | Popup       of {widId : WidId, configs : Configure list,
			mitems : MItem list}
      | ScaleWid    of {widId : WidId, packings : Pack list,
                        configs : Configure list, bindings : Binding list}

    and Widgets     =
        Pack of (Widget list)
      | Grid of (Widget list)


      (* -- selectors for all widgets *)
      val selWidgetId      : Widget -> WidId
      val selWidgetBind    : Widget -> Binding list
      val selWidgetConf    : Widget -> Configure list
      val selWidgetPack    : Widget -> Pack list

      (* -- update functions for all widgets *)
      val updWidgetBind    : Widget -> Binding list -> Widget
      val updWidgetConf    : Widget -> Configure list -> Widget
      val updWidgetPack    : Widget -> Pack list-> Widget

      (* -- selector for frames: get all subwidgets *)
      val selWidgets       : Widget -> Widget list

      (* -- selectors for Canvas widgets *)
      val selCanvasItems   : Widget -> CItem list
      val selCanvasScrollType : Widget -> ScrollType

      (* -- update functions for Canvas Widgets *)
      val updCanvasItems      : Widget -> CItem list -> Widget
      val updCanvasScrollType : Widget -> ScrollType -> Widget

      (* -- selectors for Text widgets *)
      val selTextWidScrollType  : Widget -> ScrollType
      val selTextWidAnnoText    : Widget -> AnnoText
      val selTextWidText        : Widget -> string
      val selTextWidAnnotations : Widget -> Annotation list
      val updTextWidScrollType  : Widget -> ScrollType      -> Widget
      val updTextWidAnnotations : Widget -> Annotation list -> Widget


      (* -- selectors for CItem *)
      val selItemId         : CItem -> CItemId
      val selItemCoords     : CItem -> Coord list
      val selItemConf       : CItem -> Configure list
      val selItemBind       : CItem -> Binding list

      val selItemIcon       : CItem -> IconKind
      val selItemItems      : CItem -> CItemId list
      val selItemWidgets    : CItem -> Widget list

      (* -- update functions for CItem *)
      val updItemCoords     : CItem -> Coord list -> CItem
      val updItemConf       : CItem -> Configure list -> CItem
      val updItemBind       : CItem -> Binding list -> CItem

      val updItemIcon       : CItem -> IconKind -> CItem
      val updItemItems      : CItem -> CItemId list -> CItem
      val updItemWidgets    : CItem -> Widget list -> CItem

      (* -- selectors and update function for AnnoText   *)
      val selText   : AnnoText -> string
      val selAnno   : AnnoText -> Annotation list
      val updAnno   : AnnoText -> Annotation list -> AnnoText
      val selLength : AnnoText -> {rows: int, cols: int}

      (* -- selectors for Annotation *)
      val selAnnotationId         : Annotation -> AnnId
      val selAnnotationConf       : Annotation -> Configure list
      val selAnnotationBind       : Annotation -> Binding list
      val selAnnotationMarks      : Annotation -> (Mark * Mark) list
      val selAnnotationWidgets    : Annotation -> Widget list
      (* -- update functions for Annotation *)
      val updAnnotationConf       : Annotation -> Configure list -> Annotation
      val updAnnotationBind       : Annotation -> Binding list   -> Annotation
      val updAnnotationWidgets    : Annotation -> Widget list    -> Annotation

      (* -- selectors for MItem *)
      val selMCommand        : MItem -> SimpleAction
      val selMRelief         : MItem -> RelKind
      val selMText           : MItem -> string
      val selMWidth          : MItem -> int
      val selMItemConfigure  : MItem -> Configure list
  
      type Window

      (* -- selectors *)
      val selWindowAction     : Window -> SimpleAction
      val selWindowBindings   : Window -> Binding list
      val selWindowConfigures : Window -> WinConfigure list
      val selWindowWidgets    : Window -> Widget list
      val selWindowWinId      : Window -> WinId
      val isWindowGrid        : Window -> bool

end



structure TkTypes : TK_TYPES =
  struct

    open Fonts
    open BasicTypes
    open Config
    open CItem
    open Annotation
    open AnnotatedText
    open TkEvent
    open ComState

    type EventName     = string
    type AnnId         = string
    type Title         = string
    type WidPath       = string

    type BitmapName    = string
    type BitmapFile    = string
    type ImageFile     = string
    type PixmapFile    = string
    type CursorName    = string
    type CursorFile    = string

    type EventName = string

    type Coord = int * int

    (* CItem *)
    val selItemConf       = selItemConfigure
    val selItemBind       = selItemBinding
    val updItemConf       = updItemConfigure
    val updItemBind       = updItemBinding

    (* Annotation *)
    val selAnnotationConf       = selAnnotationConfigure
    val selAnnotationBind       = selAnnotationBinding
    val updAnnotationConf       = updAnnotationConfigure
    val updAnnotationBind       = updAnnotationBinding

    (* Widget *) 
    val selWidgetId   = selWidgetWidId
    val selWidgetConf = selWidgetConfigure
    val selWidgetBind = selWidgetBinding
    val selWidgetPack = selWidgetPacking
    val selWidgetType = selWidgetWidgetType
    val updWidgetConf = updWidgetConfigure
    val updWidgetPack = updWidgetPacking
    val updWidgetBind = updWidgetBinding

    (* Frame *)
    fun selWidgets (Frame{widgets= ws,...})= selRawWids ws

    type Window = WinId * WinConfigure list * Widgets * Binding list *
                  SimpleAction


end
