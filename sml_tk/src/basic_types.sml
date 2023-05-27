(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/basic_types.sml,v $
 
   Basic Data Structures for sml_tk
  
   $Date: 2001/03/30 13:39:00 $
   $Revision: 3.0 $
   Author: bu (Last modification by $Author: 2cxl $)

   (C) 1996, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

structure BasicTypes =
struct

    type SimpleAction  	= unit -> unit
    type ScaleAction    = real -> unit

    type WidPath	= string
    type TclPath	= string
    type WidId		= string
    type WinPath        = string
    type WinId		= string
    type Title          = string
    type AnnId          = string
    type IntPath	= (WinId * WidPath)
    type PathAss	= (WidId * IntPath)

    type CItemId        = string
    type Coord          = (int * int)
    type BitmapName     = string
    type BitmapFile     = string
    type ImageFile      = string
    type ImageId        = string
(*  type PixmapFile     = string *)
    type CursorName     = string
    type CursorFile     = string


    datatype TkEvent =
	TkEvent of int                       (* %b  Button number     *)
	        *  string                    (* %s  state field       *)
                *  int                       (* %x  x field           *)
                *  int                       (* %y  y field           *)
                *  int                       (* %X  x_root field      *)
                *  int                       (* %Y  y_root field      *)
(*
	TKEButtonPress of int                 (* %b  Button number     *)
	               *  string              (* %s  state field       *)
                       *  int                 (* %x  x field           *)
                       *  int                 (* %y  y field           *)
                       *  int                 (* %X  x_root field      *)
                       *  int                 (* %Y  y_root field      *)
      | TKEUnspecified of
	                  int                 (* %x  x field           *)
                       *  int                 (* %y  y field           *)
	
*)

    type Action = TkEvent -> unit

    datatype Event = 
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

    datatype Color = 
	NoColor | Black | White | Grey | Blue | Green | Red | Brown | Yellow
      | Purple  | Orange | Mix of {red : int, blue : int, green : int}

    datatype ArrowPos = 
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
      |	XCursor    of CursorName *
	              ((Color * (Color Option.option)) Option.option) 
      | FileCursor of CursorFile * Color * ((CursorFile * Color) Option.option)

    datatype colorMode = PRINTCOLOR | PRINTGREY | PRINTMONO

    datatype ColorMapEntry = CME of string * string * string * string

    datatype FontMapEntry = FME of string * string *int

    datatype Justify = JLeft | JRight | JCenter

    datatype WrapMode = NoWrap | WrapChar | WrapWord

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
      | Font of Fonts.Font		(* -font "bla" *)
      | Variable of string		(* -variable "bla" *)
      | Value of string			(* -value "bla" *)
      | Icon of IconKind                (* -bitmap or -image ... *)
      | Cursor of CursorKind            (* -cursor ... *)
      | Command of SimpleAction
      | Anchor of AnchorKind
      | FillColor    of Color
      | Outline      of Color
      | OutlineWidth of int
(*    | Stipple *)
      | Smooth    of bool
      | Arrow     of ArrowPos
      |	ScrollRegion of int * int * int * int
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
      | Offset  of int       (* Offset over baseline for texts *)
      | Underline            (* underline for texts (see MUnderline above) *)
      | Justify of Justify   (* Justification: left/right/center *)
      | Wrap of WrapMode
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
      | WinGeometry     of ((int * int) Option.option)     (* width x height *)
	                 * ((int * int) Option.option)     (* xpos  x ypos   *)
(*
      | WinIcon         of IconKind
      | WinIconMask     of IconKind
      | WinIconName     of string
 *)
      | WinMaxSize      of int * int                       (* width * height *)
      | WinMinSize      of int * int
      | WinPositionFrom of UserKind
      | WinSizeFrom     of UserKind
      | WinTitle        of string
      | WinGroup        of WinId                          (* window / leader *)
      | WinTransient    of WinId Option.option
      | WinOverride     of bool

    datatype Edge	= Top | Bottom | Left | Right

    datatype Style = X | Y | Both

    datatype StickyKind =
	N | S | E | W | NS | NE | NW | SE | SW | EW | NSE | NSW | NEW | SEW
      | NSEW

    datatype ScrollType	=
	NoneScb | LeftScb | RightScb | TopScb | BotScb
      |	LeftTopScb | RightTopScb | LeftBotScb | RightBotScb

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
	Mark      of int * int 
      | MarkToEnd of int 
      | MarkEnd

    datatype  MItem =
	MCheckbutton of Configure list 
      | MRadiobutton of Configure list 
      | MCascade of MItem list * Configure list
      | MSeparator
      | MCommand of Configure list

    datatype AnnotationType = ATTag | ATWidget

    datatype CItemType =
	CTRectangle | CTOval | CTLine | CTPoly (*| CTArc*) | CTText
      |	CTIcon | CTWidget | CTTag

    datatype MItemType = MChe | MRad | MCas | MSep | MCo

    datatype WidgetType	=
	Fra | Mes | Lab | Lis | But | Che | Rad | Sca
      | Menbut | Ent | Can | Tex | Pop

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

    and AnnoText    =
	AnnoText    of {len : (int* int) Option.option, str : string,
		        annotations :  Annotation list}

    and Annotation  =
        TATag       of {annId : AnnId, marks : (Mark * Mark) list, 
                        configs : Configure list, bindings : Binding list}
      | TAWidget    of {annId : AnnId, mark : Mark, widgets : Widgets,
			configs : Configure list, bindings : Binding list}

    and Widget	    =  
        Frame       of {widId : WidId, widgets : Widgets,
                        packings : Pack list, configs : Configure list,
                        bindings : Binding list}
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

    type Window	= WinId * WinConfigure list * Widgets * Binding list *
                  SimpleAction



(* ************************************************************************* *)
(* 									     *)
(* Initialization of the internal state					     *)
(* 								             *)
(* ************************************************************************* *)

(*
    type GUI		= ((Window) list * (PathAss list))


    val  GUI_state  = ref([]:(Window) list,[]:PathAss list,[]:TclAnswer list) 
*)

    type TclAnswer  = string

    type programName	= string
    type programParms	= string list
    type program	= (programName * programParms)
    type protocolName	= string



(* ************************************************************************* *)
(* 								             *)
(* Exceptions								     *)
(* 									     *)
(* ************************************************************************* *)

    exception CONFIG of string
    exception WIDGET of string
    exception WINDOWS of string
    exception TCL_ERROR of string



(* ****************************************************************** *)
(* 								      *)
(* Elementary Selectors	and Tests				      *)
(* 								      *)
(* ****************************************************************** *)

fun selWindowWinId (a, _, _, _, _) = a

fun selWindowConfigures (_, wc, _, _, _) = wc

fun selWindowWidgets (_, _, c, _, _) = case c of Pack ws => ws
                                               | Grid ws => ws

fun selRawWids (Pack ws) = ws
  | selRawWids (Grid ws) = ws

fun isGrid ws = case ws of Pack _ => false
                         | Grid _ => true

fun isWindowGrid (_, _, ws, _, _) = isGrid ws

fun selWindowBindings (_, _, _, b, _) = b

fun selWindowAction (_, _, _, _, d) = d

fun updWindowConfigures (id,wc,c,b,d) wc' = (id,wc',c,b,d)

fun selWidgetWidId(Frame{widId,...})       = widId
  | selWidgetWidId(Message{widId,...})	   = widId
  | selWidgetWidId(Label{widId,...})	   = widId
  | selWidgetWidId(Listbox{widId,...})     = widId
  | selWidgetWidId(Button{widId,...})	   = widId
  | selWidgetWidId(Radiobutton{widId,...}) = widId
  | selWidgetWidId(Checkbutton{widId,...}) = widId
  | selWidgetWidId(Menubutton{widId,...})  = widId
  | selWidgetWidId(TextWid{widId,...})     = widId
  | selWidgetWidId(Canvas{widId,...})      = widId
  | selWidgetWidId(Popup{widId,...})       = widId
  | selWidgetWidId(Entry{widId,...})	   = widId
  | selWidgetWidId(ScaleWid{widId,...})    = widId

fun selWidgetWidgetType(Frame _)       = Fra
  | selWidgetWidgetType(Message _)     = Mes
  | selWidgetWidgetType(Label _)       = Lab
  | selWidgetWidgetType(Listbox _)     = Lis
  | selWidgetWidgetType(Button _)      = But
  | selWidgetWidgetType(Radiobutton _) = Rad
  | selWidgetWidgetType(Checkbutton _) = Che
  | selWidgetWidgetType(Menubutton _)  = Menbut
  | selWidgetWidgetType(TextWid _)     = Tex
  | selWidgetWidgetType(Canvas _)      = Can
  | selWidgetWidgetType(Popup _)       = Pop
  | selWidgetWidgetType(Entry _)       = Ent
  | selWidgetWidgetType(ScaleWid _)    = Sca

fun selWidgetBinding(Frame{bindings,...})       = bindings
  | selWidgetBinding(Message{bindings,...})     = bindings
  | selWidgetBinding(Label{bindings,...})       = bindings
  | selWidgetBinding(Listbox{bindings,...})     = bindings
  | selWidgetBinding(Button{bindings,...})      = bindings
  | selWidgetBinding(Radiobutton{bindings,...}) = bindings
  | selWidgetBinding(Checkbutton{bindings,...}) = bindings
  | selWidgetBinding(Menubutton{bindings,...})  = bindings
  | selWidgetBinding(TextWid{bindings,...})     = bindings
  | selWidgetBinding(Canvas{bindings,...})      = bindings
  | selWidgetBinding(Popup w)                   = []
  | selWidgetBinding(Entry{bindings,...})       = bindings
  | selWidgetBinding(ScaleWid{bindings,...})    = bindings

fun updWidgetBinding (Frame{widId, widgets, packings, configs, bindings}) nb  =
    Frame{widId=widId, widgets=widgets, packings=packings, 
	  configs=configs, bindings=nb}
  | updWidgetBinding (Message{widId, packings,configs, bindings}) nb          =
    Message{widId=widId, packings=packings,
	    configs=configs, bindings=nb}
  | updWidgetBinding (Label{widId, packings, configs, bindings}) nb           =
    Label{widId=widId, packings=packings, 
	  configs=configs, bindings=nb}
  | updWidgetBinding (Listbox{widId,scrolltype,packings,configs,bindings}) nb =
    Listbox{widId=widId, scrolltype=scrolltype, 
	    packings=packings, configs=configs, bindings=nb}
  | updWidgetBinding (Button{widId, packings, configs, bindings}) nb          =
    Button{widId=widId, packings=packings, 
	   configs=configs, bindings=nb}
  | updWidgetBinding (Radiobutton{widId, packings, configs, bindings}) nb     =
    Radiobutton{widId=widId, packings=packings,
		configs=configs, bindings=nb}
  | updWidgetBinding (Checkbutton{widId, packings, configs, bindings}) nb     =
    Checkbutton{widId=widId, packings=packings, configs=configs, bindings=nb}
  | updWidgetBinding (Menubutton{widId=widId, mitems,
				 packings, configs, bindings}) nb             =
    Menubutton{widId=widId, mitems=mitems, 
	       packings=packings,configs=configs, bindings=nb}
  | updWidgetBinding (Entry{widId, packings, configs, bindings}) nb           =
    Entry{widId=widId, packings=packings, 
	  configs=configs, bindings=nb}
  | updWidgetBinding (Canvas{widId, scrolltype, citems, 
                             packings, configs, bindings}) nb                 =
    Canvas{widId=widId, scrolltype=scrolltype, citems=citems,
	   packings=packings, configs=configs, bindings=nb}
  | updWidgetBinding (TextWid{widId, scrolltype, annotext, packings, 
                              configs, bindings}) nb                          =
    TextWid{widId=widId,scrolltype=scrolltype,annotext=annotext,
	    packings=packings, configs=configs, bindings=nb}
  | updWidgetBinding (ScaleWid{widId, packings, configs, bindings}) nb        =
    ScaleWid{widId=widId, packings=packings, configs=configs, bindings=nb}
  | updWidgetBinding pop nb    = pop


fun selWidgetPacking(Frame{packings,...})       = packings
  | selWidgetPacking(Message{packings,...})     = packings
  | selWidgetPacking(Label{packings,...})       = packings
  | selWidgetPacking(Listbox{packings,...})     = packings
  | selWidgetPacking(Button{packings,...})      = packings
  | selWidgetPacking(Radiobutton{packings,...}) = packings
  | selWidgetPacking(Checkbutton{packings,...}) = packings
  | selWidgetPacking(Menubutton{packings,...})  = packings
  | selWidgetPacking(TextWid{packings,...})     = packings
  | selWidgetPacking(Canvas{packings,...})      = packings
  | selWidgetPacking(Popup w)                   = []
  | selWidgetPacking(Entry{packings,...})       = packings
  | selWidgetPacking(ScaleWid{packings,...})    = packings

  fun updWidgetPacking (Frame{widId, widgets, packings, configs,
			      bindings}) np = 
                 Frame{widId=widId, widgets=widgets, packings=np, 
                       configs=configs, bindings=bindings}
  | updWidgetPacking (Message{widId, packings,configs, bindings}) np         = 
                 Message{widId=widId, packings=np,
                         configs=configs, bindings=bindings}
  | updWidgetPacking (Label{widId, packings, configs, bindings}) np          = 
                 Label{widId=widId, packings=np, 
                       configs=configs, bindings=bindings}
  | updWidgetPacking (Listbox{widId,scrolltype,packings,configs,bindings}) np= 
                 Listbox{widId=widId, scrolltype=scrolltype, 
                         packings=np, configs=configs, bindings=bindings}
  | updWidgetPacking (Button{widId, packings, configs, bindings}) np         = 
                 Button{widId=widId, packings=np, 
                        configs=configs, bindings=bindings}
  | updWidgetPacking (Radiobutton{widId, packings, configs, bindings}) np    = 
                 Radiobutton{widId=widId, packings=np, 
                             configs=configs, bindings=bindings}
  | updWidgetPacking (Checkbutton{widId, packings, configs, bindings}) np    = 
                 Checkbutton{widId=widId, packings=np, 
                             configs=configs, bindings=bindings}
  | updWidgetPacking (Menubutton{widId=widId, mitems, 
                      packings, configs, bindings}) np     = 
                 Menubutton{widId=widId, mitems=mitems, packings=np,
			    configs=configs, bindings=bindings}
  | updWidgetPacking (Entry{widId, packings, configs, bindings}) np          = 
                 Entry{widId=widId, packings=np, 
                       configs=configs, bindings=bindings}
  | updWidgetPacking (Canvas{widId, scrolltype, citems, 
                             packings, configs, bindings}) np    = 
                 Canvas{widId=widId, scrolltype=scrolltype, citems=citems, 
                        packings=np, configs=configs, bindings=bindings}
  | updWidgetPacking (TextWid{widId, scrolltype, annotext, packings, 
                              configs, bindings}) np = 
                 TextWid{widId=widId,scrolltype=scrolltype,annotext=annotext, 
                         packings=np, configs=configs, bindings=bindings}
  | updWidgetPacking (ScaleWid{widId, packings, configs, bindings}) np =
      ScaleWid{widId=widId,packings=np,configs=configs,bindings=bindings}
  | updWidgetPacking pop np    = pop 


fun selWidgetConfigure(Frame       {configs,...}) = configs
  | selWidgetConfigure(Message     {configs,...}) = configs
  | selWidgetConfigure(Label       {configs,...}) = configs
  | selWidgetConfigure(Listbox     {configs,...}) = configs
  | selWidgetConfigure(Button      {configs,...}) = configs
  | selWidgetConfigure(Radiobutton {configs,...}) = configs
  | selWidgetConfigure(Checkbutton {configs,...}) = configs
  | selWidgetConfigure(Menubutton  {configs,...}) = configs
  | selWidgetConfigure(TextWid     {configs,...}) = configs
  | selWidgetConfigure(Canvas      {configs,...}) = configs
  | selWidgetConfigure(Popup       {configs,...}) = configs
  | selWidgetConfigure(Entry       {configs,...}) = configs
  | selWidgetConfigure(ScaleWid    {configs,...}) = configs


fun updWidgetConfigure (Frame{widId,widgets,packings,configs,bindings}) nc = 
                 Frame{widId=widId, widgets=widgets, packings=packings, 
                       configs=nc, bindings=bindings}
  | updWidgetConfigure (Message{widId, packings,configs, bindings}) nc     = 
                 Message{widId=widId, packings=packings,
                         configs=nc, bindings=bindings}
  | updWidgetConfigure (Label{widId, packings, configs, bindings}) nc      = 
                 Label{widId=widId, packings=packings, configs=nc, 
                       bindings=bindings}
  | updWidgetConfigure(Listbox{widId,scrolltype,packings,configs,bindings})nc =
                 Listbox{widId=widId, scrolltype=scrolltype, 
                         packings=packings, configs=nc, bindings=bindings}
  | updWidgetConfigure (Button{widId, packings, configs, bindings}) nc     = 
                 Button{widId=widId, packings=packings, 
                        configs=nc, bindings=bindings}
  | updWidgetConfigure (Radiobutton{widId, packings, configs, bindings}) nc= 
                 Radiobutton{widId=widId, packings=packings, 
                             configs=nc, bindings=bindings}
  | updWidgetConfigure (Checkbutton{widId, packings, configs, bindings}) nc= 
                 Checkbutton{widId=widId, packings=packings, 
                             configs=nc, bindings=bindings}
  | updWidgetConfigure (Menubutton{widId, mitems,
                                   packings,configs,bindings}) nc = 
                 Menubutton{widId=widId,mitems=mitems,
                            packings=packings,configs=nc,bindings=bindings}
  | updWidgetConfigure (Entry{widId, packings, configs, bindings}) nc      = 
                 Entry{widId=widId, packings=packings, 
                       configs=nc, bindings=bindings}
  | updWidgetConfigure (Canvas{widId,scrolltype,citems,
                               packings,configs,bindings}) nc = 
                 Canvas{widId=widId,scrolltype=scrolltype,citems=citems,
                        packings=packings,configs=nc, bindings=bindings}
  | updWidgetConfigure (TextWid{widId, scrolltype, annotext, 
                        packings, configs, bindings}) nc      = 
                 TextWid{widId=widId,scrolltype=scrolltype,annotext=annotext,
                         packings=packings,configs=nc,bindings=bindings}
  | updWidgetConfigure (ScaleWid{widId, packings, configs, bindings}) nc =
      ScaleWid{widId=widId, packings=packings, configs=nc, bindings=bindings}
  | updWidgetConfigure pop _ = pop

fun selMItemConfigure (MCommand cs)     = cs
  | selMItemConfigure (MCheckbutton cs) = cs
  | selMItemConfigure (MRadiobutton cs) = cs
  | selMItemConfigure (MCascade(_, cs)) = cs
  | selMItemConfigure  _                = []

fun selMItemMItemType MSeparator       = MSep
  | selMItemMItemType (MCheckbutton _) = MChe
  | selMItemMItemType (MRadiobutton _) = MRad
  | selMItemMItemType (MCascade _)     = MCas
  | selMItemMItemType (MCommand _)     = MCo

fun scrollTypeToEdgeH LeftScb     = Left
  | scrollTypeToEdgeH RightScb    = Right
  | scrollTypeToEdgeH LeftTopScb  = Left
  | scrollTypeToEdgeH RightTopScb = Right
  | scrollTypeToEdgeH LeftBotScb  = Left
  | scrollTypeToEdgeH RightBotScb = Right
  | scrollTypeToEdgeH _           =
    raise CONFIG "BasicTypes.scrollTypeToEdgeH: match exhausted"

fun scrollTypeToEdgeV TopScb      = Top
  | scrollTypeToEdgeV BotScb      = Bottom
  | scrollTypeToEdgeV LeftTopScb  = Top
  | scrollTypeToEdgeV RightTopScb = Top
  | scrollTypeToEdgeV LeftBotScb  = Bottom
  | scrollTypeToEdgeV RightBotScb = Bottom
  | scrollTypeToEdgeV _           =
    raise CONFIG "BasicTypes.scrollTypeToEdgeV: match exhausted"

fun scrollTypeToOppEdgeH LeftScb     = Right
  | scrollTypeToOppEdgeH RightScb    = Left
  | scrollTypeToOppEdgeH LeftTopScb  = Right
  | scrollTypeToOppEdgeH RightTopScb = Left
  | scrollTypeToOppEdgeH LeftBotScb  = Right
  | scrollTypeToOppEdgeH RightBotScb = Left
  | scrollTypeToOppEdgeH _           = 
    raise CONFIG "BasicTypes.scrollTypeToOppEdgeH: match exhausted"

fun scrollTypeToOppEdgeV TopScb      = Bottom
  | scrollTypeToOppEdgeV BotScb      = Top
  | scrollTypeToOppEdgeV LeftTopScb  = Bottom
  | scrollTypeToOppEdgeV RightTopScb = Bottom
  | scrollTypeToOppEdgeV LeftBotScb  = Top
  | scrollTypeToOppEdgeV RightBotScb = Top
  | scrollTypeToOppEdgeV _           =
    raise CONFIG "BasicTypes.scrollTypeToOppEdgeH: match exhausted"

fun scrollTypeToGrid scb =
    case scb of
	LeftTopScb  => ([Row 1, Column 2], [Row 2, Column 1],
			[Row 2, Column 2])
      | RightTopScb => ([Row 1, Column 1], [Row 2, Column 2],
			[Row 2, Column 1])
      | LeftBotScb  => ([Row 2, Column 2], [Row 1, Column 1],
			[Row 1, Column 2])
      | RightBotScb => ([Row 2, Column 1], [Row 1, Column 2],
			[Row 1, Column 1])

fun single LeftScb  = true
  | single RightScb = true
  | single TopScb   = true
  | single BotScb   = true
  | single _        = false

fun orient LeftScb  = true
  | orient RightScb = true
  | orient _        = false



end

