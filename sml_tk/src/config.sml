(* *********************************************************************** *)
(*									   *)
(* Project: sml/Tk: an Tk Toolkit for sml	 			   *)
(* Author: Burkhart Wolff, University of Bremen	 			   *)
(* Date: 25.7.95				 			   *)
(* Purpose of this file: Functions related to "Tk-Configurations"	   *)
(*									   *)
(* *********************************************************************** *)

structure Config : CONFIG = 
struct

local open BasicUtil BasicTypes GuiState in

infix 1 confElem

(* *********************************************************************** *)
(*									   *)
(*  IMPLEMENTATION: ADD PACK INFORMATION to the "real" GUI		   *)
(*									   *)
(* *********************************************************************** *)

fun showExpand t  = if t then "1" else "0"

fun showStyle X    = "x"
  | showStyle Y    = "y"
  | showStyle Both = "both"

fun showEdge Top    = "top"
  | showEdge Bottom = "bottom"
  | showEdge Left   = "left"
  | showEdge Right  = "right"

fun showStickyKind N    = "n"
  | showStickyKind S    = "s"
  | showStickyKind E    = "e"
  | showStickyKind W    = "w"
  | showStickyKind NS   = "ns"
  | showStickyKind NE   = "ne"
  | showStickyKind NW   = "nw"
  | showStickyKind SE   = "se"
  | showStickyKind SW   = "sw"
  | showStickyKind EW   = "ew"
  | showStickyKind NSE  = "nse"
  | showStickyKind NSW  = "nsw"
  | showStickyKind NEW  = "new"
  | showStickyKind SEW  = "sew"
  | showStickyKind NSEW = "nsew"

fun packOneInfo (Expand b) = " -expand " ^ showExpand b
  | packOneInfo (Fill s)   = " -fill " ^ showStyle s
  | packOneInfo (PadX n)   = " -padx " ^ StringUtil.fromInt n
  | packOneInfo (PadY n)   = " -pady " ^ StringUtil.fromInt n
  | packOneInfo (Side e)   = " -side " ^ showEdge e
  | packOneInfo _          = ""

fun packInfo ps = concat (map packOneInfo ps)

fun gridOneInfo (PadX n)   = " -padx " ^ StringUtil.fromInt n
  | gridOneInfo (PadY n)   = " -pady " ^ StringUtil.fromInt n
  | gridOneInfo (Column n) = " -column " ^ StringUtil.fromInt n
  | gridOneInfo (Row n)    = " -row " ^ StringUtil.fromInt n
  | gridOneInfo (Sticky s) = " -sticky " ^ showStickyKind s
  | gridOneInfo _          = ""

fun gridInfo ps = concat (map gridOneInfo ps)


(* *********************************************************************** *)
(*									   *)
(* IMPLEMENTATION: SELECT CONFIGURE INFORMATION		 		   *)
(*									   *)
(* *********************************************************************** *)

fun confEq (Width _          ) (Width _          ) = true
  | confEq (Height _         ) (Height _         ) = true
  | confEq (Borderwidth _    ) (Borderwidth _    ) = true
  | confEq (Relief _         ) (Relief _         ) = true
  | confEq (Foreground _     ) (Foreground _     ) = true
  | confEq (Background _     ) (Background _     ) = true
  | confEq (MUnderline _     ) (MUnderline _     ) = true
  | confEq (Accelerator _    ) (Accelerator _    ) = true
  | confEq (Text _           ) (Text _           ) = true
  | confEq (Font _           ) (Font _           ) = true
  | confEq (Variable _       ) (Variable _       ) = true
  | confEq (Value _          ) (Value _          ) = true
  | confEq (Icon _           ) (Icon _           ) = true
  | confEq (Cursor _         ) (Cursor _         ) = true
  | confEq (Command _        ) (Command _        ) = true
  | confEq (Anchor _         ) (Anchor _         ) = true
  | confEq (FillColor _      ) (FillColor _      ) = true
  | confEq (Outline _        ) (Outline _        ) = true
  | confEq (OutlineWidth _   ) (OutlineWidth _   ) = true
(*| confEq (Stipple          ) (Stipple          ) = true *)
  | confEq (Smooth _         ) (Smooth _         ) = true
  | confEq (Offset _         ) (Offset _         ) = true
  | confEq Underline           Underline           = true
  | confEq (Justify _        ) (Justify _        ) = true
  | confEq (Wrap _           ) (Wrap _           ) = true
  | confEq (Arrow _          ) (Arrow _          ) = true
  | confEq (Capstyle _       ) (Capstyle _       ) = true
  | confEq (Joinstyle _      ) (Joinstyle _      ) = true
  | confEq (ScrollRegion _   ) (ScrollRegion _   ) = true
  | confEq (Orient _         ) (Orient _         ) = true
  | confEq (SLabel _         ) (SLabel _         ) = true
  | confEq (Length _         ) (Length _         ) = true
  | confEq (SliderLength _   ) (SliderLength _   ) = true
  | confEq (From _           ) (From _           ) = true
  | confEq (To _             ) (To _             ) = true
  | confEq (Resolution _     ) (Resolution _     ) = true
  | confEq (Digits _         ) (Digits _         ) = true
  | confEq (BigIncrement _   ) (BigIncrement _   ) = true
  | confEq (TickInterval _   ) (TickInterval _   ) = true
  | confEq (ShowValue _      ) (ShowValue _      ) = true
  | confEq (SliderRelief _   ) (SliderRelief _   ) = true
  | confEq (Active _         ) (Active _         ) = true
  | confEq (SCommand _       ) (SCommand _       ) = true
  | confEq (RepeatDelay _    ) (RepeatDelay _    ) = true
  | confEq (RepeatInterval _ ) (RepeatInterval _ ) = true
  | confEq (ThroughColor _   ) (ThroughColor _   ) = true
  | confEq (InnerPadX _      ) (InnerPadX _      ) = true
  | confEq (InnerPadY _      ) (InnerPadY _      ) = true
  | confEq (Show _           ) (Show _           ) = true
  | confEq (Tearoff _        ) (Tearoff _        ) = true
  | confEq _                   _                   = false

fun confName (Width _)          = "Width"
  | confName (Height _)         = "Height"
  | confName (Borderwidth _)    = "Borderwidth"
  | confName (Relief _)         = "Relief"
  | confName (Foreground _)     = "Foreground"
  | confName (Background _)     = "Background"
  | confName (MUnderline _)     = "MUnderline"
  | confName (Accelerator _)    = "Accelerator"
  | confName (Text _)           = "Text"
  | confName (Font _)           = "Font"
  | confName (Variable _)       = "Variable"
  | confName (Value _)          = "Value"
  | confName (Icon _)           = "Icon"
  | confName (Cursor _)         = "Cursor"
  | confName (Command _)        = "Command"
  | confName (Anchor _)         = "Anchor"
  | confName (FillColor _)      = "FillColor"
  | confName (Outline _)        = "Outline"
  | confName (OutlineWidth _)   = "OutlineWidth"
(*| confName (Stipple _)        = "Stipple"*)
  | confName (Smooth  _)        = "Smooth"
  | confName (Arrow _)          = "Arrow"
  | confName (ScrollRegion _)   = "ScrollRegion"
  | confName (Capstyle _)       = "Capstyle"
  | confName (Joinstyle _)      = "Joinstyle"
  | confName (ColorMap _)       = "ColorMap"
  | confName (ColorMode _)      = "ColorMode"
  | confName (File _)           = "File"
  | confName (FontMap _)        = "FontMap"
  | confName (PrintHeight _)    = "PrintHeight"
  | confName (PageAnchor _)     = "PageAnchor"
  | confName (PageHeight _)     = "PageHeight"
  | confName (PageWidth _)      = "PageWidth"
  | confName (PageX _)          = "PageX"
  | confName (PageY _)          = "PageY"
  | confName (Rotate _)         = "Rotate"
  | confName (PrintWidth _)     = "PrintWidth"
  | confName (PrintX _)         = "PrintX"
  | confName (PrintY _)         = "PrintY"
  | confName (Offset _)         = "Offset"
  | confName Underline          = "Underline"
  | confName (Justify _)        = "Justify"
  | confName (Wrap _)           = "Wrap"
  | confName (Orient _)         = "Orient"
  | confName (SLabel _)         = "SLabel"
  | confName (Length _)         = "Length"
  | confName (SliderLength _)   = "SliderLength"
  | confName (From _)           = "From"
  | confName (To _)             = "To"
  | confName (Resolution _)     = "Resolution"
  | confName (Digits _)         = "Digits"
  | confName (BigIncrement _)   = "BigIncrement"
  | confName (TickInterval _)   = "TickInterval"
  | confName (ShowValue _)      = "ShowValue"
  | confName (SliderRelief _)   = "SliderRelief"
  | confName (Active _)         = "Active"
  | confName (SCommand _)       = "SCommand"
  | confName (RepeatDelay _)    = "RepeatDelay"
  | confName (RepeatInterval _) = "RepeatInterval"
  | confName (ThroughColor _)   = "ThroughColor"
  | confName (InnerPadX _)      = "InnerPadX"
  | confName (InnerPadY _)      = "InnerPadY"
  | confName (Show _)           = "Show"
  | confName (Tearoff _)        = "Tearoff"
(*  | confName _                  =
    raise WIDGET "WidgetTree.confName not yet fully implemented!"*)

fun confElemH (c,[])      = false
  | confElemH (c,(x::xs)) = confEq c x orelse confElemH(c,xs)

val op confElem = confElemH

fun noDblP []      = true
  | noDblP (x::xs) = not (x confElem xs) andalso noDblP xs

(* defaultWidth : WidgetType -> int *)
fun defaultWidth        _ = 0
fun defaultHeight       _ = 0
fun defaultBorderwidth  _ = 0
fun defaultRelief       _ = Flat
fun defaultForeground   _ = Black
fun defaultBackgound    _ = Grey
fun defaultText         _ = ""
fun defaultFont         _ = (Fonts.Normalfont [])
fun defaultVariable     _ = "BLA" (* interesting... *)
fun defaultValue        _ = "0"
fun defaultIcon         _ = NoIcon
fun defaultCursor       _ = NoCursor
fun defaultCommand      _ = fn () => ()
fun defaultAnchor       _ = Center
fun defaultTextWidState _ = false
fun defaultFillColor    _ = White
fun defaultOutline      _ = Black
fun defaultOutlineWidth _ = 1
(* fun defaultStipple   _ = ... *)
fun defaultSmooth       _ = false
(* fun defaultArrow     _ = ... *)
fun defaultScrollRegion _ = (0, 0, 0, 0)
(* fun defaultCapstyle  _ = ... *)
(* fun defaultJoinstyle _ = ... *)

(* scw :: Configure list -> WidgetType -> int *)
fun scw [] wt               = defaultWidth wt
  | scw ((Width (n))::xs) _ = n
  | scw (x::xs) wt          = scw xs wt

fun selWidth w = scw (selWidgetConfigure w) (selWidgetWidgetType w)

fun selMWidth m = scw (selMItemConfigure m) (selMItemMItemType m)

fun sch [] wt = defaultHeight wt
  | sch ((Height h)::_) _ = h
  | sch (_::xs) w         = sch xs w

fun selHeight w= sch (selWidgetConfigure w) (selWidgetWidgetType w)

(* scr : Configure list -> WidgetType -> RelKind *)
fun scr [] wt = defaultRelief wt
  | scr ((Relief r)::xs) _ = r
  | scr (x::xs) wt         = scr xs wt

fun selRelief w = scr (selWidgetConfigure w) (selWidgetWidgetType w)

fun selMRelief w = scr (selMItemConfigure w) (selMItemMItemType w)

(* val sct : Configure list -> WidgetType -> string *)
fun sct [] wt = defaultText wt
  | sct ((Text t)::xs) _ = t
  | sct (x::xs) wt       = sct xs wt

fun selText w = sct (selWidgetConfigure w) (selWidgetWidgetType w)

fun selMText w = sct (selMItemConfigure w) (selMItemMItemType w)

(* scc : Configure list -> WidgetType -> SimpleAction *)
fun scc [] wt               = defaultCommand wt
  | scc ((Command c)::xs) _ = c
  | scc (x::xs) wt          = scc xs wt

fun selCommand w = scc (selWidgetConfigure w) (selWidgetWidgetType w)

fun selMCommand w = scc (selMItemConfigure w) (selMItemMItemType w)

fun scc' []                 = (fn _ => ())
  | scc' ((SCommand c)::xs) = c
  | scc' (x::xs)            = scc' xs

fun selSCommand w = scc' (selWidgetConfigure w)


(* *********************************************************************** *)
(*									   *)
(* 4C. ADD and UPDATE CONFIGURE INFORMATION to the internal GUI stat 	   *)
(*									   *)
(* *********************************************************************** *)

(* addOneConf : Configure list -> Configure -> Configure list *)
fun addOneConf(c, [])    = [c]
  | addOneConf(c, x::xs) =
    if confEq x c then c :: xs else x :: addOneConf(c, xs)

fun add old new = List.foldr addOneConf old new

(* newOneConf : Configure list -> Configure -> Configure list *)
fun newOneConf cs c = List.filter (not o(confEq c)) cs

(* defaultConf : WidgetType -> Configure -> Configure *)
fun defaultConf wt (Width _)        = Width (defaultWidth wt)
  | defaultConf wt (Height _)       = Height (defaultHeight wt)
  | defaultConf wt (Borderwidth _)  = Borderwidth (defaultBorderwidth wt)
  | defaultConf wt (Relief _)       = Relief (defaultRelief wt)
(*| defaultConf wt (Foreground _)   = Foreground (defaultForeground wt)
  | defaultConf wt (Background _)   = Background (defaultBackground wt)*)
  | defaultConf wt (Text _)         = Text (defaultText wt)
  | defaultConf wt (Font _)         = Font (defaultFont wt)
  | defaultConf wt (Variable _)     = Variable (defaultVariable wt)
  | defaultConf wt (Value _)        = Value (defaultValue wt)
  | defaultConf wt (Icon _)         = Icon (defaultIcon wt)
  | defaultConf wt (Cursor _)       = Cursor (defaultCursor wt)
  | defaultConf wt (Command _)      = Command (defaultCommand wt)
  | defaultConf wt (FillColor _)    = FillColor (defaultFillColor wt)
  | defaultConf wt (Outline _)      = Outline (defaultOutline wt)
  | defaultConf wt (OutlineWidth _) = OutlineWidth (defaultOutlineWidth wt)
(*| defaultconf wt (Stipple)        = Stipple*)
  | defaultConf wt (Smooth _)       = Smooth (defaultSmooth wt)
(*| defaultConf wt (Arrow _)        = Arrow (defaultArrow wt)
  | defaultConf wt (ScrollRegion _) = ScrollRegion(defaultScrollRegion wt)
  | defaultConf wt (Capstype _)     = Capstyle (defaultCapstyle wt)
  | defaultConf wt (Joinstyle _)    = Joinstyle (defaultJoinstyle wt)*)
  | defaultConf wt _                = 
    raise CONFIG "Config.defaultConf: not yet fully implemented"

fun new wt old nw =
    let
	val defold = foldl (twist (uncurry (newOneConf))) old nw
    in
	nw @ map (defaultConf wt) defold
    end


(* *********************************************************************** *)
(*									   *)
(* 4D. ADD CONFIGURE INFORMATION to the "real" GUI		 	   *)
(*									   *)
(* *********************************************************************** *)

fun showRel Flat   = "flat"
   |showRel Groove = "groove"
   |showRel Raised = "raised"
   |showRel Ridge  = "ridge"
   |showRel Sunken = "sunken"

local
    fun round n =
	if n < 0 then 0 else if n > 255 then 255 else n
in
    fun showCol NoColor= "{}"
      | showCol Black  = "black"
      | showCol White  = "white"
      | showCol Grey   = "grey"
      | showCol Blue   = "blue"
      | showCol Green  = "green"
      | showCol Red    = "red"
      | showCol Brown  = "brown"
      | showCol Yellow = "yellow"
      | showCol Purple = "purple"
      | showCol Orange = "orange"
      | showCol (Mix {red, blue, green}) =
	"\"#"^Word.toString(Word.fromInt(round red)) ^
	Word.toString(Word.fromInt(round green)) ^
	Word.toString(Word.fromInt(round blue)) ^ "\""
end

fun showAnchorKind North     = "n"
  | showAnchorKind NorthEast = "ne"
  | showAnchorKind East      = "e"
  | showAnchorKind SouthEast = "se"
  | showAnchorKind South     = "s"
  | showAnchorKind SouthWest = "sw"
  | showAnchorKind West      = "w"
  | showAnchorKind NorthWest = "nw"
  | showAnchorKind Center    = "center"

fun showState false = "normal"
  | showState true  = "disabled"

fun showIconKind (NoIcon) =
    " -bitmap {}"
  | showIconKind (TkBitmap (s)) =
    " -bitmap \"" ^ s ^ "\""
  | showIconKind (FileBitmap (s)) =
    " -bitmap \"@" ^ s ^ "\""
(*| showIconKind (FilePixmap(f,imId)) =
    " -image [image create pixmap " ^ imId ^ " -file " ^ f ^ "]"
 *)
  | showIconKind (FileImage(f,imId)) =
    " -image [image create photo " ^ imId ^ " -file " ^ f ^ "]"

fun showCursorKind (NoCursor)                      = "{}"
  | showCursorKind (XCursor(cn,NONE))              = cn
  | showCursorKind (XCursor(cn,SOME(fg,NONE)))     = cn ^ " " ^ (showCol fg)
  | showCursorKind (XCursor(cn,SOME(fg,SOME(bg)))) =
    cn ^ " " ^ (showCol fg)^ " " ^ (showCol bg)
  | showCursorKind (FileCursor(cf,fg,NONE))        =
    "{@" ^ cf ^ " " ^ (showCol fg) ^ "}"
  | showCursorKind (FileCursor(cf,fg,SOME(mf,bg))) =
    "{@" ^ cf ^ " " ^ mf ^ " " ^ (showCol fg) ^ " " ^ (showCol bg) ^ "}"

(* Added by E.L.Gunter 14 July 1998 *)
fun showArrowPos NoneAP = "none"
  | showArrowPos FirstAP = "first"
  | showArrowPos LastAP = "last"
  | showArrowPos BothAP = "both"

fun showColorMode PRINTCOLOR = "color"
  | showColorMode PRINTGREY = "grey"
  | showColorMode PRINTMONO = "mono"

fun showColorMapEntry (index, r,g,b) =
    "set colorMap(" ^ index ^ ") {" ^r^" "^g^" "^b^" " ^ "setrgbcolor}"

fun declareColorMap [] = []
  | declareColorMap (CME(cme1)::cmrest) =
    (Com.putTclCmd (showColorMapEntry cme1);
     declareColorMap cmrest)

fun showFontMapEntry (xfont,newfont,size) =
    "set fontMap(" ^ xfont ^ ") [" ^ newfont ^ " " ^
    StringUtil.fromInt size ^ "]"

fun declareFontMap [] = []
  | declareFontMap (FME(fme1)::fmrest) =
    (Com.putTclCmd (showFontMapEntry fme1);
     declareFontMap fmrest)

fun showBool b = if (b) then "true" else "false"

fun showReal r =
  if r < 0.0 then "-" ^ Real.toString(Real.abs r) else Real.toString r

(* showConf : IntPath -> bool -> Configure -> string *)
fun showConf _ (Width n)           = " -width " ^ StringUtil.fromInt n
  | showConf _ (Height n)          = " -height " ^ StringUtil.fromInt n
  | showConf _ (Borderwidth n)     = " -borderwidth " ^ StringUtil.fromInt n
  | showConf _ (Relief r)          = " -relief " ^ showRel r
  | showConf _ (Foreground r)      = " -foreground " ^ showCol r
  | showConf _ (Background r)      = " -background " ^ showCol r
  | showConf _ (Text t)            =
    " -text \"" ^ StringUtil.adaptString t ^ "\""
  | showConf _ (Font r)            = " -font " ^ Fonts.fontDescr r
  | showConf _ (Variable r)        = " -variable " ^ r
  | showConf _ (Value r)           = " -value " ^ r
  | showConf _ (Icon ick)          = showIconKind ick
  | showConf _ (Cursor ck)         = " -cursor " ^ showCursorKind ck
  | showConf (w, p) (Command c)    =
    " -command {" ^ Com.commToTcl ^ "  \"Command " ^ w ^ " " ^ p ^ "\"}"
  | showConf _ (Anchor a)          = " -anchor " ^ showAnchorKind a
  | showConf _ (FillColor r)       = " -fill " ^ showCol r
  | showConf _ (Outline r)         = " -outline " ^ showCol r
  | showConf _ (OutlineWidth n)    = " -width " ^ StringUtil.fromInt n
(*| showconf _ (Stipple)           = ...*)
  | showConf _ (Smooth true)       = " -smooth true"
  | showConf _ (Smooth false)      = ""
  | showConf _ (Capstyle csk)      =
    "-capstyle " ^ (case csk of Butt       => "butt"
			      | Projecting => "projecting"
			      | Round      => "round")
  | showConf _ (Joinstyle jk)      =
    "-joinstyle " ^ (case jk of Bevel     => "bevel"
			       | Miter     => "miter"
			       | RoundJoin => "round")
  | showConf _ (ScrollRegion (srl, srt, srr, srb)) =
    " -scrollregion {" ^ StringUtil.fromInt srl ^ " " ^ StringUtil.fromInt srt ^ " " ^
    StringUtil.fromInt srr ^ " " ^ StringUtil.fromInt srb ^ "}"
  | showConf _ (Offset i)          = " -offset " ^ StringUtil.fromInt i
  | showConf _ (Underline)         = " -underline true"
  | showConf _ (MUnderline n)      = " -underline " ^ StringUtil.fromInt n
  | showConf _ (Justify j)         =
    " -justify " ^ (case j of JLeft   => "left"
                            | JRight  => "right"
			    | JCenter => "center")
  | showConf _ (Wrap wm)           =
    " -wrap " ^ (case wm of NoWrap   => "none"
                          | WrapChar => "char"
			  | WrapWord => "word")
  | showConf _ (Arrow ap)          = " -arrow " ^ showArrowPos ap
  | showConf _ (Orient or)         =
    " -orient " ^ (case or of Horizontal => "horizontal"
                            | Vertical   => "vertical")
  | showConf _ (SLabel s)          = " -label " ^ s
  | showConf _ (Length i)          = " -length " ^ StringUtil.fromInt i
  | showConf _ (SliderLength i)    = " -sliderlength " ^ StringUtil.fromInt i
  | showConf _ (From r)            = " -from " ^ showReal r
  | showConf _ (To r)              = " -to " ^ showReal r
  | showConf _ (Resolution r)      = " -resolution " ^ showReal r
  | showConf _ (Digits i)          = " -digits " ^ StringUtil.fromInt i
  | showConf _ (BigIncrement r)    = " -bigincrement " ^ showReal r
  | showConf _ (TickInterval r)    = " -tickinterval " ^ showReal r
  | showConf _ (ShowValue b)       =
    " -showvalue " ^ (if b then "true" else "false")
  | showConf _ (SliderRelief r)    = " -sliderrelief " ^ showRel r
  | showConf _ (Active b)          = " -state " ^ showState(not b)
  | showConf (w, p) (SCommand c)   =
    " -command {" ^ Com.commToTcl' ^ "  \"SCommand " ^ w ^ " " ^ p ^ "\"}"
  | showConf _ (RepeatDelay i)     = " -repeatdelay " ^ StringUtil.fromInt i
  | showConf _ (RepeatInterval i)  = " -repeatinterval " ^ StringUtil.fromInt i
  | showConf _ (ThroughColor c)    = " -throughcolor " ^ showCol c
  | showConf _ (InnerPadX i)       = " -padx " ^ StringUtil.fromInt i
  | showConf _ (InnerPadY i)       = " -pady " ^ StringUtil.fromInt i
  | showConf _ (Show c)            = " -show " ^ Char.toString c
  | showConf _ (Tearoff to)        = " -tearoff " ^ Bool.toString to
  | showConf _ _                   =
    raise CONFIG "Config.showConf: not yet fully implemented"

fun showPrintConf (ColorMap cml)  = (declareColorMap cml;
				     " -colormap colorMap")
  | showPrintConf (ColorMode c)   = " -colormode " ^ (showColorMode c)
  | showPrintConf (File f)        = " -file " ^ f
  | showPrintConf (FontMap fml)   = (declareFontMap fml;
				     " -fontmap fontMap")
  | showPrintConf (PrintHeight h) = " -height " ^ h
  | showPrintConf (PageAnchor pa) = " -pageanchor " ^ showAnchorKind pa
  | showPrintConf (PageHeight ph) = " -pageheight " ^ ph
  | showPrintConf (PageWidth pw)  = " -pagewidth " ^ pw
  | showPrintConf (PageX px)      = " -pagex " ^ px
  | showPrintConf (PageY py)      = " -pagey " ^ py
  | showPrintConf (Rotate r)      = " -rotate " ^ showBool r
  | showPrintConf (PrintWidth w)  = " -width " ^ w
  | showPrintConf (PrintX px)     = " -x " ^ px
  | showPrintConf (PrintY py)     = " -y " ^ py
  | showPrintConf _               =
    raise CONFIG "Config.showPrintConf: not yet fully implemented"

fun pack p cs = concat (map (showConf p) cs)

fun packCascPath [m:int]   = StringUtil.fromInt m
  | packCascPath (m::n::s) = StringUtil.fromInt m ^ "." ^ packCascPath (n::s)
  | packCascPath _         =
    raise CONFIG "Config.packCascPath: match exhausted"

fun readCascPath str =
    let fun rc strS = 
            let val (m1,m2) = (StringUtil.breakAtDot) strS 
            in  if m2 = "" then [StringUtil.toInt m1]
                else (StringUtil.toInt m1)::(rc m2)
            end
    in  rc str
    end

fun showAllPrintConf [] = ""
  | showAllPrintConf (c1::crest) =
	concat ([(showPrintConf c1)] @ [(showAllPrintConf crest)])

(* showMConf :: IntPath -> Int -> Configure s -> String
-- Width and Relief may not appear in menu configures
-- showMConf _ (Width n)   = " -width " ^ (shop n)
-- showMConf _ (Relief r)  = " -relief " ^ (showRel r) *)

fun showMConf _ _ (Text t)         =
    " -label \"" ^ StringUtil.adaptString t ^ "\""
(* Check missing: The following two options only possible in *)
(* radio- or Checkbuttons *)
  | showMConf _ _ (Variable r)     = " -variable " ^ r
  | showMConf _ _ (Value r)        = " -value " ^ r
  | showMConf _ _ (MUnderline n)   = " -underline " ^ StringUtil.fromInt n
  | showMConf _ _ (Accelerator s)  = " -accelerator " ^ s
  | showMConf _ _ (Tearoff b)      = " -tearoff " ^ Bool.toString b
  | showMConf (w, p) m (Command c) =
    " -command {" ^ Com.commToTcl ^" \"MCommand " ^ w ^ " " ^ p ^ " " 
    ^ (packCascPath m) ^ "\"}"
  | showMConf _ _ (Font f)         = " -font " ^ Fonts.fontDescr f
  | showMConf _ _ (Foreground r)   = " -foreground " ^ showCol r
  | showMConf _ _ (Background r)   = " -background " ^ showCol r
  | showMConf _ _ _                =
    raise CONFIG ("Config.showMConf: got wrong Config")
 
(* packM :: IntPath -> Int -> [Configure s] -> String *)
fun packM p m cs = concat (map (showMConf p m) cs);

fun winConfEq (WinAspect  (_,_,_,_)) (WinAspect  (_,_,_,_)) = true
  | winConfEq (WinGeometry    (_,_)) (WinGeometry    (_,_)) = true
(*
  | winConfEq (WinIcon           _ ) (WinIcon           _ ) = true
  | winConfEq (WinIconMask       _ ) (WinIconMask       _ ) = true
  | winConfEq (WinIconName       _ ) (WinIconName       _ ) = true
 *)
  | winConfEq (WinMaxSize     (_,_)) (WinMaxSize     (_,_)) = true
  | winConfEq (WinMinSize     (_,_)) (WinMinSize     (_,_)) = true
  | winConfEq (WinPositionFrom   _ ) (WinPositionFrom   _ ) = true
  | winConfEq (WinSizeFrom       _ ) (WinSizeFrom       _ ) = true
  | winConfEq (WinTitle          _ ) (WinTitle          _ ) = true
  | winConfEq (WinGroup          _ ) (WinGroup          _ ) = true
  | winConfEq (WinTransient      _ ) (WinTransient      _ ) = true
  | winConfEq (WinOverride       _ ) (WinOverride       _ ) = true
  | winConfEq _                      _                      = false;

fun addOneWinConf (c, []   ) = [c]
  | addOneWinConf (c, x::xs) =
    if winConfEq x c then c :: xs else x :: addOneWinConf(c,  xs)

fun addWinConf old new = List.foldr addOneWinConf old new



fun accMaybe f wcnfgs  =
    let val mbs = map f wcnfgs
    in  List.foldl (fn (_ , SOME x)=> SOME x | (x, NONE)=> x) NONE mbs
    end
(* old :- 
     let 
	fun ddd (SOME x) _ = SOME x
	  | ddd NONE     x = x
	val mbs = map f wcnfgs
    in
	BasicUtil.foldl ddd NONE mbs
    end *)


fun sAsp (WinAspect(c as (_,_,_,_))) = SOME c
   |sAsp _                           = NONE

fun selWinAspect w = accMaybe sAsp (selWindowConfigures w)

fun sGeom (WinGeometry(c as (_,_))) = SOME c
   |sGeom _                         = NONE

fun selWinGeometry w = accMaybe sGeom (selWindowConfigures w)

(*
fun sIcon (WinIcon i) = SOME i
   |sIcon _           = NONE

fun selWinIcon w = accMaybe sIcon (selWindowConfigures w)

fun sIconMask (WinIconMask i) = SOME i
   |sIconMask _               = NONE

fun selWinIconMask w = accMaybe sIconMask (selWindowConfigures w)

fun sIconName (WinIconName i) = SOME i
   |sIconName _               = NONE

fun selWinIconName w = accMaybe sIconName (selWindowConfigures w)
 *)
fun sMaxSize (WinMaxSize(c as _)) = SOME c
   |sMaxSize _                    = NONE

fun selWinMaxSize w = accMaybe sMaxSize (selWindowConfigures w)

fun sMinSize (WinMinSize(c as _)) = SOME c
   |sMinSize _                    = NONE

fun selWinMinSize w = accMaybe sMinSize (selWindowConfigures w)

fun sPositionFrom (WinPositionFrom i) = SOME i
   |sPositionFrom _                   = NONE

fun selWinPositionFrom w = accMaybe sPositionFrom (selWindowConfigures w)

fun sSizeFrom (WinSizeFrom i) = SOME i
   |sSizeFrom _               = NONE

fun selWinSizeFrom w = accMaybe sSizeFrom (selWindowConfigures w)

fun sTitle (WinTitle i) = SOME i
   |sTitle _            = NONE

fun selWinTitle w = accMaybe sTitle (selWindowConfigures w)

fun sGroup (WinGroup gl) = SOME gl
  | sGroup _             = NONE

fun selWinGroup w = accMaybe sGroup (selWindowConfigures w)

fun sTransient (WinTransient i) = SOME i
  | sTransient _                = NONE

fun selWinTransient w = accMaybe sTransient (selWindowConfigures w)

fun sOver (WinOverride b) = SOME b
  | sOver _               = NONE

fun selWinOverride w = accMaybe sOver (selWindowConfigures w)



fun showPos i = 
    if ( i >= 0 ) then ("+" ^ (StringUtil.fromInt i))
                  else ("-" ^ (StringUtil.fromInt (i * ~1)))

fun packWinConf win (WinAspect (x1, y1, x2, y2)) =
    "wm aspect " ^ win ^ " " ^ StringUtil.fromInt x1 ^ " " ^ StringUtil.fromInt y1
    ^ " " ^ StringUtil.fromInt x2 ^ " " ^ StringUtil.fromInt y2 ^ "\n"
  | packWinConf win (WinGeometry (NONE, SOME(x, y))) =
    "wm geometry " ^ win ^ " =" ^ showPos x ^ showPos y ^ "\n"
  | packWinConf win (WinGeometry (SOME(w, h), NONE)) =
    "wm geometry " ^ win ^ " =" ^ StringUtil.fromInt w ^ "x" ^ StringUtil.fromInt h ^ "\n"
  | packWinConf win (WinGeometry (SOME(w, h), SOME(x, y))) =
    "wm geometry " ^ win ^ " ="
    ^ StringUtil.fromInt w ^ "x" ^ StringUtil.fromInt h  ^ showPos x ^ showPos y ^ "\n"
  | packWinConf win (WinMaxSize(w, h)) =
    "wm maxsize " ^ win ^ " " ^ StringUtil.fromInt w ^ " " ^ StringUtil.fromInt h ^ "\n"
  | packWinConf win (WinMinSize(w, h)) =
    "wm minsize " ^ win ^ " " ^ StringUtil.fromInt w ^ " " ^ StringUtil.fromInt h ^ "\n"
  | packWinConf win (WinPositionFrom User) =
    "wm positionfrom " ^ win ^ " user" ^ "\n"
  | packWinConf win (WinPositionFrom Program) =
    "wm positionfrom " ^ win ^ " program" ^ "\n"
  | packWinConf win (WinSizeFrom User) =
    "wm sizefrom " ^ win ^ " user" ^ "\n"
  | packWinConf win (WinSizeFrom Program) =
    "wm sizefrom " ^ win ^ " program" ^ "\n"
  | packWinConf win (WinTitle t) =
    "wm title " ^ win ^ " \"" ^ StringUtil.adaptString t ^ "\"" ^ "\n"
  | packWinConf win (WinGroup gl) =
    if isInitWin gl then
	"wm group " ^ win ^ " ." ^ "\n"
    else 
	"wm group " ^ win ^ " ." ^ gl ^ "\n"
  | packWinConf win (WinTransient NONE) =
    "wm transient " ^ win ^ "\n"
  | packWinConf win (WinTransient (SOME w)) =
    if isInitWin w then
	"wm transient " ^ win ^ " ." ^ "\n"
    else 
	"wm transient " ^ win ^ " ." ^ w ^ "\n"
  | packWinConf win (WinOverride true) =
    "wm overrideredirect " ^ win ^ " true" ^ "\n"
  | packWinConf win (WinOverride false) =
    "wm overrideredirect " ^ win ^ " false" ^ "\n"

end
end
