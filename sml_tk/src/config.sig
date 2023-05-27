(* ***********************************************************************

   Project: sml/Tk: an Tk Toolkit for sml
   Author: Stefan Westmeier, University of Bremen
  $Date: 2001/03/30 13:39:07 $
  $Revision: 3.0 $
   Purpose of this file: Functions related to "Tk-Configurations"

   *********************************************************************** *)

signature CONFIG =
    sig
	val selWidth    : BasicTypes.Widget -> int
	val selHeight   : BasicTypes.Widget -> int
	val selRelief   : BasicTypes.Widget -> BasicTypes.RelKind
	val selText     : BasicTypes.Widget -> string
	val selCommand  : BasicTypes.Widget -> BasicTypes.SimpleAction

	val selSCommand : BasicTypes.Widget -> BasicTypes.ScaleAction

	val selMWidth   : BasicTypes.MItem -> int
	val selMRelief  : BasicTypes.MItem -> BasicTypes.RelKind
	val selMText    : BasicTypes.MItem -> string
	val selMCommand : BasicTypes.MItem -> BasicTypes.SimpleAction

	val confEq : BasicTypes.Configure -> BasicTypes.Configure -> bool
	val confName : BasicTypes.Configure -> string
	val noDblP : BasicTypes.Configure list -> bool

	val add    : BasicTypes.Configure list -> BasicTypes.Configure list ->
	             BasicTypes.Configure list
	val new    : BasicTypes.WidgetType -> BasicTypes.Configure list ->
	             BasicTypes.Configure list -> BasicTypes.Configure list

	val pack   : BasicTypes.IntPath -> BasicTypes.Configure list -> string
	val packM  : BasicTypes.IntPath -> int list ->
	             BasicTypes.Configure list -> string

	val showAllPrintConf : BasicTypes.Configure list -> string

	val readCascPath : string -> int list

	(* ### geh÷rt hier nicht her *)
	val packInfo : BasicTypes.Pack list -> string
	val gridInfo : BasicTypes.Pack list -> string

	val showIconKind       : BasicTypes.IconKind -> string

	val showState          : bool -> string

        val showReal           : real -> string

	val winConfEq          : BasicTypes.WinConfigure -> 
	                         BasicTypes.WinConfigure -> bool

	val addWinConf         : BasicTypes.WinConfigure list -> 
	                         BasicTypes.WinConfigure list -> 
				 BasicTypes.WinConfigure list

	val selWinAspect       : BasicTypes.Window -> 
	                         (int * int * int * int) Option.option
	val selWinGeometry     : BasicTypes.Window -> 
	                         (((int * int) Option.option) *
				  ((int * int) Option.option)   ) Option.option

(*	val selWinIcon         : Window -> IconKind Option.option
	val selWinIconMask     : Window -> IconKind Option.option
	val selWinIconName     : Window -> string Option.option
 *) 
	val selWinMaxSize      : BasicTypes.Window -> 
	                         (int * int) Option.option
	val selWinMinSize      : BasicTypes.Window -> 
	                         (int * int) Option.option 
	val selWinPositionFrom : BasicTypes.Window -> 
	                         BasicTypes.UserKind Option.option
	val selWinSizeFrom     : BasicTypes.Window -> 
	                         BasicTypes.UserKind Option.option
	val selWinTitle        : BasicTypes.Window -> 
	                         BasicTypes.Title Option.option
	val selWinGroup        : BasicTypes.Window -> 
	                         BasicTypes.WinId Option.option
	val selWinTransient    : BasicTypes.Window -> 
	                         BasicTypes.WinId Option.option Option.option
	val selWinOverride     : BasicTypes.Window -> 
	                         bool Option.option

	val packWinConf : string -> BasicTypes.WinConfigure -> string
    end
