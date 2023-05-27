(* ***********************************************************************

   Project: sml/Tk: an Tk Toolkit for sml
   Author: Stefan Westmeier, University of Bremen
  $Date: 2001/03/30 13:39:13 $
  $Revision: 3.0 $
   Purpose of this file: Mark Module

   *********************************************************************** *)

signature MARK =
    sig
	exception MARK of string

	val show  : BasicTypes.Mark -> string
	val showL : (BasicTypes.Mark * BasicTypes.Mark) list -> string

	val read  : string -> BasicTypes.Mark
	val readL : string -> (BasicTypes.Mark * BasicTypes.Mark) list

    end
