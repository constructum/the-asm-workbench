(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/lazy_tree_objects.sig,v $

   Lazy Tree Lists: Objects signature

   $Date: 2001/03/30 13:39:44 $
   $Revision: 3.0 $
   Author: ludi (Last modification by $Author: 2cxl $)

   (C) 1999, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)

signature LAZY_TREE_OBJECTS =
    sig
	type obj

	val children      : obj -> obj list
	val is_leaf       : obj -> bool
	val sel_name      : obj -> string
	val icon          : obj -> SmlTk.IconKind
	val selected_icon : obj -> SmlTk.IconKind
    end

