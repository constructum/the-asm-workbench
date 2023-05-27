(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/numeric_chooser.sml,v $

   Numeric Choosers

   $Date: 2001/03/30 13:39:48 $
   $Revision: 3.0 $
   Author: ludi (Last modification by $Author: 2cxl $)

   (C) 1999, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)

structure NumericChooser : NUMERIC_CHOOSER_SIG =
    struct
	fun numeric_chooser {initial_value, min, max, increment, width,
			     orientation, selection_notifier} =
	    let
		open SmlTk

		val labId = newWidgetId()

		val STATE = ref initial_value

		fun toString i =
		    if i >= 0 then Int.toString i
		    else ("-" ^ Int.toString(abs i))

		fun change b _ =
		    if ((b andalso isSome max andalso !STATE < valOf max)
			orelse (b andalso not(isSome max))
			orelse (not b andalso isSome min andalso
				!STATE > valOf min)
			orelse (not b andalso not(isSome min))) then
			(if b then STATE := !STATE + increment
			 else STATE := !STATE - increment;
			     addConf labId [Text(toString(!STATE))];
			     selection_notifier(!STATE))
		    else ()

		val path =  OS.Path.concat(getLibPath(),
					   "icons/numeric_chooser")

		val left =
		    FileImage
		      (OS.Path.joinDirFile {dir  = path,
					    file = "left.gif"},
		       newImageId())

		val left_highlighted =
		    FileImage
		      (OS.Path.joinDirFile {dir  = path,
					    file = "left_highlighted.gif"},
		       newImageId())

		val right =
		    FileImage
		      (OS.Path.joinDirFile {dir  = path,
					    file = "right.gif"},
		       newImageId())

		val right_highlighted =
		    FileImage
		      (OS.Path.joinDirFile {dir  = path,
					    file = "right_highlighted.gif"},
		       newImageId())

		val up =
		    FileImage
		      (OS.Path.joinDirFile {dir  = path,
					    file = "up.gif"},
		       newImageId())

		val up_highlighted =
		    FileImage
		      (OS.Path.joinDirFile {dir  = path,
					    file = "up_highlighted.gif"},
		       newImageId())

		val down =
		    FileImage
		      (OS.Path.joinDirFile {dir  = path,
					    file = "down.gif"},
		       newImageId())

		val down_highlighted =
		    FileImage
		      (OS.Path.joinDirFile {dir  = path,
					    file = "down_highlighted.gif"},
		       newImageId())

		fun ch_icon id icon _ = addConf id [Icon icon]

		val arrow1 =
		    let
			val id = newWidgetId()
			val ic = if orientation = Horizontal then left else up
			val ic_highlighted =
			    if orientation = Horizontal then left_highlighted
			    else up_highlighted
			val inc = not(orientation = Horizontal)
		    in
			Button {widId    = id,
				packings =
				  if orientation = Horizontal then [Side Left]
				  else [],
				configs  = [Icon ic],
				bindings =
				  [BindEv(Enter, ch_icon id ic_highlighted),
				   BindEv(Leave, ch_icon id ic),
				   BindEv(ButtonPress(SOME 1), change inc)]}
		    end

		val lab = Label {widId    = labId,
				 packings =
				   if orientation = Horizontal then [Side Left]
				   else [],
				 configs  =
				   [Background White, Width width,
				    Text(Int.toString initial_value)],
				 bindings = []}

		val arrow2 =
		    let
			val id = newWidgetId()
			val ic = if orientation = Horizontal then right
				 else down
			val ic_highlighted =
			    if orientation = Horizontal then right_highlighted
			    else down_highlighted
			val inc = orientation = Horizontal
		    in
			Button {widId    = id,
				packings =
				  if orientation = Horizontal then [Side Left]
				  else [],
				configs  = [Icon ic],
				bindings =
				  [BindEv(Enter, ch_icon id ic_highlighted),
				   BindEv(Leave, ch_icon id ic),
				   BindEv(ButtonPress(SOME 1), change inc)]}
		    end

		val wids = [arrow1, lab, arrow2]

		fun read_value() = !STATE
		fun set_value i  =
		    if ((isSome min andalso i < valOf min)
			orelse(isSome max andalso i > valOf max)) then
			print "NumericChooser: set_value with value out of range, ignoring..."
		    else
			(STATE := i;
			 addConf labId [Text(Int.toString i)])
	    in
		{chooser    = Frame {widId    = newWidgetId(),
				     widgets  = Pack wids,
				     packings = [],
				     configs  = [],
				     bindings = []},
		 set_value  = set_value,
		 read_value = read_value}
	    end
    end
