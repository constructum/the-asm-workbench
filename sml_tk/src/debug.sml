(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/debug.sml,v $
 
   Structure for debugging output.
 
   $Date: 2001/03/30 13:39:09 $
   $Revision: 3.0 $

   Author: Stefan Westmeier (Last modification by $Author: 2cxl $)

   (C) 1996, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)


structure Debug : DEBUG = 
struct

    open BasicUtil

    val all    = [0]

    val debugs = ref ([]:int list)
	
    fun one_on  0 = debugs := all
      | one_on  l = (debugs := (l:: (!debugs)))
    
    fun one_off 0 = debugs := []
      | one_off l = (debugs := List.filter (not o (eq l)) (!debugs))

    val on  = app one_on 
    val off = app one_off

    fun print l s = if ((!debugs)= all) orelse
	               (List.exists (eq l) (!debugs))
		    then TextIO.output(TextIO.stdErr, s^"\n") 
		    else ()

    val warnme = ref true

    fun warning msg = 
	if (!warnme) then TextIO.output(TextIO.stdErr, "WARNING: "^msg^"\n")
	else ()

    fun warn_on () = warnme:= true

    fun warn_off ()= warnme:= false	
		    
    fun error msg  = TextIO.output(TextIO.stdErr, "ERROR: "^msg^"\n")

end

