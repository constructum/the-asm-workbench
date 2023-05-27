(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/mosml.sml,v $
 
   Implementation of system-dependend functions for Moscow ML 2.00
  
   $Date: 2001/03/30 13:39:14 $
   $Revision: 3.0 $
   Author: cxl (Last modification by $Author: 2cxl $)

   (C) 2000, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

structure SysDep : SYS_DEP
	
=

struct

    (* All of these are void for the time being... *)
    fun exportML{init, banner, imagefile} =
     ()

    fun setPrintDepth n = ()

    fun initTTY abort =()  
	    
    fun resetTTY () = ()
       

   (* Wrap an interrupt handler around a function f *)
   fun interruptable f i a =
       f a	      

       
   val execute= Unix.streamsOf o Unix.execute
       
   fun exec (s,sl) = 
       let
	   val pr = Unix.execute (s,sl)
       in  (Unix.reap pr)= Process.success 
       end
   
end


