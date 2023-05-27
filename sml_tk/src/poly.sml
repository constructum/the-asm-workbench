(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/poly.sml,v $
 
   Implementation of system-dependend functions for Poly/ML 4.0 beta1 
   (ML97 + Standard Basis Library).
  
   $Date: 2001/03/30 13:39:15 $
   $Revision: 3.0 $
   Author: bu/cxl (Last modification by $Author: 2cxl $)

   (C) 2001, Universitaet Freiburg
 
  ************************************************************************** *)

structure SysDep : SYS_DEP
=
struct

    (* from Isabelle --- to be used in Makefiles *)
    fun exportML{init:unit->unit, 
                 banner:string, 
                 imagefile:string} = (PolyML.commit();())
        (* runs only very rudimentarily . . . *)

    fun setPrintDepth x = ();

	(* TTY handling -- to be done.
	 *)
    fun initTTY  f = () 
    fun resetTTY () = ();

        (* Don't know yet how to do interrupt handling in PolyML (but willing
	 * to learn), so we just return the function f.  
	 *)
    fun interruptable f g x = f x

        (* These shouldn't be here if everybody would just implemented all 
	 * of the basis library according to the spec but there you go.
	 * Note typecast: PolyML forgot to make Process.status an eqtype,
	 * so you can't check returned status. *)
    fun exec (s,sl) = 
        let val succ = (RunCall.unsafeCast (OS.Process.success)):int
            val pr = Unix.execute (s,sl)
        in  RunCall.unsafeCast(Unix.reap pr)= succ
        end

end

 
