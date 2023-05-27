(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/root_poly.sml,v $
 
   PolyML root file. (Known to run with PolyML 4.0beta1.)
  
   $Date: 2001/03/30 13:39:17 $
   $Revision: 3.0 $
   Author: bu/cxl (Last modification by $Author: 2cxl $)

   (C) 2001, Universitaet Freiburg
 
  ************************************************************************** *)


(* if we haven't run before, fix definitions in structure OS *)

if List.all (fn s=> not (s= "fixedOSforPolyMLbySmlTk"))
                        (PolyML.Compiler.valueNames())
then use "poly_fix.sml" else ();

use "sys_dep.sig";
use "poly.sml";


(* Then use CM-produced root file *)
use "root.sml";


(* Here's how to produce root.sml from inside SML/NJ: 

CM.mkusefile "root.sml";

Of course, in there we need to delete njml.sml 
and sys_dep.sig.
*)
