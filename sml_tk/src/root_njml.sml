
use "sys_dep.sig";
use "njml.sml";

(* Then use CM-produced root file *)
use "root.sml";



(* Here's how to produce root.sml from inside SML/NJ: 

CM.mkusefile "root.sml";

Of course, in there we need to delete njml.sml 
and sys_dep.sig.
*)
