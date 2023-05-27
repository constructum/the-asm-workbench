(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/root_mosml.sml,v $
 
   PolyML root file. (Known to run with MoscowML 2.00.)
  
   $Date: 2001/03/30 13:39:16 $
   $Revision: 3.0 $
   Author: cxl (Last modification by $Author: 2cxl $)

   (C) 2001, Universitaet Freiburg
 
  ************************************************************************** *)

(* First load all required structures ("units") *)

load "Int";
load "Substring";
load "String";
load "Unix";
load "OS";
load "Process";
load "ListPair";
load "IO";
load "Date";
load "Time";
load "Bool";
load "CommandLine";
load "Timer";
load "Real";

(* load "Posix"; can't find that-- hence this: *)

structure Posix =
    struct 
	structure Process =
	struct fun sleep x = x end 
        structure ProcEnv =
	struct fun getlogin () = "unknown" 
	       fun getuid() = ~1  end
	structure SysDB =
	struct fun getpwuid _ = "unknown"
               structure Passwd = struct fun name _ = "unknown" end end
	   
    end;

(* OS.FileSys.readDir has a different type in MoscowML's basis, hence this: *)
structure OS =
    struct open OS
        structure FileSys= 
            struct 
                open FileSys
                val readDir= fn ds=> Option.getOpt(readDir ds, "")
            end 
    end; 




(* use Moscow-ML specific file *)
use "sys_dep.sig";
use "mosml.sml";

(* Then use CM-produced root file *)
use "root.sml";



(* Here's how to produce root.sml from inside SML/NJ: 

CM.mkusefile "root.sml";

Of course, in there we need to delete njml.sml 
and sys_dep.sig.
*)
