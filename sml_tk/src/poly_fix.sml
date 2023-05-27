(*
 * Fix two definitions in PolyML's OS structure by overloading the 
 * structure with the appropriate definitions.
 *
 * Has to be done in a separate file, so we can reload this conditionally;
 * this file is only read if the valued fixedOSforPolyMLbySmlTk doesn't exist. 
 *)

structure OS =

struct
  open OS
  structure FileSys= 

  struct 
    open FileSys
    fun readDir d= Option.getOpt(FileSys.readDir d, "")
  end

  structure Path =
  struct
    open Path
    fun mkRelative (p, r)= Path.mkRelative{path=p, relativeTo=r}
  end

end

val fixedOSforPolyMLbySmlTk = (); (* need idiosyncratic name here *)




