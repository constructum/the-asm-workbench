signature FILE =
sig
  exception MkAbsolute
  exception MkRelative

  val exists :string -> bool
  val mkAbsolute :string -> string
  val mkRelative :string -> string
end


structure File =
struct
  fun exists filename =
    let val result = ref true
	val file = TextIO.openIn filename handle IO.Io _ => (result := false; TextIO.stdIn)
    in  if !result = true then (TextIO.closeIn file) handle IO.Io _ => () else ();
	!result
    end

  exception MkAbsolute of string
  fun mkAbsolute filename = 
    (OS.FileSys.fullPath filename) handle _ => raise MkAbsolute filename

  exception MkRelative of string
  fun mkRelative filename = 
  ( if OS.Path.isRelative filename
    then OS.FileSys.realPath filename
    else OS.Path.mkRelative { path = OS.FileSys.fullPath filename, relativeTo = OS.FileSys.getDir () } )
	handle _ => raise MkRelative filename
end
