structure Trace =
struct
  val on = false

  fun out s =
    if on
    then print (s ^ "\n")
    else ()

  fun outr s l =
      out (String_.replace s l)

  fun debug debug_on modulename s L =
    if debug_on
    then print (String_.replace (modulename^"."^s^"\n") L)
    else ()
end
