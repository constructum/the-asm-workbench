structure Entry =
struct
  open GUI_Misc

  fun int_ ((lo, hi), pack_list, continuation :int -> unit) =
    let val wid_id = newWidgetId ()
	fun all_digits L =
	  List.all Misc.id (map (fn c => c >= #"0" andalso c <= #"9") L)
	fun is_integer s =
	  let fun is_int ([], state) = if state = 2 then true else false
	        | is_int (#"-" :: xs, state) = if state = 0 then is_int (xs, 1) else false
	        | is_int (L, state) =
		    if state = 0 orelse state = 1
		    then let val len = length L in (len > 0) andalso (len <= 8) andalso (all_digits L) end
	            else false
          in is_int (explode (String_.trim s), 0)
	  end
	fun error () =
	  StdDialog.error (String_.replace "Please enter an integer between $1 and $2."
					   (map Int.toString [ Int.max (~99999999, lo), Int.min (99999999, hi) ]))
        exception Invalid
        fun convert s =
	  if not (is_integer s)
	  then ( error (); raise Invalid )
	  else let val value = Int.scan StringCvt.DEC Substring.getc
                                 (Substring.full s)
	       in case value of
                    SOME (x,_) => if (x < lo) orelse (x > hi)
		                  then ( error (); raise Invalid )
		                  else x
                  | NONE => raise Invalid
	       end
    in ( Entry ( wid_id, pack_list, [ Width 6 ],
		 [ BindEv ( KeyPress "Return",
                            fn _ => (continuation (convert (readTextAll wid_id)) handle _ => ())) ] ),
         wid_id,
         fn () => convert (readTextAll wid_id),
	 fn (n :int) => insertNewText wid_id (Int.toString n) )
    end
end
