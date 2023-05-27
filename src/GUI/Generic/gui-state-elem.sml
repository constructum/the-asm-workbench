(* ******************************************************************* *\
 *
 *   The ASM Workbench - GUI
 *
 *   structure GUI_StateElem
 *
 *   Description:  Track updates of (GUI) state elements
 *
 *   Used in combination with structure StateDepWidget to redraw all the
 *   necessary widgets after a GUI state change
 *
\* ******************************************************************* *)

signature GUI_StateElem_sig =
sig
  type GSE
(* val init          : unit -> unit*)
  val register_elem : string -> GSE
  val set_updated   : GSE -> unit
  val is_updated    : GSE -> bool
  val get_name      : GSE -> string
  val gse_to_string : GSE -> string
  val reset_all     : unit -> unit
end

  
structure GUI_StateElem : GUI_StateElem_sig =
struct
  fun debug s L = GUI_Misc.debug false "GUI_StateElem" s L
		 
  datatype GSE' =
    GUI_StateElem of
      { name : string,
	updated : bool ref }

  type GSE = GSE' ref
  type GSE_LIST  = GSE list ref

  val gse_list = ref ([] : GSE list)

(* fun init () = (gse_list := [])*)

  fun register_elem (name : string) =
    let val _ = debug "register_elem (..., '$1')" [ name ]
	val gse = ref ( GUI_StateElem {name = name, updated = ref true } )
    in gse_list := gse :: (!gse_list);
       gse
    end

  fun set_updated (ref (GUI_StateElem { name, updated })) =
    let val _ = debug "set_updated ('$1')" [ name ]
    in updated := true
    end
		      
  fun is_updated (ref (GUI_StateElem { name, updated })) =
    let val _ = debug "is_updated ('$1') = $2" [ name, Bool.toString (!updated) ]
    in !updated
    end

  fun get_name (ref (GUI_StateElem { name, updated })) = name
		      
  fun gse_to_string (ref (GUI_StateElem { name, updated })) =
    String_.replace "'$1':$2" [ name, Bool.toString (!updated) ]
		      
  fun reset_all () =
    let val _ = debug "reset (...)" []
    in map (fn ref (GUI_StateElem { name, updated }) => updated := false) (!gse_list);
       ()
    end
end
