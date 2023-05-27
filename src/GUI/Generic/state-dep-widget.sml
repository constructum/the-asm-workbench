(* ******************************************************************* *\
 *
 *   The ASM Workbench - GUI
 *
 *   structure StateDepWidget
 *
 *   Description:  State dependent widgets for GUI
 *
 *   Used in combination with structure GUI_StateElem to redraw all the
 *   necessary widgets after a GUI state change
 *
\* ******************************************************************* *)

signature StateDepWidget_sig =
sig
  val init              : unit -> unit
  val shutdown          : unit -> unit
  val register_widget   : SmlTk.WidId * (SmlTk.WidId -> unit) * GUI_StateElem.GSE list -> unit
  val unregister_widget : SmlTk.WidId -> unit
  val full_gui_update   : unit -> unit

  val register_redraw_unregister : GUI_StateElem.GSE list -> SmlTk.Widget * (SmlTk.WidId -> unit) ->
                                   (unit -> unit) * (unit -> unit) * (unit -> unit)
end


structure StateDepWidget :StateDepWidget_sig =
struct
  fun debug s L = GUI_Misc.debug false "StateDepWidget" s L
		 
  structure SDW_Map = struct
    structure Map = BinaryMapFn ( struct
      type ord_key = SmlTk.WidId
      val compare  = String.compare o Pair.map SmlTk.mkWidgetString
    end )
    open Map
  end

  type 'a MAP = 'a SDW_Map.map ref

  datatype SDW' =
     StateDepWidget of
       { widId   : SmlTk.WidId,
	 redraw  : (SmlTk.WidId -> unit) ref,
	 dependencies : GUI_StateElem.GSE list,
         SDW_map : (SDW' ref) MAP }

  type SDW = SDW' ref
  type SDW_MAP = SDW MAP

  val sdw_map = ref (SDW_Map.empty :SDW SDW_Map.map)

  fun register_widget (widId :SmlTk.WidId, redraw_fct :SmlTk.WidId -> unit, dependencies) =
    let val _ = debug "register_widget ('$1', ..., [$2])"
			[ SmlTk.mkWidgetString widId,
			  List_.output ", " (map GUI_StateElem.get_name dependencies) ]
	val sdw :SDW = ref ( StateDepWidget { widId = widId, redraw = ref redraw_fct,
					      dependencies = dependencies,
					      SDW_map = sdw_map } )
    in sdw_map := SDW_Map.insert (!sdw_map, widId, sdw);
       debug "sdw_map: $1" [ List_.output ", " (map SmlTk.mkWidgetString (SDW_Map.listKeys (!sdw_map))) ]    end

  fun unregister_widget (widId :SmlTk.WidId) =
  ( debug "unregister_widget ('$1')" [ SmlTk.mkWidgetString widId ];
    sdw_map := #1 (SDW_Map.remove (!sdw_map, widId)) )

  fun clear_map () =
  ( debug "clear_map ()" [];
    SDW_Map.map (fn ref (StateDepWidget { widId, ... }) => unregister_widget widId) (!sdw_map); () )

  fun init     () = ( debug "init ()" [];     clear_map () )
  fun shutdown () = ( debug "shutdown ()" []; clear_map () )

  fun sdw_to_string (ref (StateDepWidget { widId, redraw, dependencies, ... })) =
      String_.replace "StateDepWidget { widId = '$1', redraw = <fct>, dependencies = [ $2 ] }"
		      [ SmlTk.mkWidgetString widId,
			List_.output ", " (map GUI_StateElem.gse_to_string dependencies) ]

  fun full_gui_update () =
    let val _ = debug "full_gui_update " []
	(* val _ = SDW_Map.map (fn s => print (sdw_to_string s^"\n")) (!sdw_map) *)
	fun redraw_sdw (ref (StateDepWidget { widId, redraw, dependencies, ... })) =
          let val _ = debug "-> redraw_sdw ('$1', ...)" [ SmlTk.mkWidgetString widId ]
	  in if List.exists GUI_StateElem.is_updated dependencies
	     then (!redraw) widId
	     else ()
	  end
    in SDW_Map.map redraw_sdw (!sdw_map);
       GUI_StateElem.reset_all ();
       ()
    end

   (* shortcut for creating state-dependent widgets *)
  fun register_redraw_unregister dependencies (widget, redraw_widget)  =
    let fun register_fun () =
        ( register_widget (SmlTk.selWidgetId widget, redraw_widget, dependencies); () )
        fun redraw_fun () =
	  redraw_widget (SmlTk.selWidgetId widget)
	fun unregister_fun () =
	  unregister_widget (SmlTk.selWidgetId widget)
    in (register_fun, redraw_fun, unregister_fun)  
    end
end
