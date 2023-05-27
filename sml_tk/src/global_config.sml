(* ***************************************************************************

   $Source: /repository/sml/sml_tk/src/global_config.sml,v $

   General Default-Configuration Interface for 
     Fonts, Events, Matcher and Colours


   $Date: 2001/03/30 13:39:12 $
   $Revision: 3.0 $
   Author: bu, cxl (Last modification by $Author: 2cxl $)

   (C) 1999, Bremen Institute for Safe Systems, Universitaet Bremen
   (C) 1999, Albert-Ludwigs-Universitaet Freiburg

  ************************************************************************** *)

signature EVENTS_SIG=
sig
   val Config:{abort_event:SmlTk.Event ref, activate_event:SmlTk.Event ref,
                cancel_event:SmlTk.Event ref, close_event:SmlTk.Event ref,
                confirm_event:SmlTk.Event ref, copy_event:SmlTk.Event ref,
                delete_event:SmlTk.Event ref, 

                drag_event:SmlTk.Event ref, drop_event:SmlTk.Event ref, 
                dd_motion_event: SmlTk.Event ref,
                dd_enter_event : SmlTk.Event ref,
                dd_leave_event : SmlTk.Event ref,

		object_menu_event : SmlTk.Event ref,

                duplicate_event:SmlTk.Event ref,
                find_event:SmlTk.Event ref, help_event:SmlTk.Event ref,
                info_event:SmlTk.Event ref, kill_event:SmlTk.Event ref,
                new_event:SmlTk.Event ref, paste_event:SmlTk.Event ref,
                quit_event:SmlTk.Event ref, sel_all:SmlTk.Event ref,
                sel_elem_event:SmlTk.Event ref, 
                sel_group_elem_event:SmlTk.Event ref,
                sel_range_event:SmlTk.Event ref, show_event:SmlTk.Event ref,
                update_event:SmlTk.Event ref}
    val abort_event    : unit -> SmlTk.Event
    val activate_event : unit -> SmlTk.Event
    val cancel_event   : unit -> SmlTk.Event
    val close_event    : unit -> SmlTk.Event
    val confirm_event  : unit -> SmlTk.Event
    val copy_event     : unit -> SmlTk.Event
    val delete_event   : unit -> SmlTk.Event
    val drag_event     : unit -> SmlTk.Event
    val drop_event     : unit -> SmlTk.Event
    val dd_motion_event: unit -> SmlTk.Event
    val dd_leave_event : unit -> SmlTk.Event
    val dd_enter_event : unit -> SmlTk.Event 
    val object_menu_event : unit-> SmlTk.Event

    val duplicate_event: unit -> SmlTk.Event
    val find_event     : unit -> SmlTk.Event
    val help_event     : unit -> SmlTk.Event
    val info_event     : unit -> SmlTk.Event
    val kill_event     : unit -> SmlTk.Event
    val new_event      : unit -> SmlTk.Event
    val paste_event    : unit -> SmlTk.Event
    val quit_event     : unit -> SmlTk.Event
    val sel_all        : unit -> SmlTk.Event
    val sel_elem_event : unit -> SmlTk.Event
    val sel_group_elem_event : unit -> SmlTk.Event
    val sel_range_event: unit -> SmlTk.Event
    val show_event     : unit -> SmlTk.Event
    val update_event   : unit -> SmlTk.Event

    val init           : unit -> unit
end;

signature COLORS_SIG=
sig
    val Config : {background:SmlTk.Color ref, background_act:SmlTk.Color ref,
                  background_sel:SmlTk.Color ref, foreground:SmlTk.Color ref,
                  foreground_act:SmlTk.Color ref, 
                  foreground_sel:SmlTk.Color ref}
    val init   : unit -> unit
end;

signature LCONFMGR_SIG =
sig
    type object = exn
    type object_id
    val mk_object_id  : string -> object_id
    val get_string_id : object_id -> string
    val id_kinds      : unit -> string list
    val get_data      : object_id -> object option
    val put_data      : object_id * object -> unit
end


signature MATCHER_SIG=
sig
    val Config        : {matcher:(string -> string -> bool) ref}
end;

structure GlobalConfig:
sig
    structure Fonts   : FONTS_SIG;
    structure Events  : EVENTS_SIG;
    structure Colors  : COLORS_SIG;
    structure Matcher : MATCHER_SIG;
    structure LocalConfigMgr : LCONFMGR_SIG
end =
struct
    open SmlTk;

    structure Fonts : FONTS_SIG = Fonts;
    structure Events : EVENTS_SIG = 
    struct
        val Config = 
               {sel_elem_event      = ref(ButtonPress(SOME 1)),
                sel_group_elem_event= ref(Ctrl(ButtonPress(SOME 1))),
                sel_range_event     = ref(Shift(ButtonPress(SOME 1))),
                sel_all             = ref(Meta (KeyPress "A")),
                activate_event      = ref(Double(ButtonPress(SOME 1))),
		object_menu_event   = ref(ButtonPress(SOME 3)),
                info_event          = ref(Meta (KeyPress "I")),
(* does not work for strange Tcl reasons . . .
                show_event          = ref(Meta (KeyPress "S")),
*)
                show_event          = ref(ButtonPress(SOME 3)),
                find_event          = ref(Meta (KeyPress "F")),
                help_event          = ref(Meta (KeyPress "H")),
                update_event        = ref(Meta (KeyPress "U")),
                new_event           = ref(Meta (KeyPress "N")),
                delete_event        = ref(Meta (KeyPress "D")),
                copy_event          = ref(Meta (KeyPress "C")),
                paste_event         = ref(Meta (KeyPress "V")),

                drag_event          = ref(ButtonPress (SOME 1)),
                drop_event          = ref(ButtonRelease (SOME 1)),
                dd_motion_event     = ref(ModButton(1, Motion)),
                dd_enter_event      = ref(ModButton(1, Enter)),
                dd_leave_event      = ref(ModButton(1, Leave)),
                
                duplicate_event     = ref(Meta (KeyPress "D")),
                confirm_event       = ref(KeyPress "Y"),
                cancel_event        = ref(KeyPress "N"),
                quit_event          = ref(Meta (KeyPress "Q")),
                abort_event         = ref(Meta (KeyPress ".")),
                kill_event          = ref(Meta (KeyPress ".")),
                close_event         = ref(KeyPress "Q")}

        fun sel_elem_event      () = !(#sel_elem_event Config)
        fun sel_group_elem_event() = !(#sel_group_elem_event Config)
        fun sel_range_event     () = !(#sel_range_event Config)
        fun sel_all             () = !(#sel_all Config)
        fun activate_event      () = !(#activate_event Config)
        fun info_event          () = !(#info_event Config)
        fun show_event          () = !(#show_event Config)
        fun find_event          () = !(#find_event Config)
        fun help_event          () = !(#help_event Config)
        fun update_event        () = !(#update_event Config)
        fun new_event           () = !(#new_event Config)
        fun delete_event        () = !(#delete_event Config)
        fun copy_event          () = !(#copy_event Config)
        fun paste_event         () = !(#paste_event Config)

        fun drag_event          () = !(#drag_event Config)
        fun drop_event          () = !(#drop_event Config)
        fun dd_motion_event     () = !(#dd_motion_event Config)
        fun dd_leave_event      () = !(#dd_leave_event Config)
        fun dd_enter_event      () = !(#dd_enter_event Config)
	fun object_menu_event   () = !(#object_menu_event Config)
    
        fun duplicate_event     () = !(#duplicate_event Config)
        fun confirm_event       () = !(#confirm_event Config)
        fun cancel_event        () = !(#cancel_event Config)
        fun quit_event          () = !(#quit_event Config)
        fun abort_event         () = !(#abort_event Config)
        fun kill_event          () = !(#kill_event Config)
        fun close_event         () = !(#close_event Config)

        fun init () = ()
    end;

    structure Colors : COLORS_SIG = 
    struct
        val Config = 
               {background = ref (Mix{red=200, blue=240, green=240}),
                foreground = ref(Black),
                background_sel = ref(Mix{red=300, blue=150, green=300}),
                foreground_sel = ref(Black),
                background_act = ref(Red),
                foreground_act = ref(Black)
               }

        fun init () = ()
    end

    structure Matcher = 
    struct
        val Config = {matcher = ref String.isPrefix}
    end

    structure LocalConfigMgr =
    struct
        type object      = exn;
        type object_id   = int * string

        val id_ctr = ref(0);
        fun mk_object_id s = (!id_ctr,s)
        fun get_string_id (oid:object_id) = #2 oid
        fun compare (x:object_id,y:object_id) = Int.compare(#1 x, #1 y)

        structure OM   = BinaryMapFn (struct
                                        type ord_key = object_id
                                        val  compare = compare
                                      end);

        val config_tab = ref(OM.empty : object OM.map);

        fun id_kinds ()= List.map (fn((x,y),_) => y)
                                  (OM.listItemsi (!config_tab));
        fun get_data oid = OM.find(!config_tab,oid);
        fun put_data (oid,obj) = (config_tab:=OM.insert(!config_tab,oid,obj));
    end

end;

