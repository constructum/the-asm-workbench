signature GUISTATE = 
sig
    type GUI

    val getWindowsGUI : unit -> BasicTypes.Window list
    val getPathAssGUI : unit -> BasicTypes.PathAss list
    val getWindowGUI  : BasicTypes.WinId -> 
                        BasicTypes.WinId * BasicTypes.WinConfigure list * 
                        BasicTypes.Widgets * BasicTypes.Binding list *
                        BasicTypes.SimpleAction
    val updWindowGUI  : BasicTypes.WinId -> 
                        BasicTypes.WinId * BasicTypes.WinConfigure list * 
                        BasicTypes.Widgets * BasicTypes.Binding list *
                        BasicTypes.SimpleAction -> unit      
    val updWindowsGUI : BasicTypes.Window list -> unit
    val updPathAssGUI : BasicTypes.PathAss list -> unit
    val updGUI        : BasicTypes.Window list * BasicTypes.PathAss list->unit
    val isInitWin     : BasicTypes.WinId -> bool
    val initGuiState  : unit -> unit

end