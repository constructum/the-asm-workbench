(* ***************************************************************************

   $Source: /repository/sml/sml_tk/src/toolkit/tests+examples/filemanager.sml,v $

   A simple filemanager / GenGUI example

   $Date: 2001/03/30 13:40:01 $
   $Revision: 3.0 $

   Author: ludi (Last modification by $Author: 2cxl $)
 
   (C) 1999, Bremen Institute for Safe Systems, Universitaet Bremen

  ************************************************************************** *)


local
  open SmlTk OS.FileSys

  val statusLabelID = newWidgetId()

  val SHOW_HIDDEN       = ref false
  val SORT_NAMES        = ref true
  val CURRENT_DIRECTORY = ref ""

  fun ko (SOME x) = x
    | ko NONE     = ""
in

  structure FileManagerAppl : SIMPLE_APPL_SIG =

  struct

    (* Declarations *)

    datatype objtype = folder | file

    datatype mode = none

    datatype object = foldobj of string ref
                    | fileobj of string ref

    type new_object = object * SmlTk.Coord * SmlTk.AnchorKind

    fun obj_type (foldobj _) = folder
      | obj_type (fileobj _) = file

    fun objlist_type ls =
    let
      fun all_of_type (x::xs) y = if obj_type x = y then all_of_type xs y
                                                    else false
        | all_of_type [] _      = true
    in
      if all_of_type ls folder then SOME folder
      else if all_of_type ls file then SOME file
           else NONE
    end

    fun is_constructed folder = true
      | is_constructed file   = true

    fun sel_name (foldobj name) = SOME(!name)
      | sel_name (fileobj name) = SOME(!name)

    fun modes folder = []
      | modes file   = []

    fun mode_name none = ""

    fun sel_mode (foldobj _) = none
      | sel_mode (fileobj _) = none

    fun set_mode (foldobj _, _) = ()
      | set_mode (fileobj _, _) = ()

    fun icon (folder, none) = 
	Icons.getIcon(getLibPath()^"/tests+examples/filemanager", "fold.gif")
      | icon (file,   none) = 
	Icons.getIcon(getLibPath()^"/tests+examples/filemanager", "file.gif")

    fun outline ob = false


    (* Read directory *)

    fun hidden x       = (hd(explode x) = #".")

    fun files_to_list p =
    let
      fun bSort []       = []
        | bSort l        =
      let
        fun bubble l   =
          case l of
            nil       => nil
          | [x]       => [x]
          | (x::y::r) => if String.>(x,y) then y::bubble(x::r)
                         else x::bubble(y::r)
        fun bSorth l i =
          case i of
            0 => l
          | n => bSorth (bubble l) (n-1)
      in
        bSorth l (length l - 1)
      end

      val dirstream      = openDir (!CURRENT_DIRECTORY)

      fun makelist _ ""  = []
        | makelist p x   = if hidden x andalso not(!SHOW_HIDDEN)
                           then makelist p (readDir dirstream)
                           else if p x handle NoAcc => false
                                then x :: makelist p (readDir dirstream)
                                else makelist p (readDir dirstream)
      fun dirlist p      = if (!SORT_NAMES)
                           then bSort(makelist p (readDir dirstream))
                                before closeDir dirstream
                           else makelist p (readDir dirstream)
                                before closeDir dirstream
    in
      dirlist p
    end

    fun content (x::xs) y (xp,yp) t =
        (foldobj (ref x), (xp,yp), NorthWest)
      :: (if t<7 then content xs y (xp+70,yp) (t+1)
                 else content xs y (20,yp+70) 0)
      | content [] (x::xs) (xp,yp) t =
         (fileobj (ref x), (xp,yp), NorthWest)
      :: (if t<7 then content [] xs (xp+70,yp) (t+1)
                 else content [] xs (20,yp+70) 0)
      | content [] [] _ _ = []

    fun init () = content (files_to_list (isDir))
                          (files_to_list (not o isDir)) (20,15) 0


    (* Standard operation "Show info" *)

    local
      val DIR_TEMP          = ref ""
      val HID_TEMP          = ref false

      fun permissions x =
      let
        fun read x = if access(x, [A_READ]) then "Yes"
                     else "No"

        fun write x = if access(x, [A_WRITE]) then "Yes"
                      else "No"

        fun exec x = if access(x, [A_EXEC]) then "Yes"
                     else "No"
      in
        "   Read: " ^ read x ^
        "        Write: " ^ write x ^
        "        Exec: " ^ exec x ^ "   \n"
        handle NoPermission => "?"
      end
    in
      fun fileInfo (foldobj name) =
      let
        val infoWinID = newWinId()

        fun dummy _ = true

        fun prepare x =
          (chDir x;
           DIR_TEMP := !CURRENT_DIRECTORY;
           CURRENT_DIRECTORY := getDir();
           HID_TEMP := !SHOW_HIDDEN;
           SHOW_HIDDEN := true)

        fun after _ =
          (CURRENT_DIRECTORY := !DIR_TEMP;
           chDir(!CURRENT_DIRECTORY);
           SHOW_HIDDEN := !HID_TEMP)

        fun countEntries x =
          (prepare x;
           Int.toString(List.length(files_to_list dummy))
           before after())

        fun countFiles x =
          (prepare x;
           Int.toString(List.length(files_to_list (not o isDir)))
           before after())

        fun countFolders x =
          (prepare x;
           Int.toString(List.length(files_to_list isDir))
           before after())

        fun countHidden x =
          (prepare x;
           Int.toString(List.length(files_to_list hidden))
           before after())

        val labelText = 
          if access(!name, [A_READ, A_EXEC])
          then countEntries (!name) ^ " Entries (" ^
               countHidden (!name) ^ " hidden):" ^
               "\n\nSubfolders: " ^ countFolders (!name) ^
               "        Files: " ^ countFiles (!name) ^
               "\n\n\nPermissions:\n\n" ^ permissions (!name)
          else "Permissions:\n\n" ^ permissions (!name) ^
               "\n\nNo read/exec permission!\n" ^
               "No other information availlable.\n"

        val icon =
          CIcon {citemId  = newCItemId(),
                 coord    = (10,8),
                 iconkind = FileImage(getLibPath() ^
                                      "/icons/filemanager/foldIcon.gif",
                                      newImageId()),
                 configs  = [Anchor NorthWest],
                 bindings = []}

        val infoLabel =
          Frame {widId    = newWidgetId(),
                 widgets  =
                 Pack
                   [Frame {widId    = newWidgetId(),
                           widgets  =
                           Pack
                             [Canvas {widId      = newWidgetId(),
                                      scrolltype = NoneScb,
                                      citems     = [icon],
                                      packings   = [Side Left],
                                      configs    = [Width 45, Height 35],
                                      bindings   = []},
                              Label {widId    = newWidgetId(),
                                     packings = [Side Left],
                                     configs  = [Text("Directory: " ^ !name)],
                                     bindings = []}],
                           packings = [Side Top],
                           configs  = [],
                           bindings = []},
                    Label {widId    = newWidgetId(),
                           packings = [Side Bottom],
                           configs  = [Text labelText],
                           bindings = []}],
                 packings = [],
                 configs  = [],
                 bindings = []}

        fun close _ = closeWindow infoWinID

        val closeButton =
          Button {widId    = newWidgetId(),
                  packings = [],
                  configs  = [Text "Close", Command close],
                  bindings = []}

        val infoWindow =
          mkWindow {winId    = infoWinID,
                    config   = [WinTitle("Folder info: " ^ !name)],
                    widgets  = Pack [infoLabel, closeButton],
		    bindings = [],
                    init     = noAction}
      in
        (addConf statusLabelID [Text ""];
         openWindow infoWindow)
      end
        | fileInfo (fileobj name) =
      let
        val infoWinID = newWinId()

        val icon =
          CIcon {citemId  = newCItemId(),
                 coord    = (10,8),
                 iconkind = FileImage(getLibPath() ^
                                      "/icons/filemanager/fileIcon.gif",
                                      newImageId()),
                 configs  = [Anchor NorthWest],
                 bindings = []}

        val labelText = "\nFilename: " ^ !name ^
                        "\n\n\nLength: " ^
                        Int.toString(fileSize (!name)) ^
                        " bytes\n\n\nPermissions:\n\n" ^
                        permissions (!name)

        val infoLabel =
          Frame {widId    = newWidgetId(),
                 widgets  =
                 Pack
                   [Frame {widId    = newWidgetId(),
                           widgets  =
                           Pack
                             [Canvas {widId      = newWidgetId(),
                                      scrolltype = NoneScb,
                                      citems     = [icon],
                                      packings   = [Side Left],
                                      configs    = [Width 45, Height 35],
                                      bindings   = []},
                              Label {widId    = newWidgetId(),
                                     packings = [Side Left],
                                     configs  = [Text("Filename: " ^ !name)],
                                     bindings = []}],
                           packings = [Side Top],
                           configs  = [],
                           bindings = []},
                    Label {widId    = newWidgetId(),
                           packings = [Side Bottom],
                           configs  = [Text labelText],
                           bindings = []}],
                 packings = [],
                 configs  = [],
                 bindings = []}

        fun close _ = closeWindow infoWinID

        val closeButton =
          Button {widId    = newWidgetId(),
                  packings = [],
                  configs  = [Text "Close", Command close],
                  bindings = []}

        val infoWindow =
          mkWindow {winId    = infoWinID,
                    config   = [WinTitle("File information: " ^ !name)],
                    widgets  = Pack [infoLabel, closeButton],
		    bindings = [],
                    init     = noAction}
      in
        (addConf statusLabelID [Text ""];
         openWindow infoWindow)
      end
    end

    val std_ops = [ (fileInfo, "File info") ]


    (* Rename objects (and files!) *)

    fun rename ob cc =
    let
      val nameWinID = newWinId()
      val nameEntryID = newWidgetId()

      fun name (fileobj nm) = !nm
        | name (foldobj nm) = !nm

      fun set (fileobj nm) =
        if   OS.Process.system("mv " ^ !nm ^ " " ^ readTextAll nameEntryID)
           = OS.Process.success
        then nm := readTextAll nameEntryID
        else if access(!nm, [A_WRITE])
             then addConf statusLabelID [Foreground Red,
                                         Text "Rename: Permission denied!"]
             else addConf statusLabelID
                          [Foreground Red,
                           Text "Rename failed: Illegal filename entered!"]
        | set (foldobj nm) =
        if   OS.Process.system("mv " ^ !nm ^ " " ^ readTextAll nameEntryID)
           = OS.Process.success
        then nm := readTextAll nameEntryID
        else if access(!nm, [A_WRITE])
             then addConf statusLabelID [Foreground Red,
                                         Text "Rename: Permission denied!"]
             else addConf statusLabelID
                          [Foreground Red,
                           Text "Rename failed: Illegal filename entered!"]

      fun ok _ = (set ob;
                  closeWindow nameWinID;
                  cc (name ob))

      val nameEntry =
        Frame {widId    = newWidgetId(),
               widgets  =
                     Pack [Label {widId    = newWidgetId(),
                                  packings = [Side Left],
                                  configs  = [Text "New name:"],
                                  bindings = []},
                           Entry {widId    = nameEntryID,
                                  packings = [Side Left],
                                  configs  = [Background White, Width 30],
                                  bindings = [BindEv(KeyPress "Return", ok)]}],
               packings = [PadX 15, PadY 25],
               configs  = [],
               bindings = []}

      fun canc _ = (closeWindow nameWinID;
                    cc (name ob))

      val buttons =
        Frame {widId    = newWidgetId(),
               widgets  =
                     Pack [Button {widId    = newWidgetId(),
                                   packings = [Side Left,PadX 5],
                                   configs  = [Text "Ok", Command ok,
                                               Width 15],
                                   bindings =  []},
                           Button {widId    = newWidgetId(),
                                   packings = [Side Left, PadX 5],
                                   configs  = [Text "Cancel", Command canc,
                                               Width 15],
                                   bindings = []}],
               packings = [Side Bottom],
               configs  = [],
               bindings = []}

      fun init _ = insertTextEnd nameEntryID (name ob)

      val nameWin =
        mkWindow {winId    = nameWinID,
                  config   = [WinTitle("Enter name: " ^ name ob)],
                  widgets  = Pack [nameEntry, buttons],
		  bindings = [],
                  init     = init}
    in
      (addConf statusLabelID [Text ""];
       if access(name ob, [A_WRITE])
       then openWindow nameWin
       else addConf statusLabelID [Foreground Red,
                                   Text "Rename: Permission denied!"])
    end


    (* Delete objects (and files!) *)

    fun delete ob =
      (addConf statusLabelID [Text ""];
       remove(ko(sel_name ob))
       handle NoAcc => addConf statusLabelID
                               [Foreground Red,
                                Text("Permission denied!\nDeleted the " ^
                                     "object, but not the file!")])

    fun mon_ops _ = []


    (* Copy files to folders *)

    fun copy fold (f::files) =
      (if   OS.Process.system("cp " ^ ko(sel_name(f)) ^ " " ^ fold)
          = OS.Process.success
       then ()
       else addConf statusLabelID [Foreground Red,
                                   Text "Copy: Permission denied!"];
       copy fold files)
      | copy _ []            = ()

    fun copyFile (ob1, coord, oblist, operation) =
      (addConf statusLabelID [Text ""];
       copy (ko(sel_name ob1)) oblist)

    fun bin_ops (folder, file) = SOME copyFile
      | bin_ops _              = NONE


    (* Construction area: Permissions etc. *)

    datatype ca = none

    fun area_ops _ _ _ = ()

    local
      val top =
        Frame {widId    = newWidgetId(),
               widgets  =
                     Pack [Label {widId    = newWidgetId(),
                                  packings = [Side Left, PadX 10],
                                  configs  = [Text "User", Width 20,
                                              Background White],
                                  bindings = []},
                           Label {widId    = newWidgetId(),
                                  packings = [Side Left, PadX 10],
                                  configs  = [Text "Group", Width 20,
                                              Background White],
                                  bindings = []},
                           Label {widId    = newWidgetId(),
                                  packings = [Side Left, PadX 10],
                                  configs  = [Text "Others", Width 20,
                                              Background White],
                                  bindings = []}],
               packings = [PadY 10],
               configs  = [],
               bindings = []}

      val bframe1 =
        Frame {widId    = newWidgetId(),
               widgets  =
                     Pack [Checkbutton {widId    = newWidgetId(),
                                        packings = [Side Left],
                                        configs  = [Text "Read", Width 20,
                                                    Variable "ruser"],
                                        bindings = []},
                           Checkbutton {widId    = newWidgetId(),
                                        packings = [Side Left],
                                        configs  = [Text "Read", Width 20,
                                                    Variable "rgroup"],
                                        bindings = []},
                           Checkbutton {widId    = newWidgetId(),
                                        packings = [Side Left],
                                        configs  = [Text "Read", Width 20,
                                                    Variable "rothers"],
                                        bindings = []}],
               packings = [],
               configs  = [],
               bindings = []}

      val bframe2 =
        Frame {widId    = newWidgetId(),
               widgets  =
                     Pack [Checkbutton {widId    = newWidgetId(),
                                        packings = [Side Left],
                                        configs  = [Text "Write", Width 20,
                                                    Variable "wuser"],
                                        bindings = []},
                           Checkbutton {widId    = newWidgetId(),
                                        packings = [Side Left],
                                        configs  = [Text "Write", Width 20,
                                                    Variable "wgroup"],
                                        bindings = []},
                           Checkbutton {widId    = newWidgetId(),
                                        packings = [Side Left],
                                        configs  = [Text "Write", Width 20,
                                                    Variable "wothers"],
                                        bindings = []}],
               packings = [],
               configs  = [],
               bindings = []}

      val bframe3 =
        Frame {widId    = newWidgetId(),
               widgets  =
                     Pack [Checkbutton {widId    = newWidgetId(),
                                        packings = [Side Left],
                                        configs  = [Text "Exec", Width 20,
                                                    Variable "xuser"],
                                        bindings = []},
                           Checkbutton {widId    = newWidgetId(),
                                        packings = [Side Left],
                                        configs  = [Text "Exec", Width 20,
                                                    Variable "xgroup"],
                                        bindings = []},
                           Checkbutton {widId    = newWidgetId(),
                                        packings = [Side Left],
                                        configs  = [Text "Exec", Width 20,
                                                    Variable "xothers"],
                                        bindings = []}],
               packings = [],
               configs  = [],
               bindings = []}

      val permWidget =
        Frame {widId    = newWidgetId(),
               widgets  = Pack [top, bframe1, bframe2, bframe3],
               packings = [],
               configs  = [],
               bindings = []}

      val nameEntryID  = newWidgetId()
      val userLabelID  = newWidgetId()
      val groupLabelID = newWidgetId()

      val dnameEntry =
        Frame {widId    = newWidgetId(),
               widgets  =
                     Pack [Label {widId    = newWidgetId(),
                                  packings = [Side Left],
                                  configs  = [Text "Dirname: ", Width 15],
                                  bindings = []},
                           Entry {widId    = nameEntryID,
                                  packings = [Side Left],
                                  configs  = [Background White, Width 30],
                                  bindings = []}],
               packings = [PadY 3],
               configs  = [],
               bindings = []}

      val fnameEntry =
        Frame {widId    = newWidgetId(),
               widgets  =
                     Pack [Label {widId    = newWidgetId(),
                                  packings = [Side Left],
                                  configs  = [Text "Filename: ", Width 15],
                                  bindings = []},
                           Entry {widId    = nameEntryID,
                                  packings = [Side Left],
                                  configs  = [Background White, Width 30],
                                  bindings = []}],
               packings = [PadY 3],
               configs  = [],
               bindings = []}

      val userLabel =
        Frame {widId    = newWidgetId(),
               widgets  =
                     Pack [Label {widId    = newWidgetId(),
                                  packings = [Side Left],
                                  configs  = [Text "User: ", Width 15],
                                  bindings = []},
                           Label {widId    = userLabelID,
                                  packings = [Side Left],
                                  configs  = [Relief Sunken, Width 30],
                                  bindings = []}],
               packings = [PadY 3],
               configs  = [],
               bindings = []}

      val groupLabel =
        Frame {widId    = newWidgetId(),
               widgets  =
                     Pack [Label {widId    = newWidgetId(),
                                  packings = [Side Left],
                                  configs  = [Text "Group: ", Width 15],
                                  bindings = []},
                           Label {widId    = groupLabelID,
                                  packings = [Side Left],
                                  configs  = [Relief Sunken, Width 30],
                                  bindings = []}],
               packings = [PadY 3],
               configs  = [],
               bindings = []}

      fun permSet(perm, nm) =
        Posix.FileSys.S.allSet
          (perm, Posix.FileSys.ST.mode(Posix.FileSys.stat nm))

      fun init nm id1 id2 id3 _ =
      let
        val st = Posix.FileSys.stat nm

        val user =
          Posix.SysDB.Passwd.name
            (Posix.SysDB.getpwuid(Posix.FileSys.ST.uid st))

        val group =
          Posix.SysDB.Group.name
            (Posix.SysDB.getgrgid(Posix.FileSys.ST.gid st))
      in
        (addConf statusLabelID [Text ""];
         insertTextEnd id1 nm;
         addConf id2 [Text user];
         addConf id3 [Text group];
         if permSet(Posix.FileSys.S.irusr, nm)
         then setVarValue "ruser" "1"
         else setVarValue "ruser" "0";
         if permSet(Posix.FileSys.S.iwusr, nm)
         then setVarValue "wuser" "1"
         else setVarValue "wuser" "0";
         if permSet(Posix.FileSys.S.ixusr, nm)
         then setVarValue "xuser" "1"
         else setVarValue "xuser" "0";
         if permSet(Posix.FileSys.S.irgrp, nm)
         then setVarValue "rgroup" "1"
         else setVarValue "rgroup" "0";
         if permSet(Posix.FileSys.S.iwgrp, nm)
         then setVarValue "wgroup" "1"
         else setVarValue "wgroup" "0";
         if permSet(Posix.FileSys.S.ixgrp, nm)
         then setVarValue "xgroup" "1"
         else setVarValue "xgroup" "0";
         if permSet(Posix.FileSys.S.iroth, nm)
         then setVarValue "rothers" "1"
         else setVarValue "rothers" "0";
         if permSet(Posix.FileSys.S.iwoth, nm)
         then setVarValue "wothers" "1"
         else setVarValue "wothers" "0";
         if permSet(Posix.FileSys.S.ixoth, nm)
         then setVarValue "xothers" "1"
         else setVarValue "xothers" "0")
      end

      fun mode(var, md) = if readVarValue var = "1" then [md] else nil

      fun modes _ =
        List.concat(   mode("ruser", Posix.FileSys.S.irusr)
                    :: mode("wuser", Posix.FileSys.S.iwusr)
                    :: mode("xuser", Posix.FileSys.S.ixusr)
                    :: mode("rgroup", Posix.FileSys.S.irgrp)
                    :: mode("wgroup", Posix.FileSys.S.iwgrp)
                    :: mode("xgroup", Posix.FileSys.S.ixgrp)
                    :: mode("rothers", Posix.FileSys.S.iroth)
                    :: mode("wothers", Posix.FileSys.S.iwoth)
                    :: mode("xothers", Posix.FileSys.S.ixoth) :: nil)

      fun permChanges nm =
        Posix.FileSys.chmod(nm, Posix.FileSys.S.flags(modes()))
        handle NoAcc => addConf statusLabelID
                                [Foreground Red,
                                 Text "Write changes: Permission denied!"]
    in
      fun area_open (win, fileobj nm, cc) =
      let
        fun canc cc _ = cc(fileobj nm)

        fun close cc _ =
          (if (readTextAll nameEntryID) = !nm
           then ()
           else (OS.Process.system("mv " ^ !nm ^ " " ^
                                   (readTextAll nameEntryID));
                 nm := readTextAll nameEntryID);
           permChanges (!nm);
           cc(fileobj nm))

        val buttons =
          Frame {widId    = newWidgetId(),
                 widgets  =
                       Pack [Button {widId    = newWidgetId(),
                                     packings = [Side Right],
                                     configs  = [Text "Write", Width 15,
                                                 Command(close cc)],
                                     bindings = []},
                             Button {widId    = newWidgetId(),
                                     packings = [Side Right],
                                     configs  = [Text "Cancel", Width 15,
                                                 Command(canc cc)],
                                     bindings = []}],
                 packings = [Side Bottom],
                 configs  = [],
                 bindings = []}
      in
        (none,
         [fnameEntry, userLabel, groupLabel, permWidget, buttons],
         init (!nm) nameEntryID userLabelID groupLabelID)
      end
        | area_open (win, foldobj nm, cc) =
      let
        fun canc cc _ = cc(foldobj nm)

        fun close cc _ =
          (if (readTextAll nameEntryID) = !nm
           then ()
           else (OS.Process.system("mv " ^ !nm ^ " " ^
                                   (readTextAll nameEntryID));
                 nm := readTextAll nameEntryID);
           permChanges (!nm);
           cc(foldobj nm))

        val buttons =
          Frame {widId    = newWidgetId(),
                 widgets  =
                       Pack [Button {widId    = newWidgetId(),
                                     packings = [Side Right],
                                     configs  = [Text "Write", Width 15,
                                                 Command(close cc)],
                                     bindings = []},
                             Button {widId    = newWidgetId(),
                                     packings = [Side Right],
                                     configs  = [Text "Cancel", Width 15,
                                                 Command(canc cc)],
                                     bindings = []}],
                 packings = [Side Bottom],
                 configs  = [],
                 bindings = []}
      in
        (none,
         [dnameEntry, userLabel, groupLabel, permWidget, buttons],
         init (!nm) nameEntryID userLabelID groupLabelID)
      end
    end

    val area_init = noAction

    type objectlist = object list

    structure CB = Clipboard (type obj = object list)


    (* Configuration *)

    structure Conf =
    struct
      val width          = 550
      val height         = 500
      val caWidth        = 580
      val caHeight       = 270
      val caXY           = NONE
      fun caTitle nm     = "Details of " ^ nm
      val iconNameWidth  = 50
      val iconNameFont   = SmlTk.SansSerif [SmlTk.Small]
      val background     = White
      val moveOpaque     = true
      val oneWindow      = false
      fun trashcanIcon() = Icons.getIcon(getLibPath() ^ "/icons",
                                         "trashcan.gif")
      val trashcanCoord  = (width - 100, height - 100)
      val delta          = 70
    end
  end


  structure FileManager :
    sig
      val go : unit -> unit
    end =

    struct
      structure SimpleGUI = GenGUI (structure appl = FileManagerAppl)

      val restart = ref (mkWindow {winId    = newWinId(),
                                   config   = [],
                                   widgets  = Pack [],
				   bindings = [],
                                   init     = noAction}) : Window ref

      val mainWin =
      let
        val mainWinID = newWinId()

        val statusLabel =
          Label {widId    = statusLabelID,
                 packings = [Side Top, PadY 20],
                 configs  = [Width 60, Height 2, Relief Sunken,
                             Text "Welcome!"],
                 bindings = []}

        val currentDirID = newWidgetId()

        val currentDir =
          Frame {widId    = newWidgetId(),
                 widgets  =
                       Pack [Label {widId    = newWidgetId(),
                                    packings = [Side Left],
                                    configs  = [Text "Current directory:  "],
                                    bindings = []},
                             Label {widId    = currentDirID,
                                    packings = [Side Left],
                                    configs  = [Relief Sunken, Width 50],
                                    bindings = []}],
                 packings = [PadY 20],
                 configs  = [],
                 bindings = []}

        fun quit _ = closeWindow mainWinID

        fun opt _ = (closeWindow mainWinID;
                     startTcl[(!restart)])

        val buttons =
          Frame {widId    = newWidgetId(),
                 widgets  =
                       Pack [Button {widId    = newWidgetId(),
                                     packings = [Side Right],
                                     configs  = [Text "Quit", Width 15,
                                                 Command quit],
                                     bindings = []},
                             Button {widId    = newWidgetId(),
                                     packings = [PadY 20, Side Right],
                                     configs  = [Text "Options", Width 15,
                                                 Command opt],
                                     bindings = []}],
                 packings = [Fill X],
                 configs  = [],
                 bindings = []}

        fun init _ = addConf currentDirID [Text (!CURRENT_DIRECTORY)]
      in
        mkWindow {winId    = mainWinID,
                  config   = [WinTitle "File management",
                              WinGeometry(SOME(620,710),NONE)],
                  widgets  = Pack [statusLabel, SimpleGUI.main_wid mainWinID,
                              currentDir, buttons],
		  bindings = [],
                  init     = fn() => (SimpleGUI.init
                                        (SimpleGUI.initial_state());
                                      init())}
      end

      val initWin =
      let
        val initWinID = newWinId()

        val dirEntryID = newWidgetId()
        val errLabelID = newWidgetId()

        fun ok _ =
          if access(readTextAll dirEntryID, [A_READ, A_EXEC])
          then (chDir (readTextAll dirEntryID)
                handle NotEx => ();
                CURRENT_DIRECTORY := getDir();
                if readVarValue "show" = "1"
                then SHOW_HIDDEN := true
                else SHOW_HIDDEN := false;
                if readVarValue "sort" = "1"
                then SORT_NAMES := true
                else SORT_NAMES := false;
                closeWindow initWinID;
                startTcl[mainWin])
          else if access(readTextAll dirEntryID, [])
               then addConf errLabelID [Foreground Red,
                                        Text "Permission denied!"]
               else addConf errLabelID [Foreground Red,
                                        Text "Folder does not exist!"]

        val dirEntry =
          Frame {widId    = newWidgetId(),
                 widgets  =
                       Pack [Label {widId    = newWidgetId(),
                                    packings = [Side Left],
                                    configs  = [Text "Directory:"],
                                    bindings = []},
                             Entry {widId    = dirEntryID,
                                    packings = [Side Left],
                                    configs  = [Background White],
                                    bindings = [BindEv(KeyPress "Return",
                                                       ok)]}],
                 packings = [PadY 12],
                 configs  = [],
                 bindings = []}

        val chkbuttons =
          Frame {widId    = newWidgetId(),
                 widgets  =
                       Pack [Checkbutton {widId    = newWidgetId(),
                                          packings = [Side Left],
                                          configs  = [Text "Sort files",
                                                      Variable "sort"],
                                          bindings = []},
                             Checkbutton {widId    = newWidgetId(),
                                          packings = [Side Left],
                                          configs  = [Text "Show hidden files",
                                                      Variable "show"],
                                          bindings = []}],
                 packings = [PadY 12],
                 configs  = [],
                 bindings = []}

        val errLabel =
          Label {widId    = errLabelID,
                 packings = [PadY 12],
                 configs  = [Text "Welcome!", Relief Sunken, Width 25],
                 bindings = []}

        fun quit _ = closeWindow initWinID

        val buttons =
          Frame {widId    = newWidgetId(),
                 widgets  =
                       Pack [Button {widId    = newWidgetId(),
                                     packings = [Side Left, PadX 5],
                                     configs  = [Text "Ok", Command ok,
                                                 Width 15],
                                     bindings = []},
                             Button {widId    = newWidgetId(),
                                     packings = [Side Left, PadX 5],
                                     configs  = [Text "Quit", Command quit,
                                                 Width 15],
                                     bindings = []}],
                 packings = [Side Bottom],
                 configs  = [],
                 bindings = []}

        fun init() =
          (insertTextEnd dirEntryID (!CURRENT_DIRECTORY);
           setVarValue "sort" "1";
           setVarValue "show" "0")
      in
        mkWindow {winId    = initWinID,
                  config   = [WinTitle "Options",
                              WinGeometry(SOME(300, 180), NONE)],
                  widgets  = Pack [dirEntry, chkbuttons, errLabel, buttons],
		  bindings = [],
                  init     = init}
      end

      fun go() =
        (restart:= initWin;
         if OS.Process.getEnv "HOME" = NONE
         then CURRENT_DIRECTORY := getDir()
         else CURRENT_DIRECTORY := ko(OS.Process.getEnv "HOME");
         startTcl[initWin])
  end
end
