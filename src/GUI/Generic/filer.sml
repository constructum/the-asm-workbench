(* ***************************************************************************
 
   $Source: /home/uniform/rep/sml/sml_tk/toolkit/filer.sml,v $
 
   Generic file and directory selection.
 
   $Date: 1997/01/14 09:57:06 $
   $Revision: 1.15.2.1 $
   Authors: bu, kol (Last modification $Author: litfox $)

   (C) 1996, Bremen Institute for Safe Systems (BISS), Universitaet Bremen

  **************************************************************************

   Simple Filer component taken from an older version of sml_tk,
   ported to sml_tk 3.0 by G. Del Castillo with some simplifications
   and modifications
 
  ************************************************************************** *)

signature FILER_SIG =
sig
    (* stand alone versions: *)
    val file_select : unit -> string * string 
    val dir_select  : unit -> string  (* !!! to be debugged*)
    (* system versions: *)
    val enter_file : (string * string -> unit) -> unit
    val enter_file_with_filter : string -> (string * string -> unit) -> unit
    val enter_dir  : (string -> unit) -> unit    (* !!! to be debugged*)
end

(* ************************************************************************* *)

(* Filer with a generic regular expression matcher *)

(* There are more readily usable instantiations of the filer below,
 * near the end of this file. *)

functor RegExFiler
    (structure M : 
	 sig val filter_files : string -> string -> bool;
	     exception bad_regexp
	 end) (*: FILER_SIG*) =
struct
fun ls dir = 
    let val dirStr = OS.FileSys.openDir dir;
	fun loopDir dirLst = 
	    let val result = OS.FileSys.readDir dirStr
	    in case result of
		   NONE => dirLst
		 | SOME str => loopDir dirLst@[str]
	    end
	val dirlst = loopDir [];
	val _ = OS.FileSys.closeDir dirStr;
    in dirlst
    end

local open SmlTk SmlTk21

      fun errs_occurred1 nm ()= StdDialog.error_dialog nm noAction
						 
      val direntry_widId = mkWidgetId "direntry"
      val filterentry_widId = mkWidgetId "filterentry"
      val fileentry_widId = mkWidgetId "fileentry"
      val dirliste_widId = mkWidgetId "dirliste"
      val fileliste_widId = mkWidgetId "fileliste"

      val filerMain_winId = mkWinId "filermain"
      val fileSel_winId = mkWinId "filesel"
      val dirSel_winId = mkWinId "dirsel"
				       
      fun sort (less: 'a*'a -> bool) =
	  let fun insert (x, []) = [x]
		| insert (x, y::ys) =
		  if less(y, x) then y :: insert (x, ys) else x::y::ys;
	      fun sort1 [] = []
		| sort1 (x::xs) = insert (x, sort1 xs)
	  in  sort1  end;
      
in

val filter = List.filter
	       
val debugging = false

fun cprint (s :string) =
  if debugging then print s else ()

fun mcd f s =
( cprint ("[" ^ f ^ "]: cd '" ^ s ^ "'\n");
  OS.FileSys.chDir s )

val CUR_DIR = ref(".");
val EXT_CUR_DIR = ref(".");
val CUR_FILE_FILTER = ref("*");
val SEL_FILE_DIR = ref("");
val SEL_FILE_FIL = ref("");
val DIRS = ref([]:string list); 
val FILES = ref([]:string list); 


fun filter_pair P [] = ([],[])
   |filter_pair P (a::R) = 
	let val (b,c) = filter_pair P R
        in  if P a then (a::b, c) else (b,a::c) end;

fun showDirAndFiles () = 
let val _ = cprint "[showDirAndFiles]\n"
    val (dirs,files) = filter_pair OS.FileSys.isDir (ls (!CUR_DIR))
in  	clearText direntry_widId;    
	insertTextEnd direntry_widId (!CUR_DIR);
	clearText filterentry_widId;   
	insertTextEnd filterentry_widId (!CUR_FILE_FILTER);
	clearText fileentry_widId;   
	insertTextEnd fileentry_widId (!SEL_FILE_FIL);
        DIRS := (sort (op <=) dirs);
        DIRS := (if OS.FileSys.getDir() <> "/" then ".." :: (!DIRS) else (!DIRS));
	FILES:= let val all = if size(!CUR_FILE_FILTER)= 0 then files
			      else ((filter (M.filter_files (!CUR_FILE_FILTER))
				     files) 
				     handle bad_regexp=>
					(CUR_FILE_FILTER:= "";
					 errs_occurred1
					 "Bad regular expression"();
					 files)
				    )
		in (sort (op <=) all)
		end;
	clearText dirliste_widId;
	app (insertTextEnd dirliste_widId) (!DIRS); 
	clearText fileliste_widId;
	app (insertTextEnd fileliste_widId) (!FILES) 
	(* functional programming what's it like eh? *)
end;

fun showDir () = 
let val _ = cprint "[showDir]\n"
    val (dirs,files) = filter_pair OS.FileSys.isDir (ls (!CUR_DIR))
in  	clearText direntry_widId;   
	insertTextEnd direntry_widId (!CUR_DIR);
        DIRS := dirs;
	clearText dirliste_widId;
	app (insertTextEnd dirliste_widId) (!DIRS) 
end;


fun build_dir_list win sw = 
   let val _ = cprint "[build_dir_list]\n"
       val cd = mcd "build_dir_list"
       fun cmd (_:TkEvent) = 
       let val Mark(n,_) = readCursor dirliste_widId
           val _ = cprint("FILER n= "^(Int.toString n)^"\n");
	   val newdir = List.nth(!DIRS,n)
       in  (cd newdir;  (* Provisory; may fail *)
	    CUR_DIR:= OS.FileSys.getDir(); 
	    if sw then () else SEL_FILE_DIR:= (!CUR_DIR);
		if sw then showDirAndFiles() else showDir ())
	   handle NotDirectory => 
	       errs_occurred1 "Permission denied" ()
       end;
       fun cmd1 (e:TkEvent)= (cmd(e);cd (!EXT_CUR_DIR); closeWindow win)
   in  Listbox(dirliste_widId,RightScb,
			(if sw then [Side Left] else []) 
				@ [Fill Both, Expand true],
			[Relief Raised,Height 14, Width 25],		
			[BindEv(ButtonPress (SOME 1),cmd)])
   end;

fun build_file_list win act_on_file = 
   let val _ = cprint "[build_file_list]\n"
       val cd = mcd "build_file_list"
       fun cmd (_:TkEvent) = 
       let val Mark(n,_) = readCursor fileliste_widId
	    val _ = cprint("FILER n= "^(Int.toString n)^"\n");
	    val file = List.nth(!FILES,n)
       in  SEL_FILE_DIR := (!CUR_DIR);
	   SEL_FILE_FIL := file;
	   clearText fileentry_widId;
	   insertTextEnd fileentry_widId (file)
       end
       fun cmd1 e = ( cmd(e); cd (!EXT_CUR_DIR); closeWindow win;
		      act_on_file(!SEL_FILE_DIR, !SEL_FILE_FIL) )
   in Listbox( fileliste_widId, RightScb,
	       [ Side Right, Fill Both, Expand true ],
	       [ Relief Raised,Height 14, Width 50 ],		
	       [ BindEv(ButtonPress (SOME 1),cmd),
		 BindEv(KeyPress "Up", cmd),
		 BindEv(KeyPress "Down", cmd),
		 BindEv(KeyPress "Return", cmd1),
		 BindEv(KeyPress "Escape", fn _ => closeWindow win),
		 BindEv(Double (ButtonPress (SOME 1)), cmd1) ] )
   end

fun boxblock win act_on_file = 
  ( cprint "[showDirAndFile]\n";
    Frame(newWidgetId(),[build_dir_list win true,build_file_list win act_on_file ],
			[Fill Both, Expand true],[],[]) )



fun quit_but win act_on_file = 
    let val _ = cprint "[quit_but]\n"
	val cd = mcd "quit_but"
	fun cmd () = (cd (!EXT_CUR_DIR);
		      closeWindow win;
		      act_on_file(!SEL_FILE_DIR, !SEL_FILE_FIL))
    in  Button(newWidgetId(),[Fill X,Side Right,Expand true],
				[Text"OK",Command cmd],[]) 
    end;

fun canc_but win = 
    let val _ = cprint "[canc_but]\n"
	val cd = mcd "canc_but"
	fun cmd () = (cd (!EXT_CUR_DIR);
			SEL_FILE_DIR := "";
			SEL_FILE_FIL := "";
			closeWindow win )
    in Button(newWidgetId(),[Fill X,Side Left,Expand true],
			[Text "Cancel", Command cmd],[]) end;

fun bot_line win act_on_file = 
  ( cprint "[bot_line]\n";
    Frame(newWidgetId(),[quit_but win act_on_file, canc_but win ],
			[Side Bottom, Fill X],[],[]) )


val directory_label = Label(newWidgetId(),[Fill X],[Text "Directory"],[]);
fun directory_entry sw = 
    let val _ = cprint "[directory_entry]\n"
	val cd = mcd "directory_entry"
	fun cmd (_:TkEvent) = 
	let val newdir = readTextAll direntry_widId
	in (cd newdir; CUR_DIR:= newdir;
	    if sw then showDirAndFiles() else showDir ())
	    handle NotDirectory => 
		errs_occurred1("Can not access directory: "^newdir)()
	end
    in  Entry(direntry_widId,[Fill X],[],[BindEv(KeyPress "Return",cmd)]) end;
val filter_label = Label(newWidgetId(),[Fill X],[Text "Filter"],[]);

val filter_entry = 
    let fun cmd (_:TkEvent) = (CUR_FILE_FILTER:= readTextAll filterentry_widId;
			       showDirAndFiles ())
    in Entry(filterentry_widId,[Fill X],[],[BindEv(KeyPress "Return",cmd)]) end;

val file_label = Label(newWidgetId(),[Fill X],[Text "File"],[]);
val file_entry = Entry(fileentry_widId,[Fill X],[], 
				[BindEv(KeyPress "Return",fn _ => ())]);



fun fileselect_win winId act_on_file =
let val _ = cprint "[fileselect_win]\n"
    fun init_filer_win () = 
      ( CUR_DIR:= OS.FileSys.getDir();
	EXT_CUR_DIR:=(!CUR_DIR);
	showDirAndFiles() )
in GUI_Misc.mkWin5
     ( winId, [ WinTitle "File Selection" ],
       [ directory_label, directory_entry true,
	 filter_label, filter_entry,
	 boxblock winId act_on_file,
	 file_label, file_entry,
	 bot_line winId act_on_file ],
       [ BindEv(KeyPress "Escape", fn _ => closeWindow winId) ],
       init_filer_win )
end

fun dirselect_win winId act_on_dir = 
let val _ = cprint "[dirselect_win]\n"
    fun  init_filer_win () = 
    (CUR_DIR:= OS.FileSys.getDir();
     EXT_CUR_DIR:=(!CUR_DIR);
     showDir())
in  GUI_Misc.mkWin4 ( winId, [ WinTitle "Directory Selection" ],
		      [ directory_label, directory_entry false,
			build_dir_list winId false,
			bot_line winId act_on_dir ],
		      init_filer_win )
end;


fun file_select ()= 
    ( SmlTk.init();
      cprint "[file_select]\n";
      SEL_FILE_DIR := "";
      SEL_FILE_FIL := "";
      startTclExn [fileselect_win filerMain_winId (fn (_,_) => ())];
      (!SEL_FILE_DIR, !SEL_FILE_FIL) );

fun dir_select () = 
    ( SmlTk.init();
      cprint "[dir_select]\n";
      SEL_FILE_DIR := "";
      SEL_FILE_FIL := "";
      startTcl [dirselect_win filerMain_winId (fn (_,_) => ())]
      handle WIDGET s => (print s; raise WIDGET s);
      (!SEL_FILE_DIR) );

      
fun enter_file act_on_file =
  ( cprint "[enter_file]\n";
    SEL_FILE_DIR := "";
    SEL_FILE_FIL := "";
    openWindow (fileselect_win fileSel_winId act_on_file) )

fun enter_file_with_filter filter act_on_file =
  ( cprint "[enter_file_with_filter]\n";
    CUR_FILE_FILTER := filter;
    SEL_FILE_DIR := "";
    SEL_FILE_FIL := "";
    openWindow (fileselect_win fileSel_winId act_on_file) )

fun enter_dir act_on_dir = 
    (cprint "[enter_dir]\n";
     SEL_FILE_DIR := "";
     SEL_FILE_FIL := "";
     openWindow (dirselect_win dirSel_winId (fn (a,_) => act_on_dir (a)) ));

end  

end


(* *********************************************************************** *)

structure Filer (*: FILER_SIG*) =
RegExFiler( structure M = 
    struct
      exception bad_regexp			   
      fun filter_files pattern filename =
        let fun F (c :: rest) = (if c = #"*" then ".*" else Char.toString c) ^ (F rest)
	      | F [] = ""									   fun wildcard_to_regexp patten = F (explode pattern)
	in Rex.match (wildcard_to_regexp pattern) filename
	   handle Rex.bad_expr => raise bad_regexp
	end
    end )
