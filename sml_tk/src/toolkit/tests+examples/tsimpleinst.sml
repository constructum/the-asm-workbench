(* ***************************************************************************

   $Source: /repository/sml/sml_tk/src/toolkit/tests+examples/tsimpleinst.sml,v $

   Test and example program for TGenGUI.

   This example only knows two object types, texts and numbers. Numbers have
   four different subtypes, corresponding to the four riders of the apocalypse,
   or rather the four basic arithmetic operations. 


   Texts can be concatened by dropping one onto the other, or they can
   be edited in the construction area. Numbers can added, subtracted
   etc. by dropping them onto each other. If you drag a number object into the 
   con area, a textual representation of the number is appended to the text
   currently under construction. 

   There is also the possibility to import a text by calling up the
   file browser and dragging one file into the construction system. 

   Use SimpleInst.go() to start. 

   $Date: 2001/03/30 13:40:06 $
   $Revision: 3.0 $

   Author: cxl&bu (Last modification by $Author: 2cxl $)
 
   (C) 2000, Bremen Institute for Safe Systems, Universitaet Bremen
             Albert-Ludwigs-Universität Freiburg

  ************************************************************************** *)
 


structure TSimpleInstAppl (* : APPL_SIG *) =

struct

  local open SmlTk BasicUtil in     

    (* Instantiating the utility windows *)

    (* We have text objects and numbers. Numbers have different modes,
     * namely plus, minus, times or div.
     *)

    datatype objtype0 = text | num
	
    datatype mode    = plusM | minusM | timesM | divM

    type objtype = objtype0 * mode option

    fun mode (_ ,m) = valOf m 

    type name = string ref

    fun mode_name plusM  = "Add me"
      | mode_name minusM = "Subtract me"
      | mode_name timesM = "Multiply me"
      | mode_name divM   = "Divide me"
      

    datatype object     = textobj of string* string ref
	                | number  of int* mode ref* string ref

    fun ord (textobj x, number y)  = LESS
       |ord (textobj (_,x), textobj (_,x')) = String.compare (!x,!x')
       |ord (number (_,_,x), number (_,_,x')) = String.compare (!x,!x')
       |ord (number x, textobj y)  = GREATER

    fun name_of (textobj (_,x)) = x
       |name_of (number (_,_,x)) = x

    fun rename s (textobj (_,x)) = (x:=s)
       |rename s (number (_,_,x)) = (x:=s)

    fun reset_name _ = ()

    fun string_of_name s t = !s

    fun obj_type (textobj _) = (text,NONE)
      | obj_type (number (_,m,_))  = (num,SOME (!m))

    fun modes (text,_) = []
      | modes (num,_)  = [plusM, minusM, timesM, divM]

    fun sel_mode (textobj _) = plusM  (* disnae matter what we return here *)
      | sel_mode (number(_, m, _))= !m

    fun set_mode (textobj _, _)= ()
      | set_mode (number(_, m, _), nu)=  m:= nu


    fun objlist_type ls = 
	let fun forall p = not o (List.exists (not o p))
	in
	    if forall (fn oo => fst(obj_type oo) = text) ls 
            then SOME(text,NONE)
	    else if forall (fn oo => fst(obj_type oo) = num) ls 
                 then SOME(num,SOME plusM)
		 else NONE
	end
    
    type objectlist = unit -> object list
    type cb_objects = objectlist
    fun  cb_objects_abs x = x
    fun  cb_objects_rep x = x

    type new_object  = object * (SmlTk.Coord * SmlTk.AnchorKind)

    fun is_constructed (text,_)  = true
      | is_constructed (num,_)   = false

    fun get_name (textobj(_ ,nm))    = !nm
      | get_name (number(_ , _, nm)) = !nm

    fun sel_name ob = SOME (get_name ob)

    fun label_action {obj,cc} =
	let fun set (textobj(_, nm)) nuname   = (nm:= nuname; cc nuname)
	      | set (number(_, _, nm)) nuname = (nm:= nuname; cc nuname)
	in  UW.enterLine{title="Renaming object", default="",
			 prompt="Please enter new name: ",
			 width= 20, cc=set obj}
	end

    val create_actions = []

    fun set_name (textobj(_, nm), nuname) = nm:= nuname
      | set_name (number(_, _, nm), nuname)  = nm:= nuname

    fun sel_text   (textobj (t,_))= t
    fun sel_number (number(m, _, _)) = m

    fun outline _ = false (* never outline *)

    fun icon (ot, m) =
	let
	    fun iconnm (text, _)     = "note.gif"
	      | iconnm (num, SOME plusM)  = "number.gif"
	      | iconnm (num, SOME minusM) = "nummin.gif"
	      | iconnm (num, SOME timesM) = "numtim.gif"
	      | iconnm (num, SOME divM)   = "numdiv.gif"
	in
	    Icons.getIcon(getLibPath()^"/tests+examples/icons", 
			  iconnm (ot, m))
	end
 
   (* Configuring GenGUI *)

   structure Conf = 
       struct
	   val width         = 500
	   val height        = 300
	   val caWidth       = 350
	   val caHeight      = 300
	   val caXY          = SOME (50, 470)
	   fun caTitle nm    = "Edit text: "^nm

	   val iconNameWidth = 60
           val iconNameFont  = SmlTk.SansSerif [SmlTk.Small]

	   val background    = Grey

	   val moveOpaque    = true
	       
	   val oneWindow     = true

	   fun trashcanIcon()= Icons.getIcon(getLibPath()^"/icons",
	                                        "trashcan.gif")
	   val trashcanCoord = (width-50, (height div 2)- 50)	 
	       
	   val delta         = 70

       end

    (* The standard operations: show & info *)

    fun show (textobj(tx, nm)) = 
	UW.display{title= !nm, width= 40, height= 20,
		   text= mkAT tx, cc= fn _ => ()}
      | show (number(n, _, nm)) =
	UW.display{title= !nm, width= 6, height= 3,
		   text= mkAT ("Value : "^(Int.toString n)),
		   cc= fn _ => ()}

    fun stat   (textobj(tx, nm)) = 
	let fun count p = List.length o (List.filter p)
	    val tc = explode tx
	    val nl = count StringUtil.isLinefeed tc
	    val nc = List.length tc
	    val nspc = count Char.isSpace tc
	    val na = ((count Char.isAlpha tc) * 100) div nc
	    val st = "\nNumber of lines : "^(Int.toString nl)^
	             "\nNumber of chars : "^(Int.toString nc)^
	       	     "\nNumber of spaces: "^(Int.toString nspc)^
		     "\nPercentage of alphanumerical char's: "^
		        (Int.toString na)^"\n"
	in  UW.display{title= "Statistics for "^(!nm),
		       width= 40, height= 20,
		       text= mkAT st, cc= fn _ => ()}
	end
      | stat  (number(n, _, nm))= 
	let val st= "The number has "^(Int.toString ((size (Int.toString n))-1))^
				       " digits.\n"
	in UW.display{title= "Statistics for "^(!nm),
		      width= 40, height= 20,
		      text= mkAT st, cc= fn _ => ()}
	end

 
    fun std_ops _  = [(show, "Show"), (stat, "Info")]


    fun delete _ = ()


    (* Initially appearing objects. *)

    fun init () =  (* oldfashioned initialization . . . *)
	[(number(2, ref plusM, ref "2"), ((10, 10), South)),
	 (number(4, ref plusM, ref "4"), ((10, 10), East)),
	 (number(5, ref plusM, ref "5"), ((10, 10), South)),
	 (textobj("Bring me my bow of burning gold!\n"^
		  "Bring me my arrows of desire!\n"^
	 	  "Bring me my spear! O clouds unfold!\n"^
		  "Bring me my chariot of fire!\n", 
	          ref "Jer'lem 1"), ((100, 10), Center)),
	 (textobj("I will not cease from mental fight\n"^
		  "Nor shall my sword sleep in my hand\n"^
		  "Till we have built Jerusalem\n"^
		  "In England's green and pleasant land\n",
		  ref "Jer'lem 2"), ((100, 10), South))]

    fun mon_ops _ = []

    (* For texts, there is just one binary operation: concatenation *)
    fun tconc (t1, wh, [], cc_newop) = cc_newop (t1, (wh, South))
      | tconc (t1, wh, t,  cc_newop) = 
	           cc_newop (textobj(StringUtil.concatWith "\n" 
				            (map sel_text (t1::t)),
			               ref  (StringUtil.concatWith " and " 
					    (map get_name (t1::t)))),
			     (wh, South))

    fun numop (number(n, m, _), wh, ls, cc_newop) =
	let fun appl_op [] = n
	      | appl_op ((number(n, m, _))::ns) = 
		case !m of plusM  => (appl_op ns)+n
		         | minusM => (appl_op ns)-n
		         | timesM => (appl_op ns)*n
		         | divM   => (appl_op ns) div n
	    val nunum = appl_op ls 
	in  cc_newop (number(nunum, m, ref (Int.toString nunum)), (wh, South))
	end
	

    fun bin_ops ((text,_), (text,_)) = SOME tconc
      | bin_ops ((num,_), (num,_))   = SOME numop
      | bin_ops (_, _)               = NONE

   (* 
    * The Construction Area.
    * 
    * The Construction Area essentially consists of a text widget
    * which can be used to edit the text. If another text is dragged
    * down from the manipulation area, it will appended at the end.
    *)

    fun  txId wsId = subWidId(wsId, "xTxEd")
	
    type ca = WidId
	   
    val concatCR = StringUtil.concatWith "\n" 

    fun area_ops (text,_) wid ls = 
	SmlTk.insertTextEnd (txId wid) (concatCR (map sel_text ls)) 
      | area_ops (num,_) wid ls =
        SmlTk.insertTextEnd (txId wid) (concatCR (map (Int.toString o sel_number) ls))



    fun area_open(win, textobj(tx, nm), cc) = 
	let val wsWId = newWidgetId()
	    val title = Label{widId=newWidgetId(), 
			      packings=[Side Top, Fill X], 
			      configs=[Relief Groove, Borderwidth 2,
				       Text (!nm)], bindings=[]}
	    val txwid = TextWid{widId= txId wsWId, scrolltype=RightScb, 
				annotext=mkAT tx, packings=[Fill Both], 
				configs=[], bindings=[]}
	    fun close txid cc nm () = 
		                cc (textobj (SmlTk.readTextAll txid, nm))
	    val quit  = Button{widId= newWidgetId(),
			       packings= [Side Right, Side Bottom],
			       configs= [Text "Close",
					 Command(close (txId wsWId) cc nm)
					 ], bindings= []}
	    val widgs  = [quit, txwid]			    
	in  (wsWId, if (Conf.oneWindow) then title::widgs
		    else widgs, K0)
	end
    
    val area_init = fn ()=> () (* no init necessary *)
	

   (* Communicating with the Filer:
    *
    * First, we need to instantiate the clipboard: 
    *)

   structure CB = Clipboard(type obj= unit-> object list)

   (* Instantiate the filer.
    * We need to provide it with a function to convert files to 
    * texts (fileToObj below); we'll do so by reading the file's contents
    * into the text of the object.
    *)
   structure Filer =
       Filer(structure Options = 
	     struct
               exception NoFile of string
               fun icons_path () = OS.Path.concat(SmlTk.getLibPath(),
                                                  "icons/filer")
	       val icons_size = (40, 10)
	       val default_pattern = NONE
               fun root() = NONE
               val default_filter = NONE
               structure Conf = FilerDefaultConfig
               structure CB= 
		   struct (* we have to insert a closure here *)
		       type obj= object list
		       fun  put obs ev cb = 
			   CB.put (fn ()=> obs) ev cb
		   end
	       val filetypes =
               let
                 fun fileToObj { dir  : string,
                                 file : string  } =
                 let
                   val filenm= "/"^OS.Path.joinDirFile {dir = dir, file = file}
                   val objnm = ref ("File: "^file)
                   val txt  =
                   let
                     fun read_file si = 
                       if TextIO.endOfStream si then ""
                       else (valOf (TextIO.inputLine si))^(read_file si)
                     val is  = TextIO.openIn filenm
                     val txt = read_file is
                     val _   = TextIO.closeIn is
                   in
                     txt
                   end
                   handle NoFile f => "NoFile: "^f
                 in
                   [textobj(txt, objnm)]
                 end
               in
                 [ { ext     = [""],
                     display = SOME { comment = "Default filetype",
                                      icon    = "unknown_Icon.gif",
                                      preview = NONE : ({ dir  : string,
                                                          file : string }
                                                        -> unit) option,
                                      file_to_obj = SOME fileToObj } } ]
               end
	     end) 
       end
end

structure TSimpleInst =
    (* sig val go: unit -> unit end  *)
struct

    local open SmlTk in

    structure TSimpleGUI = TGenGUI (structure appl= TSimpleInstAppl)

    val Result = ref(!TSimpleGUI.gui_state)

    local open TSimpleInstAppl TSimpleGUI.TreeObj in
      val init_objects = 
	[Folder((ref "texts",((120, 20), South)),
                [Content(textobj("I will not cease from mental fight\n"^
		              "Nor shall my sword sleep in my hand\n"^
		              "Till we have built Jerusalem\n"^
		              "In England's green and pleasant land\n",
		               ref "Jer'lem 2"), ((100, 10), South)),
                 Content(textobj("I will not cease from mental fight\n"^
		              "Nor shall my sword sleep in my hand\n"^
		              "Till we have built Jerusalem\n"^
		              "In England's green and pleasant land\n",
		               ref "Jer'lem 3"), ((100, 10), South))
                ]),
         Content(number(2, ref plusM, ref "2"), ((10, 10), South)),
	 Content(number(4, ref plusM, ref "4"), ((10, 10), East)),
	 Content(number(5, ref plusM, ref "5"), ((10, 10), South)),
	 Content(textobj("Bring me my bow of burning gold!\n"^
		  "Bring me my arrows of desire!\n"^
	 	  "Bring me my spear! O clouds unfold!\n"^
		  "Bring me my chariot of fire!\n", 
	          ref "Jer'lem 1"), ((100, 10), Center)),
	 Content(textobj("I will not cease from mental fight\n"^
		  "Nor shall my sword sleep in my hand\n"^
		  "Till we have built Jerusalem\n"^
		  "In England's green and pleasant land\n",
		  ref "Jer'lem 2"), ((100, 10), South))
        ]
    end (* local *)

    val init_guistate = (([],NONE),init_objects)

    fun quitButton win =
	let fun confirmQuit() = 
	    UW.confirm("Do you really want to quit?",
		       (fn()=> (Result := TSimpleGUI.state();
                                closeWindow win)))
	in  Button{widId= newWidgetId(),
		   packings= [Side Bottom, Fill X, Expand true],
		   configs= [Relief Ridge, Borderwidth 2,
			     Text "Quit", Command confirmQuit],
		   bindings= []} 
	end

    fun newFolderButton win = 
	Button{widId= newWidgetId(),
	       packings= [Side Bottom, Fill X, Expand true],
	       configs= [Relief Ridge, Borderwidth 2,
			 Text "New Folder",
			 Command (fn _ => TSimpleGUI.create_folder(20,20))],
	       bindings= []}
	
    fun filerButton win = 
	Button{widId= newWidgetId(),
	       packings= [Side Bottom, Fill X, Expand true],
	       configs= [Relief Ridge, Borderwidth 2,
			 Text "Import File",
			 Command (fn _ => TSimpleInstAppl.Filer.enter_file())],
	       bindings= []}

    val mainWin =
	let val wid = newWinId()
	in  mkWindow{winId    = wid, 
		     config   =[WinTitle "sml_tk Office 2000",
			        WinGeometry (NONE, SOME (50, 50))],
		     widgets  =Pack [TSimpleGUI.main_wid wid, 
                                   quitButton wid, filerButton wid,
                                   newFolderButton wid
			           ],
		     bindings = [],
	             init     =(fn ()=>TSimpleGUI.init init_guistate)}
	end

    fun go () = (SmlTk.startTclExn[mainWin]; !Result) 

    end (* local *)

end


structure TS = TSimpleInst;
