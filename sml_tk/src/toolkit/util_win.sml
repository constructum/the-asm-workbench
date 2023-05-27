 (* *************************************************************************

    $Source: /repository/sml/sml_tk/src/toolkit/util_win.sml,v $

    Utitility windows.

    Windows for errors, warnings, user confirmation and text entry. 

    A better version of util_win.sml

    $Date: 2001/03/30 13:39:56 $
    $Revision: 3.0 $

    Author: cxl (Last modification by $Author: 2cxl $)

   (C) 1997, 1998, Bremen Institute for Safe Systems, Universitaet Bremen

   ************************************************************************* *)


structure UW : UTIL_WIN =

struct

     open SmlTk BasicUtil

     (* -- Configuration section ------------------------------------------- *)

     (* Width (in pixels) and font for error, warning and confirmation wins *)
     val msgFont      = Normalfont []
     val msgWidth     = 180
     val buttonRelief = Raised
     val buttonWidth  = 5
     val buttonFont   = SansSerif []	    
     fun errorIconFilenm()   = OS.Path.concat(SmlTk.getLibPath(),
					      "images/stop.gif")
     fun warningIconFilenm() = OS.Path.concat(SmlTk.getLibPath(),
					      "images/warning.gif")
     fun infoIconFilenm()    = OS.Path.concat(SmlTk.getLibPath(),
					      "images/info.gif")
     val infoTimeOut         = 10 (* Info windows stay up at least this long *)

     val enterTextFont = Typewriter [Large]

     (* -- End of configuration section ------------------------------------ *)

     fun errwrnwidgs(iconpath, msg, cc) = 
	 [Label{widId= newWidgetId(), packings= [Side Left, Fill Y],
		configs= [Icon (FileImage (iconpath, newImageId()))], 
		bindings= []},
	  Frame{widId= newWidgetId(),
		widgets= Pack[Message{widId= newWidgetId(),
				      packings= [PadX 20, PadY 20, 
						 Side Top, Fill X],
				      configs=  [Text msg, Width msgWidth, 
						 Font msgFont],
				      bindings= []},
			      Button{widId= newWidgetId(),
				     configs= [Text "Continue", Command cc,
					       Relief buttonRelief,
					       Width buttonWidth,
					       Font buttonFont],
				     packings= [Side Right],
				     bindings= []}],
		packings= [Side Top, Fill Both], 
		bindings= [], configs= []}]

    fun errwrnwin(title, iconpath, msg, cc) =
	let val wid     = newWinId()
	    fun close ()= (closeWindow wid; cc())
	in
	    mkWindow{winId    = wid, 
		     config   = [WinTitle title, WinTransient NONE], 
		     widgets  = Pack(errwrnwidgs(iconpath, msg, close)), 
                     bindings = [],
		     init     = noAction}
	end


    fun error   msg = openWindow (errwrnwin("Error Message",
					    errorIconFilenm(), msg, K0))
				  
    fun warning msg = openWindow(errwrnwin("Warning Message",
					    warningIconFilenm(), msg, K0))

    fun error_cc(msg, cc)   = openWindow(errwrnwin("Error Message",
					    errorIconFilenm(), msg, cc))
				  
    fun warning_cc(msg, cc) = openWindow(errwrnwin("Warning Message",
					    warningIconFilenm(), msg, cc))



    (* --- Confirmation [ OK ]  [ Cancel ] -------------------------------- *)

    val buttonConf = [Width  buttonWidth,
		      Relief buttonRelief,
		      Font   buttonFont]

    fun OkCancelButtons(win, cont) =
	let fun cc () = (closeWindow win; cont())
	    fun no () = (closeWindow win)
	in  Frame{widId= newWidgetId(), 
		  widgets= Pack[Button{widId= newWidgetId(),
				       packings= [PadX 10, PadY 15, Side Left],
				       configs= [Text "Cancel", Command no]@
				                 buttonConf, 
				       bindings= []},
				Button{widId= newWidgetId(),
				       packings= [PadX 10, PadY 15, Side Right], 
				       configs= [Text "OK", Command cc]@
				                 buttonConf, 
				       bindings= []}],
	          packings= [Side Bottom, Fill X], configs= [], bindings= []}
	end


    fun confirm(msg, cc) =
	let val win = newWinId()
	    val pic = Label{widId= newWidgetId(), 
			    packings= [Side Left, Fill Y],
			    configs= [Icon (FileImage(warningIconFilenm(),
						      newImageId()))], 
			    bindings= []}
	    val msg = Message{widId= newWidgetId(),
				     packings= [Side Top, Fill Both],
				     configs= [Text msg, Width msgWidth, 
					       Font  msgFont],
				     bindings= []}
	    val frm = Frame{widId= newWidgetId(),
			    widgets= Pack [msg, OkCancelButtons(win, cc)], 
			    packings= [Side Top, Fill X, Expand true], 
			    configs= [], bindings= []}
	in openWindow (mkWindow{winId    = win, 
				config   =[WinTitle "Please Confirm Or Abort",
					   WinTransient NONE], 
				widgets  = Pack [pic, frm], 
                                bindings = [],
				init     = noAction})
	end

    (* --- display a text -------------------------------------------------- *)


    fun disp_win(winid, title, cc, disp_widg) =
	let fun quitBut win =  Frame{widId= newWidgetId(),
				     widgets= Pack
				     [Button{widId= newWidgetId(), 
					     packings= [Side Right, 
							PadX 10, PadY 10],
					     configs= [Text "Close", 
					      Command (fn () =>
						       closeWindow win)]@
					     (buttonConf), 
					     bindings= []}],
				     packings= [Side Bottom, Fill X], 
				     bindings= [], configs= []}
	in  winid before 
	    openWindow(mkWindow{winId    = winid, 
				config   = [WinTitle title], 
				widgets  = Pack [quitBut winid, disp_widg],
                                bindings = [],
				init     = fn ()=> cc (selWidgetId disp_widg)})
	end


    fun display' {winId, widId, title, width, height, text, cc} = 
	(* add scroll button if text is longer than height *)
	(* !! This doesn't quite work, since it doesn't take into account
	 * line wrapping-- hence disabled *)
	let val scb = if #rows(selLength text)>= height then LeftScb 
		      else NoneScb 
	in ignore (disp_win(winId, title, cc, 
			    TextWid{widId= widId, 
				    scrolltype= RightScb, annotext= text,
				    packings= [Side Top, Fill Both, 
					       Expand true], 
				    configs= [Active false, 
					      Borderwidth 1, Width width,
					      Height height], 
				    bindings= []}))
	end

    fun display_id {winId, widId, title, width, height, text} = 
	display'{winId= winId, widId= widId, title=title, width=width, 
		 height=height, text=text, cc= fn _=> ()}

    fun display {title, width, height, text, cc} = 
	display'{winId= newWinId(), widId= newWidgetId(), title=title, 
		 width=width, height=height, text=text, cc= cc}
	

    (* --- informative messages -------------------------------------------- *)


    fun infofrm msg =
	let val pic = Label{widId= newWidgetId(), 
			    packings= [Side Left, Fill Y, Expand true,
				       PadX 10, PadY 10],
			    configs= [Icon (FileImage(infoIconFilenm(),
						      newImageId()))],
			    bindings=[]}
	    val w   = if String.size msg < 80 then [Width 100] else []
	in  Frame{widId= newWidgetId(),
		  widgets= Pack [pic, 
				 Message{widId= newWidgetId(), 
					 packings= [Side Top, Fill Both, 
						    Expand true],
					 configs= w@[Text msg, Font msgFont],
					 bindings= []}], 
		  configs= [], bindings= [], packings= []}
	end

    fun info_cc msg =
	let val frm= infofrm msg
	    val w= newWinId()
	    (* to make sure the info message stays on
             * for at least timeout seconds: start timer... *)
            val owt = Timer.startRealTimer()
	in  (openWindow(mkWindow{winId    = w, 
				 config   = [WinTitle "Information"], 
				 widgets  = Pack [frm],
                                 bindings = [],
				 init     = noAction});
	     fn ()=> if occursWin w then
	     (* window's still up, check if it stayed up long enough *)
	         let val elapsd  = Timer.checkRealTimer owt
                     val timeout = Time.fromSeconds (Int.toLarge infoTimeOut)
                 in  (if  Time.<(elapsd, timeout) then
                          ignore(Posix.Process.sleep
                                 (Time.-(timeout, elapsd)))
                      else ());
                     closeWindow w
                 end
             else ())
	end

    fun info msg =
	let val frm= infofrm msg
	in  ignore(disp_win(newWinId(), "Information", fn _=> (), frm))
	end

   (* ---- enter windows ------------------------------------ *)
 
   fun buttons(win, twids, cont) = 
       (* This and OkCancelButtons above are remarkably similar, but
	* I don't know how to implement this in terms of OkCancelButtons *)
       let fun cc () =  let val txts= map readTextAll twids
			in  (closeWindow win; cont txts)
			end
	   fun no () =  closeWindow win
       in  Frame{widId= newWidgetId(), 
		 widgets= Pack
		          [Button{widId= newWidgetId(), 
				  packings= [Side Left, PadX 5, PadY 5], 
				  configs= [Text "Cancel", Command no]@buttonConf, 
				  bindings= []},
			   Button{widId= newWidgetId(),
				  packings= [Side Right, PadX 5, PadY 5], 
				  configs= [Text "OK", Command cc]@buttonConf,
				  bindings= []}],
		 packings= [Side Bottom, Fill X], bindings= [], configs= []}
       end
	   
   fun enterText0{title, prompt, default, widgetsbelow, width, 
		  heights, headers, cc} = 
       let
	   val win     = newWinId()
	   val twids   = List.tabulate(length heights, fn _ => newWidgetId())
	   (* make sure there's enough headers *)
	   val hds     = headers @ List.tabulate(length heights,
						 fn _ => "")
	   val prmpt   = Label{widId= newWidgetId(),
			       packings= [Side Top, Fill X, Expand true], 
			       configs= [Text prompt, Width width],
			       bindings= []}
	   fun ewtxt((w, h), p) = 
	       let val tw= TextWid{widId= w, scrolltype= RightScb, 
				   annotext= mkAT default,
				   packings= [Side Top, Expand true],
				   configs= [Relief Raised, Borderwidth 2, 
					     Width width, Height h,
					     Font enterTextFont],
				   bindings= []}
	       in if p= "" then [tw] else
		       [Label{widId= newWidgetId(),
				    packings= [Side Top, Expand true, Fill X],
				    configs= [Text p], bindings= []}, tw]
	       end
	   val txwdgs   = List.concat (ListPair.map ewtxt 
				       (ListPair.zip(twids, List.rev heights), 
					hds))
	   val bws      = Frame{widId= newWidgetId(),
				widgets=  Pack (buttons(win, twids, cc)::
					  (map (fn w=> updWidgetPack w 
						[Side Top]) widgetsbelow)),
				packings= [Side Bottom, Fill X, Expand true],
				configs= [], bindings= []}
	   fun init ()  = ()
       in openWindow(mkWindow{winId    = win, 
			      config   = [WinTitle title], 
			      widgets  = Pack ([bws,prmpt]@txwdgs),
                              bindings = [],
			      init     = init})
       end

   (* convert function on singleton lists of strings to function on strings *)
   fun list2s cc (t::_)= cc t 
     | list2s cc _     = cc ""

   fun enterText{title, prompt, default, height, width, cc}=
       enterText0{title=title, prompt=prompt, default=default, 
		  widgetsbelow=[], heights=[height], headers=[""],
		  width= width, cc= list2s cc}


   fun enterLine{title, prompt, default, width, cc} =
       let
	   val win   = newWinId()
	   val twid  = newWidgetId()
	   val wHere = if (size prompt) > (width* 2) 
		       then [Side Left, Fill Y, Expand true]
		       else [Side Top, Fill X, Expand true]
	   val prmpt = Label{widId= newWidgetId(),
			     packings= wHere, bindings= [],
			     configs= [Text prompt]}
	   val entln = Entry{widId= twid, packings= [Side Left], 
			     configs= [Relief Ridge, Width width, 
				       Borderwidth 2, Font enterTextFont],
			     bindings= [BindEv(KeyPress("Return"),
					       fn _ => let val txt= readTextAll twid
						       in  (closeWindow win; 
							    cc txt)
						       end)]}
       in openWindow(mkWindow{winId= win,
			      config= [WinTitle title],
			      widgets  = Pack [Frame{widId= newWidgetId(),
					             widgets= Pack
						               [prmpt, entln], 
					             packings= [Side Top,
								Fill X],
					             configs=[], bindings=[]},
						buttons(win, [twid],
							list2s cc)],
                              bindings = [],
			      init     = fn _ => insertText
                                                   twid default(Mark(1,0))})
       end


end
