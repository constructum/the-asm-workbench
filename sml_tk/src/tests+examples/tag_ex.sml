(*
 *
 *  Project: sml/Tk: an Tk Toolkit for sml
 *  Author: Stefan Westmeier, University of Bremen
 * $Date: 2001/03/30 13:39:35 $
 * $Revision: 3.0 $
 *  Purpose of this file: Tag example
 *
 *)

structure TagEx :
    sig val go : unit -> string end =
struct

open BasicUtil SmlTk



(* Main Window *)
val mainWinId = mkWinId("hauptfenster")
val t1 = mkWidgetId "t1";
val fat = newAnnotationId();


val menu:Widget     = 
    let
	val quit      = mkSimpleAction(fn () => closeWindow mainWinId)

	fun delBut tn = mkSimpleAction(fn () => delAnnotation t1 tn)

	fun delTag tn = mkAction(fn (_:TkEvent) => delAnnotation t1 tn)

	fun colTag tn co = mkAction(fn (_:TkEvent) => addAnnotationConf 
	                                     t1 tn [Background co])

	val newBut = fn () =>
	    let
		val tn = newAnnotationId()
	    in
		TAWidget{annId    = tn,
			 mark     = MarkEnd,
			 widgets  =
			   Pack [Button{widId    = newWidgetId(),
					packings = [Fill X],
					configs  = [Text "Delete Me",
						    Command (delBut tn)],
					bindings=[]}],
			 configs = [],
			 bindings= []}
	    end

	val newTag = fn () => 
                     let 
			 val tn = newAnnotationId()
		     in
			 TATag{annId= tn,
			       marks= [(Mark(2,11), MarkToEnd 2)],
			       configs=[Background Blue,Borderwidth 2,Relief Raised],
			       bindings= [BindEv(ButtonPress NONE,delTag tn),
					  BindEv(Enter,(colTag tn Red)),
					  BindEv(Leave,(colTag tn Blue))]}
		     end

	val newSel = fn () => 
                     let 
			 val tn = newAnnotationId()
		     in
			 TATag{annId=tn,
			       marks= readSelection t1,
			       configs=[Background Green,
					Borderwidth 2,Relief Raised],
			       bindings= [BindEv(ButtonPress NONE ,delTag tn),
					  BindEv(Enter,(colTag tn Red)),
					  BindEv(Leave,(colTag tn Green))]}
		     end

	val addBut = mkSimpleAction(fn () => addAnnotation
	                        t1
				(newBut()))

	val addTag = mkSimpleAction(fn () => addAnnotation
	                        t1
				(newTag()))

	val addSel = mkSimpleAction(fn () => addAnnotation
	                        t1
				(newSel()))

	fun printTags wid = fn () =>
	    let
		val widg = getWidget wid
		val ans  = selTextWidAnnotations widg
		val ans' = List.filter (fn TATag _ => true | _ => false) ans

		fun prtAnPos an =
		    let
			val tn = selAnnotationId an
			val ms = readAnnotationMarks wid tn
		    in
			TextIO.output(TextIO.stdErr,"Tag: "^mkAnnIdString(tn)^"\n");
			TextIO.output(TextIO.stdErr,"\t"^showMarkL(ms)^"\n")
		    end
	    in
		app prtAnPos ans'
	    end


    in
	Frame{widId=newWidgetId(),
	      widgets=
                 Pack [Menubutton{widId=newWidgetId(),
			          mitems=[MCommand([Text "Quit",Command quit])],
			          packings=[Side Left],
			          configs=[Text "File", Tearoff false],
			          bindings=[]},
	       	       Menubutton{widId=newWidgetId(),
			          mitems=[MCommand([Text "Add Button",Command addBut]),
			                  MCommand([Text "Add Tag"   ,Command addTag]),
			                  MCommand([Text "Conv Sel"  ,Command addSel]),
			                  MCommand([Text "Clear Text", 
				                   Command (mkSimpleAction(fn ()=> clearText t1))]),
			                  MCommand([Text "Insert New Text", 
				                   Command (mkSimpleAction (fn ()=> insertTextEnd t1 
				                     ("No never, no never no more\n"^
					              "will I trust the Elves of Dunsinore\n")))]),
			                  MSeparator,
			                  MCommand([Text "Print Tags",
                                                   Command (mkSimpleAction(printTags t1))])
	 				 ],
			  	 packings=[Side Left],
			         configs=[Text "Item", Tearoff false],
			         bindings=[]},
	       	       Menubutton{widId=newWidgetId(),
			  	  mitems=[MCheckbutton [Text "Writeable",
					 Variable "TWState",
					 Command (mkSimpleAction(fn ()=>
					  case(readVarValue "TWState") of
					      "0" => setTextWidReadOnly t1 true
					     | _  => setTextWidReadOnly t1 false
					 ))],
			   MSeparator,
			   MCommand([Text "Clear Text+Annotations", 
				     Command (mkSimpleAction(fn ()=> clearAnnoText t1))]),
			   MCommand([Text "Replace Text+Annotations", 
				     Command (mkSimpleAction(fn ()=> let
					                 val t  = "Neuer Text\n"
					                 val tg = TATag{annId=fat,
									marks=[(Mark(1,0),Mark(1,5))],
									configs= [Background Red,Borderwidth 2,Relief Raised],bindings= []}
						      in
							  replaceAnnoText t1 (AnnoText{len=NONE, str=t, annotations=[tg]})


					      end
						  ))])
			   ],
			  packings=[Side Left], configs=[Text "Widget State", Tearoff false], bindings=[]}
	       ],
	      packings=[Fill X],
	      configs=[Relief Raised,Borderwidth 2],
	      bindings=[]}
    end

val board : Widget   = 
    let
	val t  = "\nDies ist ein Tag-Test\n\nUnd noch ein Test ...\n"
	val tg = TATag{annId= fat,marks= [(Mark(2,9),Mark(2,21))],
		       configs= [Background Red,Borderwidth 2,Relief Raised],
		       bindings=[BindEv(ButtonPress NONE, mkAction(fn _ => 
				   (TextIO.output(TextIO.stdOut, 
				"Button press in annotation\n");
				    addAnnotationConf t1 fat [Background Blue]))),
				 BindEv(ButtonRelease NONE, mkAction(fn _ => 
								     (TextIO.output(TextIO.stdOut, "Button release in annotation\n");
								      addAnnotationConf t1 fat [Background Red]))),
				 BindEv(Enter, mkAction(fn _ => 
							TextIO.output(TextIO.stdOut, "Annotation entered\n")))]}

	val wg1 =
	    TAWidget{annId    = newAnnotationId(),
		     mark     = Mark(3,0),
		     widgets  = Pack [Button{widId    = newWidgetId(),
					     packings = [Fill X],
					     configs  = [Text "Push Me",
							 Command noAction],
					     bindings = []},
				      Button{widId    = newWidgetId(),
					     packings = [Fill X],
					     configs  = [Text "Push Me", 
							 Command noAction],
					     bindings = []}],
		     configs  = [],
		     bindings = []}

	val wg2 =
	    TAWidget{annId    = newAnnotationId(),
		     mark     = Mark(3,0),
		     widgets  =
		       Pack [Button {widId    = newWidgetId(),
				     packings = [Fill X],
				     configs  = [Text "Push Me",
						 Command
						 (mkSimpleAction
						  (fn () => () ))],
				     bindings = []}],
		     configs  = [],
		     bindings = []}

	val at  = AnnoText{len=NONE, str=t, annotations=[tg, wg1, wg2]}
    in
	Frame{widId=newWidgetId(),
	      widgets=Pack [TextWid{widId=t1, scrolltype=LeftScb,
		                    annotext=at,
		                    packings=[],
		                    configs=[Active false],
		                    bindings=[]}],
	      packings=[Side Left, Fill X],
	      configs=[Width 200,Height 200,Relief Raised,Borderwidth 2],
	      bindings=[]}
    end

val area    = [menu,board];

val act  = mkSimpleAction(fn () => ())



val initwin = [mkWindow{winId    = mainWinId, 
			config   = [WinTitle "Tag Example"],
			widgets  = Pack area, 
                        bindings = [],
			init     = act}]


val go = fn () => startTclExn initwin


end;

