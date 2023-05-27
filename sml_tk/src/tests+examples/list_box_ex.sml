
structure ListBoxEx :>
    sig val go : unit -> string end =
struct

open SmlTk

val marks = ref (MarkEnd,MarkEnd);

val mainWinId = mkWinId "main"

structure WarnWin :>
sig
    val warn : string -> unit
end
=
    struct
	val warn = mkWinId "warning"
	val nogoon   = mkSimpleAction(fn () => closeWindow warn);
	val noaction = mkSimpleAction(fn () => ());    

	fun Message1 msg yes no = Label{widId=newWidgetId(),packings=[Fill X, Expand true],
				        configs=[Text msg, Relief Flat, Width 25], bindings=[]};
	fun yesbut  msg yes no = Button{widId=newWidgetId(),
				        packings=[Side Left,  Fill X, Expand true],
					configs=[Text "Ok", Command yes], bindings=[]};
	fun yesno   msg yes no = Frame{widId=newWidgetId(), widgets=Pack [yesbut msg yes no],
				       packings=[], configs=[], bindings=[]};

	fun tree2   msg yes no = [Message1 msg yes no, yesno msg yes no];

	fun warnwin msg yes no = mkWindow{winId=warn, 
					  config=[WinTitle "Warning"],
					  widgets=Pack(tree2 msg yes no),
                                          bindings = [],
					  init=noaction}

	fun warning msg yes no = openWindow (warnwin msg yes no);

	fun warn s = warning s nogoon noaction;
    end;


val doQuit       = mkSimpleAction(fn () => closeWindow mainWinId);
val doNoAction   = mkSimpleAction(fn () => ());

val list = mkWidgetId "lister";
fun doSelect () = 
    let
	val ms  = readSelRange (list)
	val cm  = fn (a,MarkEnd) => (a,a)
		   | (a,b)       => (a,b)
	val sel = fn [m] => (marks:=m;readText (list) (cm(m)))
                   | _   => ""
	val se  = sel ms
    in
       WarnWin.warn se
    end

and lister () = Listbox{widId=list, scrolltype=RightBotScb,
			packings=[Fill X],
			configs=[Relief Ridge,Borderwidth 2],
			bindings=[BindEv(Double(ButtonPress (SOME 2)),
				  fn _=> doSelect())]}

and doFill () = app (insertTextEnd list) ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
					  "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
					  "cccccccccccccccccccccccccccccccccc"]

and fillButton fill = Button{widId=newWidgetId(),
			     packings=[Side Left,Fill X,Expand true],
			     configs=[Text "Fill",Command (mkSimpleAction(fill))], bindings=[]}

and filler () = Frame{widId=newWidgetId(),
		      widgets=Pack [fillButton doFill],
		      packings=[Fill X],
		      configs=[Relief Ridge,Borderwidth 2],
		      bindings=[]}

and selButton fill = Button{widId=newWidgetId(),
			    packings=[Side Left,Fill X,Expand true],
			    configs=[Text "Select",Command (mkSimpleAction(fill))], bindings=[]}

and selector () = Frame{widId=newWidgetId(),
			widgets=Pack [selButton doSelect],
			packings=[Fill X],
			configs=[Relief Ridge,Borderwidth 2],
		        bindings=[]}


and quitButton quit = Button{widId=newWidgetId(),
			     packings=[Side Left,Fill X,Expand true],
			     configs=[Text "Quit",Command (quit)], bindings=[]}

and quitter () = Frame{widId=newWidgetId(),
		       widgets=Pack [quitButton doQuit],
		       packings=[Fill X],
		       configs=[Relief Ridge,Borderwidth 2],
		       bindings=[]};

val initwin = [mkWindow{winId=mainWinId, 
			config=[WinTitle "ListBox Example"],
			widgets=Pack [lister(),filler(),selector(),quitter()],
		     	bindings = [],
			init=doNoAction}]

val go = fn () => startTclExn initwin


end;


