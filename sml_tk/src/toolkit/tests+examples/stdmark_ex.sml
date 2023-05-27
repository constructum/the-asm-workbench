(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/tests+examples/stdmark_ex.sml,v $
 
   sml_tk Standard Markup Language: an example.

   $Date: 2001/03/30 13:40:04 $
   $Revision: 3.0 $
   Author: cxl (Last modification by $Author: 2cxl $)

   (C) 1997, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)


structure StdMarkEx : sig val go : unit-> unit end =

    struct

    open SmlTk SmlTk21

    val sometext = 
	"One can do <font bold>boldfaced<\\font> bits, and <font em>italic<\\font> bits.\n"^
	"One can make things <font large>larger<\\font> and <font tiny>smaller<\\font>.\n"^
	"There are <font tt>different<\\font> typefaces as well, such as that and <font sf>sans-serif<\\font>, and of course, <font symb>symbols<\\font>!\n"^
	"And <font bold sf>all these things <\\font>can be <font bold it>combined<\\font>.\n"^
	"You can <raise 5>boxes<\\raise>, <raise -5>lower<\\raise> and <box>box text<\\box>.\n"^
	"There are also special characters: &alpha;, &omega;, &Sigma;.\n"

    fun textWidget win txt =
	let val twid = newWidgetId()
	    val anno = StdMarkup.getAnnText txt
	in  TextWid(twid, NoneScb, anno,
		    [Fill X, Side Top], [Active false], [])
	end


    fun quitButton win =
	Button(newWidgetId(),
                   [Side Bottom, Fill X, Expand true],
                   [Relief Ridge, Borderwidth 2,
                    Text "Quit", Command (fn ()=> closeWindow win)], []) 


   fun mainWin txt =
        let val wid = newWinId()
        in  mkWindow{winId= wid, 
		     config= [WinTitle "SmlTk Standard Markup Test Window"], 
		     widgets= Pack [textWidget wid txt, quitButton wid],
		     bindings = [],
		     init= noAction}
        end
 
    fun go () = (SmlTk.startTclExn[mainWin sometext];())
                   
end


