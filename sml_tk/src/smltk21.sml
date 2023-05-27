(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/smltk21.sml,v $
 
   Compatibility Mode for sml_tk3.0 vs. sml_tk2.1
  
   $Date: 2001/03/30 13:39:18 $
   $Revision: 3.0 $
   Author: bu (Last modification by $Author: 2cxl $)

   (C) 1996, Bremen Institute for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)

structure SmlTk21 (* :
signature
sig
    val AnnoText : (int * int) option * string * Annotation list -> AnnoText
    val Button : WidId * Pack list * Configure list * Binding list -> Widget
    val CIcon : CItemId * Coord * IconKind * Configure list * Binding list
                -> CItem
    val CLine : CItemId * Coord list * Configure list * Binding list -> CItem
    val COval : CItemId * Coord * Coord * Configure list * Binding list
                -> CItem
    val CPoly : CItemId * Coord list * Configure list * Binding list -> CItem
    val CRectangle : CItemId * Coord * Coord * Configure list * Binding list
                     -> CItem
    val CTag : CItemId * CItemId list -> CItem
    val CWidget : CItemId * Coord * WidId * Widget list * Configure list
                  * Configure list * Binding list
                  -> CItem
    val Canvas : WidId * ScrollType * CItem list * Pack list * Configure list
                 * Binding list -> Widget
    val CheckButton : WidId * Pack list * Configure list * Binding list
                      -> Widget
    val Entry : WidId * Pack list * Configure list * Binding list -> Widget
    val Frame : WidId * Widget list * Pack list * Configure list * Binding list
                -> Widget
    val Label : WidId * Pack list * Configure list * Binding list -> Widget
    val Listbox : WidId * ScrollType * Pack list * Configure list
                  * Binding list
                  -> Widget
    val MenuButton : WidId * bool * MItem list * Pack list * Configure list
                     * Binding list
                     -> Widget
    val Message : WidId * Pack list * Configure list * Binding list -> Widget
    val Popup : WidId * bool * MItem list -> Widget
    val RadioButton : WidId * Pack list * Configure list * Binding list
                      -> Widget
    val TATag : AnnId * (Mark * Mark) list * Configure list * Binding list
                -> Annotation
    val TAWidget : AnnId * Mark * WidId * Widget list * Configure list
                   * Configure list * Binding list
                   -> Annotation
    val TextWid : WidId * ScrollType * AnnoText * Pack list * Configure list
                  * Binding list
                  -> Widget
end *) =
struct

    (* constructors *)

    fun	CRectangle (cid, c1, c2, cl, bl) =
                SmlTk.CRectangle{citemId=cid, coord1=c1, 
                                 coord2=c2, configs=cl, bindings=bl};
    fun COval (cid, c1, c2, cl, bl) =
                SmlTk.COval{citemId=cid, coord1=c1, 
                      coord2=c2, configs=cl, bindings=bl};
    fun CLine (cid, c, cl, bl) =
                SmlTk.CLine{citemId=cid, coords=c, configs=cl, bindings=bl};
    fun CPoly (cid, c, cl, bl) =
                SmlTk.CPoly{citemId=cid, coords=c, configs=cl, bindings=bl};
    fun CIcon (cid, c, icon, cl, bl) =
                SmlTk.CIcon{citemId=cid, coord=c, iconkind=icon,
                      configs=cl, bindings=bl};
    fun CWidget (cid, c, wid, widgs, cl1, cl2, bl) =
                SmlTk.CWidget{citemId=cid, coord=c,
			      widgets=SmlTk.Pack widgs,
                              configs=cl2 , bindings=bl};
    fun CTag (cid, cids) = SmlTk.CTag{citemId=cid, citemIds=cids}

    
    fun AnnoText (p, st, ann) = SmlTk.AnnoText{len=p, str=st, annotations=ann}

    
    fun TATag (ann, m, cl, bl) = 
                SmlTk.TATag{annId=ann, marks=m, configs=cl, bindings=bl}
    fun TAWidget (ann, m, wid, widgs, cl1, cl2, bl)=
                    SmlTk.TAWidget{annId=ann, mark=m,
				   widgets=SmlTk.Pack widgs,
				   configs=cl2, bindings=bl}

    fun Frame (wid, wl, pl, cl, bl) = 
                SmlTk.Frame{widId=wid, widgets=SmlTk.Pack wl, packings=pl,
                            configs=cl, bindings =bl}; 
  
    fun Message (wid, pl, cl, bl) = 
                SmlTk.Message{widId=wid, packings=pl, 
                              configs=cl, bindings =bl}; 

    fun Label (wid, pl, cl, bl) = 
                SmlTk.Label{widId=wid, packings=pl, 
                            configs=cl, bindings =bl}; 

    fun Listbox (wid, st, pl, cl, bl) = 
                SmlTk.Listbox{widId=wid, scrolltype=st, packings=pl, 
                              configs=cl, bindings =bl}; 
     
    fun Button (wid, pl, cl, bl) = 
                SmlTk.Button{widId=wid, packings=pl, 
                             configs=cl, bindings =bl}; 

    fun RadioButton (wid, pl, cl, bl) = 
                SmlTk.Radiobutton{widId=wid, packings=pl, 
                                  configs=cl, bindings =bl}; 
    fun CheckButton (wid, pl, cl, bl) = 
                SmlTk.Checkbutton{widId=wid, packings=pl, 
                                  configs=cl, bindings =bl}; 
    fun MenuButton (wid, yn, mit, pl, cl, bl) =
                SmlTk.Menubutton{widId=wid, mitems=mit, packings=pl,
				 configs=SmlTk.Tearoff yn :: cl, bindings=bl}
    fun Entry (wid, pl, cl, bl) =
                SmlTk.Entry{widId=wid, packings=pl, configs=cl, bindings =bl}; 
    fun TextWid (wid, st, annot, pl, cl, bl) = 
                SmlTk.TextWid{widId=wid, scrolltype=st, annotext=annot, 
                              packings=pl, configs=cl, bindings =bl}; 
    fun Canvas (wid, st, cit, pl, cl, bl) = 
                SmlTk.Canvas{widId=wid, scrolltype=st, citems=cit, 
                             packings=pl, configs=cl, bindings =bl}; 
    fun Popup (wid, yn, mit) =
	        SmlTk.Popup{widId=wid, mitems=mit,
			    configs = [SmlTk.Tearoff yn]};

    val replaceTextWidText = SmlTk.replaceAnnoText
    val clearTextWidText   = SmlTk.clearAnnoText
end
