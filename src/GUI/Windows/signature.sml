structure SignatureWindow =
struct
  open GUI_Misc
  val debug = debug true "SignatureWindow"

  val signatureWindow_winId = mkWinId "signatureWindow"

  structure Sgn = ASM_Signature
  structure Dct = ASM_Dictionary

  val update_ref = ref noAction
  fun update f   = ( f (); (!update_ref) () )

  datatype OBJECTS_FLAG  = ShowTypes | ShowFunctions | ShowRules | ShowAll
  datatype ORIGIN_FLAG   = OnlyBuiltin | FromSelectedFile | FromSpecification | OnlyInteractive | AllOrigins
  datatype ORDER_FLAG    = OrderOfDefinition | AlphabeticalOrder

  val ObjectsFlag  = ref ShowAll
  val OriginFlag   = ref FromSelectedFile
  val OrderFlag    = ref OrderOfDefinition

  val replace = String_.replace
  val width   = 200

  fun title () =
    let val s1 = case (!ObjectsFlag) of ShowTypes => "Types" | ShowFunctions => "Functions"
                                    | ShowRules => "Rules" | ShowAll => ""
        val s2 = case (!OriginFlag, !ObjectsFlag) of
            (OnlyBuiltin, ShowAll)      => "(All Built-in Names)" 
          | (OnlyBuiltin, _)            => "(Built-in " ^ s1 ^ ")"
          | (FromSelectedFile, ShowAll) => "(\"" ^ (Filelist.getSelectedFile ()) ^ "\")"
          | (FromSelectedFile, _)       => "(" ^ s1 ^ " from \"" ^ (Filelist.getSelectedFile ()) ^ "\")"
          | (FromSpecification, ShowAll)  => "(Names from any Specification File)"
          | (FromSpecification, _)        => "(" ^ s1 ^ " from any Specification File)"
          | (OnlyInteractive, ShowAll)  => "(Interactively Defined Names)"
          | (OnlyInteractive, _)        => "(Interactively Defined " ^ s1 ^ ")"
          | (AllOrigins, ShowAll)       => "(Complete Signature)"
          | (AllOrigins, _)             => "(" ^ s1 ^ ")"
    in replace "Signature Window $1" [ s2 ]
    end

  fun printSignatureEntry with_type sign name =
    let val optSignItem = Sgn.find sign name
    in case Sgn.idKind optSignItem of
	 SOME Sgn.TypeKind =>
	 ( let val args = List.tabulate (valOf (Sgn.typeArity optSignItem), Misc.K "_")
	   in replace "type $1$2"
		      [ name, if length args = 0 then "" else " (" ^ (List_.output ", " args) ^ ")" ]
	   end handle _ => "" )
       | SOME Sgn.FuncKind =>
	 ( let val f_T      = valOf (Sgn.typeOf optSignItem)
               val fun_kind = ASM.functionKindToString width (valOf (Sgn.functionKind optSignItem))
	       val fun_name = ASM.infixedNameToString  width (valOf (Sgn.opStatus optSignItem), name)
	   in if not with_type
	      then replace "$1 function $2" [ fun_kind, fun_name ]
	      else if ASM_Type.isUnitType (ASM_Type.domain f_T)
	      then replace "$1 function $2 : $3" [ fun_kind, fun_name, ASM.typeToString width (ASM_Type.range f_T) ]
	      else replace "$1 function $2 : $3" [ fun_kind, fun_name, ASM.typeToString width f_T ]
	   end handle _ => "" )
       | SOME Sgn.RuleKind =>
         ( let val rule_domain = ASM_Type.domain (valOf (Sgn.typeOf optSignItem))
           in if (not with_type) orelse (ASM_Type.isUnitType rule_domain)
	    then replace "rule $1" [ name ]
	    else replace "rule $1 : $2" [ name, ASM.typeToString width rule_domain ]
           end handle _ => "" )
       | NONE => ""
    end


  fun isTypeName name     = (Sgn.idKind (Sgn.find (!ASM_Top.sign) name) = SOME Sgn.TypeKind)
  fun isFunctionName name = (Sgn.idKind (Sgn.find (!ASM_Top.sign) name) = SOME Sgn.FuncKind)
  fun isRuleName name     = (Sgn.idKind (Sgn.find (!ASM_Top.sign) name) = SOME Sgn.RuleKind)
  fun functionKind name   = (Sgn.functionKind (Sgn.find (!ASM_Top.sign) name))
  fun isBuiltin name      = Sgn.defines (ASM_Top.initialSign, name)

  exception LocateDef
  fun locateDef name =
    case Dct.locate (!ASM_Top.dict, name) of
      SOME (pos, loc) => (pos, loc)
    | _ => raise LocateDef


  fun ShowWithoutType name = printSignatureEntry false (!ASM_Top.sign) name
  fun ShowWithType name    = printSignatureEntry true (!ASM_Top.sign) name

  val ShowName = ref ShowWithType



  fun listNamesSatisfying property = List.filter property (map #1 (Sgn.toList (!ASM_Top.sign)))
  fun listAllNames ()    = listNamesSatisfying (Misc.K true)
  fun listBuiltin ()     = map #1 (Sgn.toList (ASM_Top.initialSign))
  fun listUserDefined () = listNamesSatisfying (not o isBuiltin)


  fun ListNames () =
    let val nameList =
	  if (!OrderFlag = AlphabeticalOrder)
	  then case !OriginFlag of
                 OnlyBuiltin => listBuiltin ()
               | AllOrigins  => listAllNames ()
               | _           => listUserDefined ()
	  else case !OriginFlag of
                 OnlyBuiltin => listBuiltin ()
               | AllOrigins  => listBuiltin () @ Filelist.getSpecificationNames ()
               | _           => Filelist.getSpecificationNames ()
	fun from_file filePred objName =
	  case Dct.locate (!ASM_Top.dict, objName) of
	    SOME (ASM_AST.File s, _) => filePred s
	  | _                     => false
	fun P name =
	( (name <> "_TUPLE")
	  andalso
	    ( (!ObjectsFlag = ShowAll)
	      orelse
	      ((!ObjectsFlag = ShowTypes) andalso (isTypeName name))
	      orelse
	      ((!ObjectsFlag = ShowFunctions) andalso (isFunctionName name))
	      orelse
	      ((!ObjectsFlag = ShowRules) andalso (isRuleName name)) )
	  andalso
	    ( ((!OriginFlag = OnlyBuiltin) andalso (isBuiltin name))
  (*            orelse
	      ((!OriginFlag = OnlyInteractive) andalso (ASM.is_interactive (ASM.symbol_id name)))  *)
	      orelse
	      ( (!OriginFlag = FromSelectedFile)
		   andalso (from_file (fn s => s = Filelist.getSelectedFile ()) name) )
	      orelse
	      ((!OriginFlag = FromSpecification) andalso (from_file (fn _ => true) name))
	      orelse
	      ((!OriginFlag = AllOrigins) andalso (true)) ) )
    in List.filter P nameList
    end

  (* ********************************************************************** *)
      
  structure Listbox =
  struct
    val id = mkWidgetId "signatureListbox"

    structure State =
    struct
      val names    = ref ([] :string list)
      val position = ref ~1
      fun valid_position () = (!position) >= 0 andalso (!position) < length (!names)
      fun set_position pos = (position := pos)
      fun set_names names_ = (names := names_)
    end

    val viewObjectRef = ref (fn () => ())

    fun widget () =
      Listbox (
	id, RightScb, [ Side Left, Side Top, Fill Both, Expand true ],
	GUI_Misc.Listbox.stdConfig () @ [ Width 60 ],
	GUI_Misc.Listbox.stdBindings changePosition
	@ [ BindEv(Double (ButtonPress (SOME 1)), fn _ => (!viewObjectRef)()) ] )

    and register () =
      StateDepWidget.register_widget ( id, fn id => redraw (),
				       [ Filelist.gse_filelist, Filelist.gse_filelist_pos ] )
    and unregister () =
      StateDepWidget.unregister_widget id

    and changePosition () =
      let val pos = GUI_Misc.Listbox.setPosition id
      in State.set_position pos
	 (*; StateDepWidget.full_gui_update () *)
      end

    and redraw () =
      let val names = ListNames ()
      in State.set_names names;
	 GUI_Misc.Listbox.fill id (map (!ShowName) names);
	 changeTitle signatureWindow_winId (mkTitle (title ()))
      end
  end

  fun get_file_def (filename: string, start_: (int * int), end_: (int * int)) =
    let open TextIO
      val stream = openIn filename
      fun skipLines n = if n > 0 then (inputLine stream; skipLines (n-1)) else ()
      fun readLines n =
        let fun F (i, L : string list) :string list =
	      case inputLine stream of
	             SOME s => if i = 0
			       then (F (i + 1, substring (s, #2 start_-1, size s - #2 start_+1) :: L))
			       else if i < n-1
			       then F (i + 1, s :: L)
			       else if i = n-1
			       then F (i + 1, substring (s, 0, #2 end_) :: L)
			       else L
		   | NONE => L
              in rev (F (0, []))
              end
          val _     = skipLines (#1 start_ - 1)
          val lines = readLines (#1 end_ - #1 start_ + 1)
      in TextIO.closeIn stream;
	 String.concat lines
      end

  fun get_definition (s: string) =
    ( let open ASM_AST
	    val (loc, Pos { first = start_, last = end_ }) = locateDef s
      in case loc of
	   Primitive     => "[ primitive ]"
	 | Interactive x => x
	 | File FileName => get_file_def (FileName, start_, end_)
      end handle _ => "[ definition not found ]" )

  (* ********************************************************************** *)

  datatype ELEM = ViewObjectElem of {
    win_id                : WinId,
    type_label_id         : WidId,
    definition_textbox_id : WidId,
    name                  : string
  }

  local
    val closeWindowRef = ref (fn (elem :ELEM) => ())
  in
  
  structure ViewObjectWindows = CHILD_WINDOWS (
    type ELEM = ELEM
    val closeWindowRef = closeWindowRef

    fun get_win_id (ViewObjectElem elem :ELEM) = #win_id elem

    val pack_ttB = [ Side Left, Side Top, Expand false, Fill X ]

    fun mkframe L =
      Frame (newWidgetId (), L, [ Side Top, Expand true, Fill Both ], [Relief Ridge, Borderwidth 2], [])
    fun mkfixframe L =
      Frame (newWidgetId (), L, [ Side Top, Expand false, Fill Both ], [Relief Ridge, Borderwidth 2], [])
    fun contents (ViewObjectElem (elem as { win_id :WinId, type_label_id :WidId, definition_textbox_id :WidId, name :string })) =
      let val () = ()   (* (location, position) = (locateDef name) *)
      in [ (* viewObjectWindowMenu elem,*)
           mkfixframe [
               Label (newWidgetId (), pack_ttB, [ Text "Type" ], []),
	       mkfixframe [
		 Label (#type_label_id elem, [ Side Left, Fill X, Expand false ], [], [])
	       ]
           ],
           mkframe [
             Label (newWidgetId (), pack_ttB, [ Text "Definition" ], []),
             stdTextWid (#definition_textbox_id elem, [ Width 60, Height 10 ])
           ],
           Frame ( newWidgetId (),
             [ LeftButton (" Close ", fn _ => (!closeWindowRef) (ViewObjectElem elem)) ],
             [ Side Bottom, Expand false, Fill X ], [], [] ) ]
      end

    and init (elem' as ViewObjectElem elem) =
    ( StateDepWidget.register_widget ( #type_label_id elem, fn id => updateTypeLabel elem',
				       [ Filelist.gse_filelist ] );
      StateDepWidget.register_widget ( #definition_textbox_id elem, fn id => updateDefinitionTextbox elem',
				       [ Filelist.gse_filelist ] );
      () )

    and destroy (elem' as ViewObjectElem elem) =
    ( StateDepWidget.unregister_widget (#type_label_id elem);
      StateDepWidget.unregister_widget (#definition_textbox_id elem);
      () )

    and updateTypeLabel (elem' as ViewObjectElem elem) =
      addConf (#type_label_id elem) [ Text (ShowWithType (#name elem)) ]

    and updateDefinitionTextbox (elem' as ViewObjectElem elem) =
      replaceText (#definition_textbox_id elem) (get_definition (#name elem))
		    
    and postprocess (elem' as ViewObjectElem elem) =
    ( updateTypeLabel elem';
      updateDefinitionTextbox elem' )

    val terminate = destroy
  )
					      
  end (* local *)
    
  (* ********************************************************************** *)

  (* Zeige die Definition einer Funktion an *)
  fun viewObject () =
      if not (Listbox.State.valid_position ())
      then StdDialog.error "No function selected!"
      else let val name = List.nth (!Listbox.State.names, !Listbox.State.position)
	       val title = [ WinTitle ("View: " ^ name) ]
	   in ViewObjectWindows.create (
	       title, ViewObjectElem { win_id = newWinId (),
				       type_label_id = newWidgetId (),
				       definition_textbox_id = newWidgetId (),
				       name = name } )
	   end

  val _ = (Listbox.viewObjectRef := viewObject)

  fun redraw_listbox F ()  = (F (); Listbox.redraw ())

  fun list_names_with_types () = redraw_listbox (fn () => ShowName := ShowWithType) ()
  fun list_only_names ()       = redraw_listbox (fn () => ShowName := ShowWithoutType) ()


  (* --- Menus of the Signature Window --- *)
	
  fun MItem (text, action) = MCommand [ Text text, Command action ]
					 
  val objectsMenu =
    MenuButton ( newWidgetId (), true,
                 [ MItem ( " Types ",     redraw_listbox (fn () => ObjectsFlag := ShowTypes) ),
                   MItem ( " Functions ", redraw_listbox (fn () => ObjectsFlag := ShowFunctions) ),
                   MItem ( " Rules ",     redraw_listbox (fn () => ObjectsFlag := ShowRules) ),
                   MSeparator,
                   MItem ( " All ",       redraw_listbox (fn () => ObjectsFlag := ShowAll) ) ],
                 [ Side Left, Fill X ],
		 [ Text " Objects " ], [] )

  val originMenu =
    MenuButton ( newWidgetId (), true,
                 [ MItem ( " ASM-SL Primitives ", redraw_listbox (fn () => OriginFlag := OnlyBuiltin) ),
                   MItem ( " Selected File ", redraw_listbox (fn () => OriginFlag := FromSelectedFile) ),
                   MItem ( " Specification (All Files) ", redraw_listbox (fn () => OriginFlag := FromSpecification) ),
                   MSeparator,
                   MItem ( " All ", redraw_listbox (fn () => OriginFlag := AllOrigins) ) ],
                 [ Side Left, Fill X ],
		 [ Text " Origin " ], [] )

  val optionsMenu =
    MenuButton ( newWidgetId (), true,
                 [ MItem ( " Show names with types ", list_names_with_types ),
                   MItem ( " Show only names ",       list_only_names ),
                   MSeparator,
                   MItem ( " Order of definition ",   redraw_listbox (fn _ => OrderFlag := OrderOfDefinition) ),
                   MItem ( " Alphabetical order ",    redraw_listbox (fn _ => OrderFlag := AlphabeticalOrder) ) ],
                 [ Side Left, Fill X ],
		 [ Text " Options " ], [] )

  fun FunctionMenu () = 
    Frame ( newWidgetId (),
      [ Label ( newWidgetId (),  [Side Left, Fill X], [ Text "Filter by: "], [] ),
	objectsMenu, originMenu, optionsMenu ],
      [ Side Top, Fill X ], [], [] )

  val closeWindowRef = ref (fn () => ())
  fun buttons () =
      Frame ( newWidgetId (),
	      (map LeftExpandButton [ (" View ", viewObject),
				      (* ("Define", defineObject),*)
				      (" Close ", fn () => (!closeWindowRef)()) ]),
    [Fill X], [Relief Ridge, Borderwidth 2], []
  )

  structure MainWindow = MakeWindow (
    val win_id_str = SOME signatureWindow_winId
    fun window id = ( [ WinTitle (title ()) ],
		      [ FunctionMenu (), Listbox.widget (), buttons () ] )
    fun init ()        = Listbox.register ()
    fun postprocess () = Listbox.redraw ()
    fun terminate ()   = ( ViewObjectWindows.closeAll (); Listbox.unregister () )
    val closeWindowRef = closeWindowRef
  )

  fun select () = MainWindow.select ()
end
