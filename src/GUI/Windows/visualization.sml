structure Arrow =
struct
  val PI = 3.14159265358979
  exception Atan4
  fun atan4 ((x1, y1), (x2, y2)) =
    if Real.compare (x1, x2) = EQUAL
    then if y1 < y2
         then PI / 2.0
         else if y1 > y2
         then 3.0 * PI / 2.0
         else raise Atan4
    else if x1 < x2
    then if Real.compare (y1, y2) = LESS orelse Real.compare (y1, y2) = EQUAL
         then Math.atan ((y2 - y1) / (x2 - x1))
         else Math.atan ((y2 - y1) / (x2 - x1)) + 2.0 * PI
    else PI + Math.atan ((y2 - y1) / (x2 - x1))

  val arrow_top_radius = 5.0
  val arrow_top_angle  = PI / 6.0

  fun round x =
    if x > 0.0 then Real.ceil (x - 0.5) else Real.floor (x + 0.5)

  fun arrow ((x1,y1),(x2,y2)) =
    let val r = arrow_top_radius
        val (rx1,ry1,rx2,ry2) = (real x1, real y1, real x2, real y2)
        val base_angle = atan4 ((rx2,ry2),(rx1,ry1))
        val angle1 = base_angle - PI / 6.0
        val angle2 = base_angle + PI / 6.0
    in [ [(x1, y1), (x2, y2)],
         [(x2, y2), (round (rx2 + r * (Math.cos angle1)), round (ry2 + r * (Math.sin angle1)))],
         [(x2, y2), (round (rx2 + r * (Math.cos angle2)), round (ry2 + r * (Math.sin angle2)))] ]
    end
end

(******************************************************************************)

structure VisualStructures =
struct
  open SmlTk SmlTk21

  datatype V_COLOUR =
    V_Black | V_White | V_Grey  | V_Blue
  | V_Green | V_Red   | V_Brown | V_Yellow
  | V_Transparent

  datatype V_ANCHOR_KIND =
    V_North | V_NorthEast | V_East | V_SouthEast
  | V_South | V_SouthWest | V_West | V_NorthWest
  | V_Center

  type V_COORD = int * int

  datatype V_PRIMITIVE =
    V_Rectangle of V_COORD * V_COORD * V_COLOUR * V_COLOUR * int
  | V_Oval of V_COORD * V_COORD * V_COLOUR * V_COLOUR * int
  | V_Line of V_COORD list * V_COLOUR * int
  | V_Arrow of V_COORD * V_COORD * V_COLOUR * int
  | V_Text of string * V_COORD * V_COLOUR * V_COLOUR * V_ANCHOR_KIND

  fun compare (vp1 :V_PRIMITIVE, vp2 :V_PRIMITIVE) :order =
    let fun v_primitive_tag x = case x of V_Rectangle _ => 1 | V_Oval _ => 2 | V_Line _ => 3 | V_Arrow _ => 4 | V_Text _ => 5
        fun v_anchor_kind_tag x = case x of V_North => 1 | V_NorthEast => 2 | V_East => 3 | V_SouthEast => 4 | V_South => 5 | V_SouthWest => 6 | V_West => 7 | V_NorthWest => 8 | V_Center => 9
        fun v_colour_tag x = case x of V_Black => 1 | V_White => 2 | V_Grey => 3 | V_Blue => 4 | V_Green => 5 | V_Red => 6 | V_Brown => 7 | V_Yellow => 8 | V_Transparent => 9
        fun coord_compare (coord1, coord2) = Pair.compare (Int.compare, Int.compare) (coord1, coord2)
        fun colour_compare (col1, col2) = Int.compare (v_colour_tag col1, v_colour_tag col2)
        fun anchor_kind_compare (ak1, ak2) = Int.compare (v_anchor_kind_tag ak1, v_anchor_kind_tag ak2)
    in case (vp1, vp2) of
          (V_Rectangle (args1 as (coord1a, coord1b, col1a, col1b, width1)), V_Rectangle (args2 as (coord2a, coord2b, col2a, col2b, width2))) =>
            Tuple5.compare (coord_compare, coord_compare, colour_compare, colour_compare, Int.compare) (args1, args2)
        | (V_Oval (args1 as (coord1a, coord1b, col1a, col1b, width1)), V_Oval (args2 as (coord2a, coord2b, col2a, col2b, width2))) =>
            Tuple5.compare (coord_compare, coord_compare, colour_compare, colour_compare, Int.compare) (args1, args2)
        | (V_Line (args1 as (coords1, col1, width1)), V_Line (args2 as (coords2, col2, width2))) =>
            Triple.compare (List_.compare coord_compare, colour_compare, Int.compare) (args1, args2)
        | (V_Arrow (args1 as (coords1a, coord1b, col1, width1)), V_Arrow (args2 as (coords2a, coords2b, col2, width2))) =>
            Tuple4.compare (coord_compare, coord_compare, colour_compare, Int.compare) (args1, args2)
        | (V_Text (args1 as (txt1, coord1, col1a, col1b, ak1)), V_Text (args2 as (txt2, coord2, col2a, col2b, ak2))) =>
            Tuple5.compare (String.compare, coord_compare, colour_compare, colour_compare, anchor_kind_compare) (args1, args2)
        | (_, _) => Int.compare (v_primitive_tag vp1, v_primitive_tag vp2)
    end

  structure VPrimitiveMap = struct
    structure Map = RedBlackMapFn (
      struct
        type ord_key = V_PRIMITIVE
        val compare = compare
      end )
    open Map
    fun fromList L = List.foldl insert' empty L
  end

  structure VPrimitiveSet = struct
    structure Set = RedBlackSetFn (
      struct
        type ord_key = V_PRIMITIVE
        val compare = compare
      end )
    open Set
    fun fromList L = List.foldl add' empty L
  end

  structure ASM2ML =
  struct
    local open ASM_Value
    in
      fun v_colour (c :VALUE) :V_COLOUR =
      ( case c of
          CELL (s, _) =>
          ( case s of
              "V_Black"  => V_Black
            | "V_White"  => V_White
            | "V_Grey"   => V_Grey
            | "V_Blue"   => V_Blue
            | "V_Green"  => V_Green
            | "V_Red"    => V_Red
            | "V_Brown"  => V_Brown
            | "V_Yellow" => V_Yellow
            | "V_Transparent" => V_Transparent
            | _ => V_Grey )
        | _ => V_Grey )

      fun v_anchor_kind (c :VALUE) :V_ANCHOR_KIND =
      ( case c of
          CELL (s, _) =>
          ( case s of
              "V_North"     => V_North
            | "V_NorthEast" => V_NorthEast
            | "V_East"      => V_East
            | "V_SouthEast" => V_SouthEast
            | "V_South"     => V_South
            | "V_SouthWest" => V_SouthWest
            | "V_West"      => V_West
            | "V_NorthWest" => V_NorthWest
            | "V_Center"    => V_Center
            | _ => V_Center )
        | _ => V_Center )

      fun v_primitive (c :VALUE) :V_PRIMITIVE =
        let exception Problem
        in case c of
             CELL ( "V_Rectangle",
               TUPLE [ TUPLE [INT x1, INT y1], TUPLE [INT x2, INT y2], outline_col_value, fill_col_value, INT width ] ) =>
                   let val (x1,y1,x2,y2,width) = Tuple5.map IntInf.toInt (x1,y1,x2,y2,width)
                   in V_Rectangle ((x1, y1), (x2, y2), v_colour outline_col_value, v_colour fill_col_value, width)
                   end
           | CELL ( "V_Oval",
               TUPLE [ TUPLE [INT x1, INT y1], TUPLE [INT x2, INT y2], outline_col_value, fill_col_value, INT width ] ) =>
                   let val (x1,y1,x2,y2,width) = Tuple5.map IntInf.toInt (x1,y1,x2,y2,width)
                   in V_Oval ((x1, y1), (x2, y2), v_colour outline_col_value, v_colour fill_col_value, width)
                   end
           | CELL ( "V_Line", TUPLE [ x_y_list, col_value, INT width ] ) =>
               let val x_y_list' = ((ASM2ML.list x_y_list) handle _ => raise Problem)
                   val x_y_list'' = (map (fn (TUPLE [INT x, INT y]) => (IntInf.toInt x, IntInf.toInt y)
                                           | _ => raise Problem) x_y_list')
                   val width = IntInf.toInt width
               in V_Line (x_y_list'', v_colour col_value, width)
               end
           | CELL ( "V_Arrow", TUPLE [ TUPLE [INT x1, INT y1], TUPLE [INT x2, INT y2], col_value, INT width ] ) =>
               let val (x1,y1,x2,y2,width) = Tuple5.map IntInf.toInt (x1,y1,x2,y2,width)
               in V_Arrow ((x1, y1), (x2, y2), v_colour col_value, width)
               end
           | CELL ( "V_Text",
               TUPLE [ STRING text, TUPLE [INT x, INT y], fg_col_value, bg_col_value, anchor_value ] ) =>
                 let val (x,y) = Pair.map IntInf.toInt (x,y)
                 in V_Text (text, (x, y), v_colour fg_col_value, v_colour bg_col_value, v_anchor_kind anchor_value)
                 end
           | _ => raise Problem
        end
        handle Problem => V_Text ("Error in visualization primitive!", (2, 2), V_Red, V_Green, V_NorthWest)

      fun v_primitive_list (x :VALUE) :V_PRIMITIVE list =
        let exception Problem
            val L = ((ASM2ML.list x) handle _ => raise Problem)
        in map v_primitive L
        end
        handle Problem => [ V_Text ( "Visualization error (not a list of V_PRIMITIVEs)!",
                                     (2, 2), V_Red, V_Green, V_NorthWest ) ]
    end
  end

  fun smltk_color transparent_allowed c =
    case c of
      V_Black  => Black
    | V_White  => White
    | V_Grey   => Grey
    | V_Blue   => Blue
    | V_Green  => Green
    | V_Red    => Red
    | V_Brown  => Brown
    | V_Yellow => Yellow
    | V_Transparent => if transparent_allowed then NoColor else Grey

  fun smltk_anchor_kind ak =
    case ak of
      V_North     => North
    | V_NorthEast => NorthEast
    | V_East      => East       
    | V_SouthEast => SouthEast
    | V_South     => South      
    | V_SouthWest => SouthWest
    | V_West      => West       
    | V_NorthWest => NorthWest
    | V_Center    => Center     

  local
    val smltk_color = smltk_color true
  in
    fun mkRectangle id ((x1, y1), (x2, y2), outline_col, fill_col, width) =
    [ ( CRectangle ( id, (x1, y1), (x2, y2),
                     [ Outline (smltk_color outline_col), FillColor (smltk_color fill_col),
                       OutlineWidth width ], [ (* no bindings *) ] ),
        id ) ]

    fun mkOval id ((x1, y1), (x2, y2), outline_col, fill_col, width) =
    [ ( COval ( id, (x1, y1), (x2, y2),
                [ Outline (smltk_color outline_col), FillColor (smltk_color fill_col),
                  OutlineWidth width ], [ (* no bindings *) ] ),
        id ) ]
  end (* local *)

  local
    val smltk_color = smltk_color false
  in
    fun mkLine id (x_y_list, col, width) =
    [ ( CLine ( id, x_y_list, [ FillColor (smltk_color col), OutlineWidth width ],
                [ (* no bindings *) ] ),
        id ) ]

    fun mkText id (text, (x, y), foregnd_col, backgnd_col, anchor_kind) =
      let val widget = CWidget ( id, (x, y), newCItemFrameId (),
                         [ Label (newWidgetId (), [],
                           [ Text text,
                             Background (smltk_color backgnd_col),
                             Foreground (smltk_color foregnd_col) ],
                         [ (* no bindings *) ]) ],
                         [ ], [ Anchor (smltk_anchor_kind anchor_kind) ], [] )
      in [(widget, id)]
      end

    fun mkArrow ((x1, y1), (x2, y2), col, width) =
      let fun mkLine x =
            let val id = newCItemId ()
            in (CLine (id, x, [ FillColor (smltk_color col), OutlineWidth width ], []), id)
            end
      in map mkLine (Arrow.arrow ((x1,y1),(x2,y2)))
      end
  end (* local *)

  fun canvasItems (v_prim :V_PRIMITIVE) :(CItem * CItemId) list =
    case v_prim of
      V_Rectangle args => mkRectangle (newCItemId ()) args
    | V_Oval args      => mkOval (newCItemId ()) args
    | V_Line args      => mkLine (newCItemId ()) args
    | V_Arrow args     => mkArrow args
    | V_Text args      => mkText (newCItemId ()) args
end

(******************************************************************************)

structure VisualizationWindow =
struct
  open GUI_Misc VisualStructures
  fun debug s L = GUI_Misc.debug false "VisualizationWindow" s L

  val canvasId = mkWidgetId "canvas"
  val canvas   = Canvas (canvasId, RightScb, [], [Side Top, Expand true, Fill Both], [], [])

  (* --- local state --- *)

  local
    structure VPrimitiveSet = VisualStructures.VPrimitiveSet
    structure VPrimitiveMap = VisualStructures.VPrimitiveMap
    val v_prim_set_ref = ref (VPrimitiveSet.empty :VPrimitiveSet.set)
    val contents_ref   = ref (VPrimitiveMap.empty :((CItem * CItemId) list) VPrimitiveMap.map)

    fun delete_one (CItem_CItemId_list: (CItem * CItemId) list) =
      ( debug "delete_one $1" [ Output.toString (Write.list Write.string) (map (fn x => String_.replace "(<CItem>, $1)" [ mkCItemString (#2 x) ]) CItem_CItemId_list) ];
        app (fn x => (delCItem canvasId (#2 x)) handle _ => ()) CItem_CItemId_list )

    fun add_one (CItem_CItemId_list :(CItem * CItemId) list) =
      ( debug "add_one $1" [ Output.toString (Write.list Write.string) (map (fn x => String_.replace "(<CItem>, $1)" [ mkCItemString (#2 x) ]) CItem_CItemId_list) ];
        app ((addCItem canvasId) o #1) CItem_CItemId_list )
      
    fun put_one v_primitive =
      let val CItem_CItemId_list = canvasItems v_primitive
      in debug "put_one $1" [ Output.toString (Write.list Write.string) (map (fn x => String_.replace "(<CItem>, $1)" [ mkCItemString (#2 x) ]) CItem_CItemId_list) ];
         (v_primitive, CItem_CItemId_list)
      end

    fun put (v_primitive_list :V_PRIMITIVE list) =
      ( debug "put" [];
        let val new_v_prim_set = VPrimitiveSet.fromList v_primitive_list
            val to_be_deleted  = VPrimitiveSet.difference (!v_prim_set_ref, new_v_prim_set)
            val to_be_added    = VPrimitiveSet.difference (new_v_prim_set, !v_prim_set_ref)
            val contents_to_be_added = VPrimitiveMap.fromList (map put_one (VPrimitiveSet.listItems to_be_added))
        in VPrimitiveSet.app (fn v_prim => delete_one (VPrimitiveMap.lookup (!contents_ref, v_prim))) to_be_deleted;
           VPrimitiveSet.app (fn v_prim => add_one (VPrimitiveMap.lookup (contents_to_be_added, v_prim))) to_be_added;
           contents_ref := VPrimitiveMap.unionWith (fn (old, new) => new)
                                                   ( VPrimitiveMap.fromList (map (fn v_prim => (v_prim, VPrimitiveMap.lookup (!contents_ref, v_prim)))
                                                                                 (VPrimitiveSet.listItems (VPrimitiveSet.difference (!v_prim_set_ref, to_be_deleted)))),
                                                     contents_to_be_added );
           v_prim_set_ref := new_v_prim_set
        end )
  in
    fun delete_all () = put []
                            
    fun redraw_canvas () =
      let val _ = debug "redraw_canvas" [];
          val asm_vprim_list = VisualOptions.eval_visualization_term ()
          val v_primitive_list = VisualStructures.ASM2ML.v_primitive_list asm_vprim_list
      in put v_primitive_list
      end
      handle VisualOptions.VisualizationTerm =>
             ( StdDialog.error "Error in visualization term \
                                   \(\"Options\", \"Visualization\", \"Visualization Term\")" )
           | _ =>
             ( StdDialog.error "Visualization error (unrecognized) " )
  end

  (* --- end local state --- *)

  fun mkframe L =
    Frame ( newWidgetId (), L, [ Side Top, Expand true, Fill Both ],
            [ Relief Ridge, Borderwidth 2 ], [])


  val closeWindowRef = ref (fn () => ())
  fun closeButton () = RightButton ("Close", fn _ => (!closeWindowRef) ())
                                                       
  structure Canvas_SDW = MakeStateDepWidget (
    val widget = canvas
    fun redraw id = redraw_canvas ()
    val dependencies = [ ASM_GUI_State.gse_asm_state, Filelist.gse_filelist, VisualOptions.gse_visual_options ]
  )
  structure MainWindow = MakeWindow (
    val terminated = ref false
    val win_id_str = SOME (mkWinId "visualizationWindow")
    fun window id = ( [ WinTitle "State Visualization Window" ],
                      [ mkframe [ canvas ], closeButton () ] )
    fun init () = ( debug "MainWindow.init" [];
                    terminated := false;
                    Canvas_SDW.register () )
    fun postprocess () = Canvas_SDW.redraw ()
    fun terminate () = ( debug "MainWindow.terminate" [];
                         if not (!terminated)
                         then ( delete_all ();
                                Canvas_SDW.unregister ();
                                terminated := true )
                         else () )
    val closeWindowRef = closeWindowRef
  )

  fun select () = MainWindow.select ()
  fun terminate () = MainWindow.terminate ()
  fun close ()  = MainWindow.close ()
end
