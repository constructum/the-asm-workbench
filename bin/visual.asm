//
// "visual.asm" - G. Del Castillo, 1999
//    data structures needed for state visualization
//    (automatically loaded when the debugger starts)
//

freetype V_COLOUR ==
{ V_Black, V_White, V_Grey, V_Blue,
  V_Green, V_Red, V_Brown, V_Yellow,
  V_Transparent }

freetype V_ANCHOR_KIND ==
{ V_North, V_NorthEast, V_East, V_SouthEast,
  V_South, V_SouthWest, V_West, V_NorthWest,
  V_Center }

typealias V_COORD == INT * INT

freetype V_PRIMITIVE ==
{ V_Rectangle : V_COORD * V_COORD * V_COLOUR * V_COLOUR * INT,
  V_Oval : V_COORD * V_COORD * V_COLOUR * V_COLOUR * INT,
  V_Line : [V_COORD] * V_COLOUR * INT,
  V_Arrow : V_COORD * V_COORD * V_COLOUR * INT,
  V_Text : STRING * V_COORD * V_COLOUR * V_COLOUR * V_ANCHOR_KIND }
