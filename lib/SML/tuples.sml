(*
##
## "tuples.sml", G. Del Castillo, 2000
##
##
##
*)


structure Single =
struct
  val read  = Read.single
  val write = Write.single
end


structure Pair =
struct
  fun map f (x1, x2) = (f x1, f x2)
  fun apply (f1, f2) (x1, x2) = (f1 x2, f2 x2)

  fun compare (cmpfun1, cmpfun2) ((x1,x2),(y1,y2)) =
    case cmpfun1 (x1,y1) of
      LESS    => LESS
    | GREATER => GREATER
    | EQUAL   => cmpfun2 (x2,y2)

  val read  = Read.triple
  val write = Write.triple
end


structure Triple =
struct
  fun map f (x1, x2, x3) = (f x1, f x2, f x3)
  fun apply (f1, f2, f3) (x1, x2, x3) = (f1 x2, f2 x2, f3 x3)

  fun compare (cmpfun1, cmpfun2, cmpfun3) ((x1,x2,x3),(y1,y2,y3)) =
    case Pair.compare (cmpfun1, cmpfun2) ((x1,x2),(y1,y2)) of
      LESS    => LESS
    | GREATER => GREATER
    | EQUAL   => cmpfun3 (x3,y3)

  val read  = Read.triple
  val write = Write.triple
end


structure Tuple4 =
struct
  fun map f (x1, x2, x3, x4) = (f x1, f x2, f x3, f x4)
  fun apply (f1, f2, f3, f4) (x1, x2, x3, x4) = (f1 x2, f2 x2, f3 x3, f4 x4)

  fun compare (cmpfun1, cmpfun2, cmpfun3, cmpfun4) ((x1,x2,x3,x4),(y1,y2,y3,y4)) =
    case Triple.compare (cmpfun1, cmpfun2, cmpfun3) ((x1,x2,x3),(y1,y2,y3)) of
      LESS    => LESS
    | GREATER => GREATER
    | EQUAL   => cmpfun4 (x4,y4)

  val read = Read.tuple4
  val write = Write.tuple4
end


structure Tuple5 =
struct
  fun map f (x1, x2, x3, x4, x5) = (f x1, f x2, f x3, f x4, f x5)
  fun apply (f1, f2, f3, f4, f5) (x1, x2, x3, x4, x5) = (f1 x2, f2 x2, f3 x3, f4 x4, f5 x5)

  fun compare (cmpfun1, cmpfun2, cmpfun3, cmpfun4, cmpfun5) ((x1,x2,x3,x4,x5),(y1,y2,y3,y4,y5)) =
    case Tuple4.compare (cmpfun1, cmpfun2, cmpfun3, cmpfun4) ((x1,x2,x3,x4),(y1,y2,y3,y4)) of
      LESS    => LESS
    | GREATER => GREATER
    | EQUAL   => cmpfun5 (x5,y5)

  val read  = Read.tuple5
  val write = Write.tuple5
end
