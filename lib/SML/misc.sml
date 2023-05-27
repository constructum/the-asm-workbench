(*
##
## "misc.ml", G. Del Castillo, 1996-2000
##
##
##
*)


structure Misc =
struct
  fun id x  = x
  fun const x = fn _ => x
  val K = const
  fun flip f x y = f y x
  fun fnpair (f1, f2) = fn (x, y) => (f1 x, f2 y)

  fun fixpoint F E =
    let fun fp x =
	  let val x' = F x
	  in if x = x' then x' else fp x'
          end
    in fp E
    end

  fun iterate n f x =
    if n <= 0
    then x
    else iterate (n-1) f (f x)

  fun map2 (f1, f2) L = map (fn (x1, x2) => (f1 x1, f2 x2)) L
  fun map3 (f1, f2, f3) L = map (fn (x1, x2, x3) => (f1 x1, f2 x2, f3 x3)) L
  fun map4 (f1, f2, f3, f4) L = map (fn (x1, x2, x3, x4) => (f1 x1, f2 x2, f3 x3, f4 x4)) L
  fun map5 (f1, f2, f3, f4, f5) L = map (fn (x1, x2, x3, x4, x5) => (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5)) L
                 
  fun flatten3l ((x1,x2),x3) = (x1,x2,x3)
  fun flatten4l (((x1,x2),x3),x4) = (x1,x2,x3,x4)
  fun flatten5l ((((x1,x2),x3),x4),x5) = (x1,x2,x3,x4,x5)
  fun flatten6l (((((x1,x2),x3),x4),x5),x6) = (x1,x2,x3,x4,x5,x6)
  fun flatten7l ((((((x1,x2),x3),x4),x5),x6),x7) = (x1,x2,x3,x4,x5,x6,x7)
  fun flatten8l (((((((x1,x2),x3),x4),x5),x6),x7),x8) = (x1,x2,x3,x4,x5,x6,x7,x8)
  fun flatten9l ((((((((x1,x2),x3),x4),x5),x6),x7),x8),x9) = (x1,x2,x3,x4,x5,x6,x7,x8,x9)
                                        
  fun flatten3r (x1,(x2,x3)) = (x1,x2,x3)
  fun flatten4r (x1,(x2,(x3,x4))) = (x1,x2,x3,x4)
  fun flatten5r (x1,(x2,(x3,(x4,x5)))) = (x1,x2,x3,x4,x5)
  fun flatten6r (x1,(x2,(x3,(x4,(x5,x6))))) = (x1,x2,x3,x4,x5,x6)
  fun flatten7r (x1,(x2,(x3,(x4,(x5,(x6,x7)))))) = (x1,x2,x3,x4,x5,x6,x7)
  fun flatten8r (x1,(x2,(x3,(x4,(x5,(x6,(x7,x8))))))) = (x1,x2,x3,x4,x5,x6,x7,x8)
  fun flatten9r (x1,(x2,(x3,(x4,(x5,(x6,(x7,(x8,x9)))))))) = (x1,x2,x3,x4,x5,x6,x7,x8,x9)
                                        
  val zip = ListPair.zip
  val unzip = ListPair.unzip

  exception ZipWith
  fun zipWith f (L1, L2) =
    let fun Z ([], [])       = []
          | Z (x::xs, y::ys) = f(x,y) :: (Z (xs, ys))
          | Z (_, _)         = raise ZipWith
    in Z (L1, L2)
    end

  fun zip3 ([], _, _) = []
    | zip3 (_, [], _) = []
    | zip3 (_, _, []) = []
    | zip3 (x1::xs1, y1::ys1, z1::zs1) = (x1,y1,z1)::(zip3 (xs1,ys1,zs1))

  fun unzip3 [] = ([], [], [])
    | unzip3 ((x1,y1,z1)::rest) =
        let val (xs1, ys1, zs1) = unzip3 rest
        in (x1 :: xs1, y1 :: ys1, z1 :: zs1)
        end

  fun timing f x =
    let
      open Timer
      val real_start = startRealTimer ()
      val cpu_start  = startCPUTimer ()
      val z = f x
      val cpu  = checkCPUTimer cpu_start
      val real = checkRealTimer real_start
    in
      { usr = Time.toReal (#usr cpu), sys = Time.toReal (#sys cpu), total = Time.toReal real }
(*
        "usr: ", Real.toString (),
        ",  sys: ", Real.toString (),
(*      ",  gc: ", Real.toString (Time.toReal (#gc cpu)), *)
        "  (total time: ", Real.toString (Time.toReal real), ")\n"
      ]);
      z *)
    end
end


structure List =
struct
  open List

  fun foldll f b L = foldl (fn (a, b) => f (b, a)) b L

  fun compare _ ([], []) = EQUAL
    | compare _ (_, [])  = GREATER
    | compare _ ([], _)  = LESS
    | compare cmpfun (e1::l1, e2::l2) =
	case cmpfun (e1, e2) of
	  LESS    => LESS
	| GREATER => GREATER
	| EQUAL   => compare cmpfun (l1, l2)
end
