structure StringSet = struct
  structure Set = BinarySetFn ( struct
    type ord_key = string
    val compare = String.compare
  end )
  open Set
  fun fromList L = List.foldl add' empty L
end


structure StringMap = struct
  structure Map = BinaryMapFn ( struct
    type ord_key = string
    val compare   = String.compare
  end )
  open Map
  fun fromList L = List.foldl insert' empty L
end


structure StringArray =
struct
  structure A :MONO_ARRAY =
  struct
    open Array
    type elem   = string
    type vector = elem Vector.vector
    type array  = elem Array.array
  end
  open A

  structure Sort = ArrayQSortFn (A)
  fun sort arr = Sort.sort String.compare arr

  structure BSearch = BSearchFn (A)
  fun bsearch arr = BSearch.bsearch String.compare arr

  fun fromListSorted L =
    let val arr = A.fromList L
        val _   = sort arr
    in arr
    end
end



structure RandomChoice =
struct
  local
    (* --- NOTE: the following line of code is likely to raise an overflow some day after 2038... --- *)
    val rand = ref (Rand.mkRandom (Word31.fromLargeInt (Time.toSeconds (Time.now ()))))
  in 
    fun random (lo, hi) = (Rand.range (lo, hi)) ((!rand) ())
  end
end
