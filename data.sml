(* A bunch of data structures that get used all over the place. *)
(* I really wouldn't mind type classes. *)

(* Build an ORD_KEY that does lexicographic ordering from two other ORD_KEYs *)
functor PairOrdKey (structure K1 : ORD_KEY
                    structure K2 : ORD_KEY)
        : ORD_KEY =
struct
  type ord_key = K1.ord_key * K2.ord_key
  fun compare ((k1, k2), (k1', k2')) =
      case K1.compare (k1, k1') of
          EQUAL => K2.compare (k2, k2')
        | order => order
end

structure IntKey =
struct
  type ord_key = int
  val compare = Int.compare
end

structure IntMap2 = GoodMapFn(IntKey)
structure IntSet = GoodSetFn(IntMap2)
structure IntPairKey = PairOrdKey(structure K1 = IntKey structure K2 = IntKey)
structure IntPairMap = GoodMapFn(IntPairKey)
structure IntPairSet = GoodSetFn(IntPairMap)

(* Then you can do... *)
structure WordMap = GoodMapWordFn(WordWordable)
structure IntMap = GoodMapWordFn(IntWordable)
