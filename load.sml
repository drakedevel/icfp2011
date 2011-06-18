signature ALLOCATOR =         (* imperative *)
sig
  exception OOM
  type allocr                   (* allocator *)
  val new : unit -> allocr
  val copy : allocr -> allocr
  val alloc : allocr -> LTG.slotno
  val use : allocr -> LTG.slotno -> unit
  (* raises OOM /before/ allocating anything if would overflow. *)
  val allocMany : allocr -> int -> LTG.slotno list
  val free : allocr -> LTG.slotno -> unit
  val withSlot : allocr -> (LTG.slotno -> 'a) -> 'a
end

local
    open Util infixr 0 $
    open LTG infix &
    structure E = Evaluator
    type move = E.move
    val move = E.move
    val L = E.L
    val R = E.R
in

  (* We want some sort of scheme to reserve 
   * slots we are otherwising using *)
  structure Allocator : ALLOCATOR =
  struct
    structure IM = IntMap (* sets don't have firsti; argh. *)

    exception OOM
    type allocr = unit IM.map ref (* free list *)

    fun add S x = IM.bind S x ()

    fun new () = ref $ foldl (fn (i, S) => add S i) IM.empty $ upto (max_slot+1)
    fun copy (ref xs) = ref xs

    fun alloc (R as ref S) =
        case IM.firsti S of
            NONE => raise OOM
          | SOME (x, ()) => (R := IM.delete S x; x)

    fun allocMany (R as ref S) n =
        let fun take 0 = []
              | take n = alloc R :: take (n-1)
        in if IM.count S < n then raise OOM
           else take n
        end

    fun free (R as ref S) x = R := add S x

    fun withSlot a f =
        let val s = alloc a
        in after f (free a) s
        end

    fun use a x = a := IM.delete (!a) x
  end

  (* these are dumb loader functions.
   * they assume our opponent never interacts with us.
   *)

  structure Load =
  struct

    (* converts an int to binary, big-endian  *)
    fun toBinary (x : value) : bool list =
        let (* little-endian helper *)
            fun bin 0 = []
              | bin x = odd x :: bin (x div 2)
        in rev (bin x)
        end

    (* Generates a list of "ops" to generate an int. An op is double or succ. *)
    fun opsForInt n = intercalate [CDbl] $ map (fn true => [CSucc] | false => []) $ toBinary n
    val numOpsForInt = length o opsForInt (* FIXME don't need to construct list *)

    (* generates a sequence of moves to store v into dest *)
    fun intNoPut dest v = R dest CZero :: map (fn c => L c dest) (opsForInt v)
    fun int (dest : slotno) (v : value) : move list = L CPut dest :: intNoPut dest v

    (* basically the same as above, but as only one function, and returning
     * the comb that computes the value, rather than the move list, since the
     * comb needs to be shifted into place.  NOTE: %CZero, rather than %%CZero
     * this is what we want, since we want to generate a card in the end.
     *)
    fun encode n : comb =
        foldl (fn (card, exp) => CApp (%card, exp)) (%CZero) (opsForInt n)

     (*if f!d == F, then run_moves (shift d E) will leave (f!d) == CApp(F,E)*)
     fun shift (dest : slotno) (CApp (e1,e2)) =
         [L CK dest, L CS dest] @ shift dest e1 @ shift dest e2
       | shift (dest : slotno) (%c) = [R dest c]
       | shift (dest : slotno) (CVal n) = shift dest (encode n)

     (* checks if an expr is valid to load *)
     fun checkExpr (% c) = ()
       | checkExpr (CVal v) = ()
       | checkExpr (CApp (e1, e2)) = (checkExpr e1; checkExpr e2)
       | checkExpr (e1 & e2) = raise Fail "invalid expr to load; contains &"
       | checkExpr (CVar _) = raise Fail "invalid expr to load; contains variable"

     (* shift (encode x) takes 1 + 3 * (# ops to apply)
      * where an op is double or succ
      *
      * loading n into a slot takes 2 + #ops (+1 to put, +1 to zero)
      *
      * so loading n into slot i and then shift (get i) takes:
      *)
     fun shiftInt (A : Allocator.allocr) dest n =
         (* FIXME: can determine the shorter without generating both lists *)
         let val shiftEncode = shift dest (encode n)
             val tempShift = Allocator.withSlot A
                             (fn i => int i n @
                                      shift dest (CApp (%CGet, encode i)))
         in minBy length shiftEncode tempShift
         end

     fun loadDumb d (% c) = [R d c]
       | loadDumb d (CVal v) = int d v
       | loadDumb d (CApp (%c, e)) = loadDumb d e @ [L c d]
       | loadDumb d (CApp (e1, e2)) = loadDumb d e1 @ shift d e2

     (* A fast, constant-space loader.
      *
      * Loads a combinator using a modified version of sully's algorithm. Allocates temporary slots
      * to build numbers when building them via shift would take longer than building them in a temp
      * and loading it.
      *
      * TODO: should take game state and determine whether it needs to load I into dest.
      *)
     fun load (A : Allocator.allocr) (dest : slotno) (expr : comb) : move list =
         let val () = checkExpr expr
             fun left x = L x dest
             val right = R dest
             (* right-applies a number into dest, using temps if necessary *)

             (* right-applies an expression into dest *)
             fun shift (% c) = [right c]
               | shift (CApp (e1, e2)) = [left CK, left CS] @ shift e1 @ shift e2
               | shift (CVal v) = shiftInt A dest v

             (* loads an expression into dest, assuming dest contains I. *)
             fun load (% c) = [right c]
               | load (CApp (%c, e)) = load e @ [left c]  (* optimization *)
               | load (CApp (e1, e2)) = load e1 @ shift e2
               | load (CVal v) = intNoPut dest v
         in left CPut (* load I into dest *) :: load expr
         end

  end

end
