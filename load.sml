signature ALLOCATOR =         (* imperative *)
sig
  exception OOM
  exception AlreadyInUse
  type allocr                   (* allocator *)
  val new : LTG.board -> allocr
  val copy : allocr -> allocr
  val allocFilter : (LTG.slotno -> bool) -> allocr -> LTG.slotno
  val alloc : allocr -> LTG.slotno
  val use : allocr -> LTG.slotno -> unit
  (* raises OOM /before/ allocating anything if would overflow. *)
  val allocMany : allocr -> int -> LTG.slotno list
  val free : allocr -> LTG.slotno -> unit
  val freeMany : allocr -> LTG.slotno list -> unit
  val update : allocr -> LTG.board -> unit
  val withSlot : allocr -> (LTG.slotno -> 'a) -> 'a
  val aslr : allocr -> allocr
  val cheap : allocr -> allocr
  val board : allocr -> LTG.board
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
    val ass_random = Random.rand (Int.fromLarge (LargeInt.mod ((Time.toMicroseconds (Time.now ())), LargeInt.fromInt (valOf Int.maxInt))), 0xF00F)
    structure IM = IntMap (* sets don't have firsti; argh. *)

    exception OOM
    exception AlreadyInUse
    type allocr = ((unit IM.map ref) * (unit IM.map ref) * bool * board) (* live list, free list, aslr *)

    fun add S x = IM.bind S x ()

    fun new b = let val all =  foldl (fn (i, S) => add S i) IM.empty $ upto
      (max_slot+1) in (ref all, ref all, true, b) end
      (* WARNING, in ASLR mode, an OOM condition will cause looping *)

    fun aslr (z, m, _, b) = (z, m, true, b)
    fun cheap (z, m, _, b) = (z, m, false, b)
    fun copy (ref zs, ref xs, v, b) =
        (Print.esay "WARNING: copying allocator";
         (ref zs, ref xs, v, b))

    fun magic y = 166.503-0.000541462/(Math.pow ((~2.14896e~13+2.3966e~13 *
      y+2.3966e~13 * (Math.sqrt (0.804142+y * (y - 1.79334)))),
      (1.0/3.0)))+2.80815e6 * (Math.pow ((~2.14896e~13+2.3966e~13 * y+2.3966e~13
      * (Math.sqrt(0.804142+y * (y -1.79334)))), (1.0/3.0)))

    fun random_dist () = floor (magic (Random.randReal ass_random))
    fun allocFilter f (R as (Z, S, mode, _)) =
      if mode
      then let val a = random_dist () in
        if (IM.has (IM.intersectWith (fn (x, y) => x) ((!Z), (!S))) a) andalso
        (f a)
        then a
        else allocFilter f R
           end
      else case IM.firsti (IM.filteri (fn (x, y) => f x) (IM.intersectWith (fn (x, y) => x) ((!Z),
      (!S)))) of
                NONE => raise OOM
              | SOME (x, ()) => (S := IM.delete (!S) x; x)

    fun alloc m = allocFilter (fn (_) => true) m
    fun allocMany (R as (ref Z, ref S, _, _)) n =
        let fun take 0 = []
              | take n = alloc R :: take (n-1)
        in if IM.count (IM.intersectWith (fn (x, y) => x) (Z, S)) < n then raise OOM
           else take n
        end

    fun free (R as (_, S, _, _)) x = S := add (!S) x
    fun freeMany R = List.app (free R)
    fun update (Z, _, _, _) (B{v = ref vital,...}) =
        Z := IntMap.map (fn (x) => ()) (IntMap.filter (fn (x) => x > 0) vital)

    fun withSlot (a : allocr) f =
        let val s = alloc a
        in after f (free a) s
        end

    fun use (_, a, _, _) slotno =
        (if not (IM.has (!a) slotno) then raise AlreadyInUse else ();
         a := IM.delete (!a) slotno)

    fun board (_,_,_,b) = b
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

    fun putIfNecessary (B {f,v,f',v'}) dest : move list =
        case IntMap.look' (!f) dest of %CI => [] | _ => [L CInc dest]

    (* generates a sequence of moves to store v into dest *)
    fun intNoPut dest v = R dest CZero :: map (fn c => L c dest) (opsForInt v)
    (* note: inc is strictly better than put *)
    fun int (a : Allocator.allocr) (dest : slotno) (v : value) : move list =
        putIfNecessary (Allocator.board a) dest @ intNoPut dest v

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
     (* XXX: uses allocator in a buggy way *)
     (* fun shiftInt (A : Allocator.allocr) dest n =
      *     (* FIXME: can determine the shorter without generating both lists *)
      *     let val shiftEncode = shift dest (encode n)
      *         val tempShift = Allocator.withSlot A
      *                         (fn i => int i n @
      *                                  shift dest (CApp (%CGet, encode i)))
      *     in minBy length shiftEncode tempShift
      *     end *)
     fun shiftInt dest n = shift dest (encode n)

     (* loader that takes into account state *)
     fun load_fast (b : LTG.board, a : Allocator.allocr) dest expr =
         let val () = checkExpr expr
             fun left x = L x dest
             val right = R dest
             (* right-applies a number into dest, using temps if necessary *)

             (* right-applies an expression into dest *)
             fun shift (% c) = [right c]
               | shift (CApp (e1, e2)) = [left CK, left CS] @ shift e1 @ shift e2
               | shift (CVal v) = shiftInt dest v

             (* loads an expression into dest, assuming dest contains I. *)
             fun load (% c) = [right c]
               | load (CApp (%c, e)) = load e @ [left c]  (* optimization *)
               | load (CApp (e1, e2)) = load e1 @ shift e2
               | load (CVal v) = intNoPut dest v
         in putIfNecessary b dest @ load expr
         end

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
               | shift (CVal v) = shiftInt dest v

             (* loads an expression into dest, assuming dest contains I. *)
             fun load (% c) = [right c]
               | load (CApp (%c, e)) = load e @ [left c]  (* optimization *)
               | load (CApp (e1, e2)) = load e1 @ shift e2
               | load (CVal v) = intNoPut dest v
         in left CInc (* make sure dest contains I *) :: load expr
         end

  end

end
