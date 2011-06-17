local 
    open Util infixr 0 $
    open LTG
    structure E = Evaluator
    type move = E.move
    val move = E.move
    val L = E.L
    val R = E.R
in

  signature ALLOCATOR =         (* imperative *)
  sig
    exception OOM
    type allocr                   (* allocator *)
    val mkEmpty : unit -> allocr
    val alloc : allocr -> slotno
    (* raises OOM /before/ allocating anything if would overflow. *)
    val allocMany : allocr -> int -> slotno list
    val free : allocr -> slotno -> unit
    val withSlot : allocr -> (slotno -> 'a) -> 'a
  end

  structure Allocator : ALLOCATOR =
  struct
    (* TODO: always use lowest available slot *)
    exception OOM
    type allocr = slotno list ref (* free list *)

    fun mkEmpty () = ref (upto (max_slot+1))

    fun alloc (ref []) = raise OOM
      | alloc (R as ref (x::xs)) = (R := xs; x)

    fun allocMany (R as ref xs) n = 
        (R := List.drop (xs, n);
         List.take (xs, n))
        handle Subscript => raise OOM

    fun free (R as ref xs) x = R := x::xs

    fun withSlot a f =
        let val s = alloc a
        in after f (free a) s
        end
  end

  structure Load =
  struct
  
    (* converts a value to binary, _big-endian_ *)
    fun toBinary (x : value) : bool list = 
        let (* little-endian helper *)
            fun bin 0 = []
              | bin x = odd x :: bin (x div 2)
        in rev (bin x)
        end

    (* these are dumb loader functions.
     * they assume our opponent never interacts with us.
     *)

    fun int (dest : slotno) (v : value) : move list =
        [ L CPut dest
        , R dest CZero ] @
        intercalate
            [ L CDbl dest ]
            (map (fn true => [ L CSucc dest ] | false => [])
                 (toBinary v))
   
  end

end
