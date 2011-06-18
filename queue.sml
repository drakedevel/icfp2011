signature QUEUE =
sig
  type 'a queue

  val queue : unit -> 'a queue
  val reset : 'a queue -> unit

  val is_empty : 'a queue -> bool
  val insert : 'a queue * 'a -> unit

  exception EmptyQueue
  val remove : 'a queue -> 'a
  val front : 'a queue -> 'a
end


structure Queue :> QUEUE =
struct
  datatype 'a node = Node of 'a * 'a node option ref

  type 'a queue = ('a node * 'a node) option ref

  fun queue () = ref NONE
  fun reset q = q := NONE

  fun mk_node elem = Node (elem, ref NONE)

  fun is_empty (ref NONE) = true
    | is_empty _ = false

  fun insert (q as ref NONE, x) =
      let val node = mk_node x
      in q := SOME (node, node) end
    | insert (q as ref (SOME (head, Node (_, tailref))), x) =
      let val node = mk_node x
      in tailref := SOME node;
         q := SOME (head, node)
      end

  exception EmptyQueue
  fun remove (ref NONE) = raise EmptyQueue
    | remove (q as ref (SOME (Node (x, headnext), tail))) =
      (q := (case !headnext of
                 SOME head' => SOME (head', tail)
               | NONE => NONE);
       x)

  fun front (ref NONE) = raise EmptyQueue
    | front (ref (SOME (Node (x, _), _))) = x
end
