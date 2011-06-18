signature JOB = sig

  type move = Evaluator.move
  type job

  datatype repeat = ROnce of unit -> unit | RForever

  val schedule : move list -> repeat -> job
  val suspend : job -> unit
  val resume : job -> unit

  val get_move : unit -> move
end

structure Job : JOB = struct
  type move = Evaluator.move
  datatype repeat = ROnce of unit -> unit | RForever
  datatype state = IN | DEAD | OUT

  type job = { repeat : repeat,
                   moves : move list,
                   cur : move list ref,
                   state : state ref }

  val jobs : job Queue.queue = Queue.queue ()

  fun schedule moves repeat = let
      val info = { repeat = repeat,
                   moves = moves,
                   cur = ref moves,
                   state = ref IN }
    in
      Queue.insert (jobs, info);
      info
    end

  fun suspend (j: job) = (#state j := DEAD)
  fun resume (j: job) = (
    case !(#state j) of
      IN => ()
    | DEAD => (#state j := IN)
    | OUT => (#state j := IN; Queue.insert (jobs, j))
  )

  fun get_move () = let
      val j as { repeat, moves, cur, state } = Queue.remove jobs
    in
      case !state of DEAD => (state := OUT; get_move ())
      | _ => (
        hd (!cur)
        before (
          cur := tl (!cur);
          case (!cur, repeat) of
            (nil, ROnce f) => f ()
          | (nil, RForever) => (Queue.insert (jobs, j); cur := moves)
          | (_, _) => (Queue.insert (jobs, j))
        )
      )
    end

end
