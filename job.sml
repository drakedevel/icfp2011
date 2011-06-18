signature JOB = sig

  type job

  datatype repeat = ROnce | RForever

  val schedule : move list -> repeat -> job
  val suspend : job -> unit
  val resume : job -> unit

  val getMove : unit -> move
end

structure Job : JOB = struct
  datatype repeat = ROnce | RForever
  type job = { repeat : repeat,
                   moves : move list,
                   cur : move list ref,
                   enabled : bool ref }

  val inStack = ref (nil: job list)
  val outStack = ref (nil: job list)

  fun schedule moves repeat = let
      val info = { repeat = repeat,
                   moves = moves,
                   cur = ref moves,
                   enabled = ref true }
    in
      inStack := (info :: !inStack);
      info
    end

  fun suspend (j: job) = (#enabled j := true)
  fun resume (j: job) = (#enabled j := true)

  fun reschedule () = (
    case !outStack of nil => raise Fail "no moves"
                      | moves => (outStack := nil; inStack := moves)
  )

  fun runJob (j as { repeat, moves, cur, enabled }) = hd (!cur)
    before (
      cur := tl (!cur);
      case (!cur, repeat) of
        (nil, ROnce) => ()
      | (nil, RForever) => (outStack := j::(!outStack); cur := moves)
      | (_, _) => (outStack := j::(!outStack))
    )

  fun getMove () = (case !inStack of
      nil => (reschedule(); getMove())
    | (m::ms) => (inStack := ms; outStack := m::(!outStack); runJob m)
  )

end
