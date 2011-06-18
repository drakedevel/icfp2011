structure Top =
struct
  open LTG
  open Evaluator
  infix ?
  val (op ?) = CApp

  val rand = Random.rand (0x1337BABE, 0x1337D00D)

  fun randDir () = case (Random.randRange (0,1) rand) of
               0 => LeftApp
             | 1 => RightApp
             | _ => raise Fail "wtf"

  fun randCard () = int_to_card (Random.randRange (0,14) rand)

  fun randSlot () = Random.randRange (0,255) rand

  fun randMove b = let
      val b' = copy_board b
      val dir = randDir ()
      val card = randCard ()
      val slot = randSlot ()
      val mv = move dir card slot
      val res = run_move b mv
  in
      case res of
      SOME _ => (b, mv)
    | NONE => randMove b'
  end
(*
  local
      fun proponent b = let
          val (b', mv) = randMove b
          val _ = ReaderWriter.put_move mv
      in opponent b' end

      and opponent b = let
          val mv = ReaderWriter.get_move ()
          val _ = run_move b mv
      in proponent b end
  in
      fun random_main (name, args) = case args of
                  ["0"] => proponent (build_board ())
                | ["1"] => opponent (build_board ())
                | _ => raise Fail "what do you want from me?"
                  (*handle _ => OS.Process.success*)
  end

  local
      fun xbox () =
      let val alloc = Allocator.new ()
          val [0,1] = Allocator.allocMany alloc 2
          val payload = %CS ? (%CGet ? %CZero) ? (%CGet ? %CZero)
          val copy_back = %CGet ? CVal 1
      in Load.load alloc 1 payload @ Load.load alloc 0 copy_back end
      val xbox_moves = xbox ()

      fun proponent (mv::mvs) = let
          val _ = ReaderWriter.put_move mv
      in opponent mvs end
        | proponent [] = proponent xbox_moves

      and opponent mvs = let
          val mv = ReaderWriter.get_move ()
      in proponent mvs end
  in
      fun xbox_main (name, args) = case args of
                  ["0"] => proponent []
                | ["1"] => opponent []
                | _ => raise Fail "what do you want from me?"
                  (*handle _ => OS.Process.success*)
  end

  local
      val setup = Terms.load' Terms.repeat_kill
      val noobing = [ R 0 CI ]

      fun proponent b (mv::mvs) = let
          val _ = ReaderWriter.put_move mv
          val _ = run_move b mv
      in opponent b mvs end
        | proponent b [] = proponent b noobing

      and opponent b mvs = let
          val mv = ReaderWriter.get_move ()
          val _ = run_move b mv
      in proponent b mvs end
  in
      fun noob_main (name, args) = case args of
                  ["0"] => proponent (build_board ()) setup
                | ["1"] => opponent (build_board ()) setup
                | _ => raise Fail "what do you want from me?"
                  (*handle _ => OS.Process.success*)
  end
*)

  (* Let's fire off a job... *)
  fun addNoobing () = ignore (Job.schedule [ R 0 CI ] Job.RForever)
  val _ = Job.schedule (Terms.load' Terms.repeat_kill) (Job.ROnce addNoobing)

  fun logic old_board new_board =
      let val (my_diff, their_diff) = Evaluator.diff_boards old_board new_board
      in () end

  local
      fun proponent b_old b = let
          val mv = Job.get_move ()
      in
          ReaderWriter.put_move mv;
          run_move b mv;
          opponent (switch_teams b)
      end
      and opponent b = let
          val mv = ReaderWriter.get_move ()
          val b' = copy_board b
      in
          run_move b mv;
          proponent (switch_teams b') (switch_teams b)
      end
  in
      fun main (name, args) = case args of
             ["0"] => proponent (build_board ()) (build_board ())
           | ["1"] => opponent (build_board ())
           | _ => raise Fail "Incorrect arguments"
      fun main' args = main args
          handle e => (Print.esay ("Unhandled exception: " ^ exnMessage e);
                       OS.Process.failure)
  end

end
