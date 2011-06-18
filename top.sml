structure Top =
struct
  open LTG
  open Evaluator

  val rand = Random.rand (0x1337BABE, 0x1337D00D)

  fun randDir () = case (Random.randRange (0,1) rand) of
               0 => LeftApp
             | 1 => RightApp
             | _ => raise Fail "wtf"

  fun randCard () = case (Random.randRange (0,14) rand) of
            0 => CI
              | 1 => CZero
              | 2 => CSucc
              | 3 => CDbl
              | 4 => CGet
              | 5 => CPut
              | 6 => CS
              | 7 => CK
              | 8 => CInc
              | 9 => CDec
              | 10 => CAttack
              | 11 => CHelp
              | 12 => CCopy
              | 13 => CRevive
              | 14 => CZombie
              | _ => raise Fail "wtf"

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

  fun xbox () =
      let infix ?
          val (op ?) = CApp

          val alloc = Allocator.new ()
          val [0,1] = Allocator.allocMany alloc 2
          val payload = %CS ? (%CGet ? %CZero) ? (%CGet ? %CZero)
          val copy_back = %CGet ? CVal 1
      in Load.load alloc 1 payload @ Load.load alloc 0 copy_back end

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

  val main = xbox_main
end
