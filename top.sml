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
      fun main (name, args) = case args of
                  ["0"] => proponent (build_board ())
                | ["1"] => opponent (build_board ())
                | _ => raise Fail "what do you want from me?"
                  (*handle _ => OS.Process.success*)
  end
end
