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
		      | 1 => (L.% (L.CVal 0))
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
      val res = run_move b dir card slot
  in
      case res of
	  SOME _ => (b, {dir = dir, card = card, slot = slot})
	| NONE => randMove b'
  end

  local
      fun proponent b = let
	  val (b', move) = randMove b
      in
	  ReaderWriter.put_move move; opponent b'
      end

      and opponent b = let
	  val {dir,card,slot} = ReaderWriter.get_move ()
      in
	  run_move b dir card slot; proponent b
      end
  in
      fun main (name, args) = case args of
				  ["0"] => proponent (build_board ())
				| ["1"] => opponent (build_board ())
				  (*handle _ => OS.Process.success*)
  end
end
