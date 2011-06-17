structure Top =
struct
  open LTG
  open Evaluator

  val rand = Word.toInt o Rand.mkRandom (Word31.fromLargeInt (Time.toMicroseconds (Time.now ())))

  fun randApp () = case ((rand ()) mod 2) of
		       0 => LeftApp
		     | 1 => RightApp
		     | _ => raise Fail "wtf"

  fun randCard () = case ((rand ()) mod 14) of
			0 => CI
		      | 1 => CSucc
		      | 2 => CDbl
		      | 3 => CGet
		      | 4 => CPut
		      | 5 => CS
		      | 6 => CK
		      | 7 => CInc
		      | 8 => CDec
		      | 9 => CAttack
		      | 10 => CHelp
		      | 11 => CCopy
		      | 12 => CRevive
		      | 13 => CZombie
		      | _ => raise Fail "wtf"

  fun randSlot () = (rand ()) mod 256

  fun randMove b = let
      val b' = copy_board b
      val app = randApp ()
      val card = randCard ()
      val slot = randSlot ()
      val res = run_move b app slot card
  in
      case res of
	  SOME _ => (app, card, slot)
	| NONE => randMove b'
  end

  fun main (name, args) = let
      val board = build_board ()
      val move = randMove board
  in
      (*OS.Process.success*) raise Fail "oh my god"
  end
end
