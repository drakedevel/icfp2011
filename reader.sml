structure ReaderWriter =
struct
  structure L = LTG

  type move = {dir : L.app_dir,
               card : L.comb,
               slot : int}

  fun read () = valOf (TextIO.inputLine TextIO.stdIn)

  fun write str = (TextIO.output (TextIO.stdOut, str); TextIO.flushOut TextIO.stdOut)

  fun to_int s = valOf (Int.fromString s)

  fun to_card "I\n" = L.CI
    | to_card "zero\n" = (L.% (L.CVal 0))
    | to_card "succ\n" = L.CSucc
    | to_card "dbl\n" = L.CDbl
    | to_card "get\n" = L.CGet
    | to_card "put\n" = L.CPut
    | to_card "S\n" = L.CS
    | to_card "K\n" = L.CK
    | to_card "inc\n" = L.CInc
    | to_card "dec\n" = L.CDec
    | to_card "attack\n" = L.CAttack
    | to_card "help\n" = L.CHelp
    | to_card "copy\n" = L.CCopy
    | to_card "revive\n" = L.CRevive
    | to_card "zombie\n" = L.CZombie
    | to_card x = raise Fail ("Invalid input card name " ^ x)

  fun from_card L.CI = "I\n"
    | from_card (L.% (L.CVal 0)) = "zero\n"
    | from_card L.CSucc = "succ\n"
    | from_card L.CDbl = "dbl\n"
    | from_card L.CGet = "get\n"
    | from_card L.CPut = "put\n"
    | from_card L.CS = "S\n"
    | from_card L.CK = "K\n"
    | from_card L.CInc = "inc\n"
    | from_card L.CDec = "dec\n"
    | from_card L.CAttack = "attack\n"
    | from_card L.CHelp = "help\n"
    | from_card L.CCopy = "copy\n"
    | from_card L.CRevive = "revive\n"
    | from_card L.CZombie = "zombie\n"
    | from_card _ = raise Fail "Invalid output card type."

  fun to_dir "1\n" = L.LeftApp
    | to_dir "2\n" = L.RightApp
    | to_dir _ = raise Fail "Invalid input application direction."

  fun from_dir L.LeftApp = "1\n"
    | from_dir L.RightApp = "2\n"

  fun from_int i = (Int.toString i) ^ "\n"

  fun get_move () = let
      val dir = to_dir (read ())
      val a_str = read ()
      val b_str = read ()
  in
      case dir of
	  L.LeftApp => {dir = dir, card = to_card a_str, slot = to_int b_str}
	| L.RightApp => {dir = dir, card = to_card b_str, slot = to_int a_str}
  end

  fun put_move {dir,card,slot} = let
      val dir_str = from_dir dir
      val card_str = from_card card
      val slot_str = from_int slot
  in
      write dir_str;
      case dir of
	  L.LeftApp => (write card_str; write slot_str)
	| L.RightApp => (write slot_str; write card_str)
  end
end
