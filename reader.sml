structure Reader =
struct
  structure L = LTG
  structure E = Evaluator

  fun read () = valOf (TextIO.inputLine TextIO.stdIn)

  fun to_int s = valOf (Int.fromString s)

  fun to_card s =
    if s = "I\n" then L.CI else
    if s = "zero\n" then L.CZero else
    if s = "succ\n" then L.CSucc else
    if s = "dbl\n" then L.CDbl else
    if s = "get\n" then L.CGet else
    if s = "put\n" then L.CPut else
    if s = "S\n" then L.CS else
    if s = "K\n" then L.CK else
    if s = "inc\n" then L.CInc else
    if s = "dec\n" then L.CDec else
    if s = "attack\n" then L.CAttack else
    if s = "help\n" then L.CHelp else
    if s = "copy\n" then L.CCopy else
    if s = "revive\n" then L.CRevive else
    if s = "zombie\n" then L.CZombie else
      raise Fail "owned"

  fun to_dir s = let val n = to_int s in
    if n = 1 then L.LeftApp else L.RightApp
  end

  fun get_move () : E.move = let
    val dir_str = read ()
    val () = TextIO.print dir_str
    val card_str = read ()
    val slot_str = read ()
  in E.move (to_dir dir_str)
            (to_card card_str)
            (to_int slot_str)
     handle _ => raise Fail "server gave us invalid move"
  end
end
