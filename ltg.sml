structure LTG =
struct
  type slotno = int             (* slot number *)
  type value = int              (* field value (other than function) *)
  type vitality = int

  val max = 65335
  val max_slot = 255

  fun is_alive v = v > 0
  val is_dead = not o is_alive
  fun is_valid_slot n = n >= 0 andalso n <= max_slot

  datatype card = 
           CI
         | CZero
         | CSucc
         | CDbl
         | CGet
         | CPut
         | CS
         | CK
         | CInc
         | CDec
         | CAttack
         | CHelp
         | CCopy
         | CRevive
         | CZombie

  datatype comb =
           CVal of value
         | CVar of Variable.var
         | CApp of comb * comb
         | & of comb * comb (* staged computation *)
         | % of card

  datatype app_dir = LeftApp | RightApp

  datatype board = B of { f : comb array, v: vitality array,
                          f': comb array, v': vitality array }

  val num_slots : slotno = 256
  val init_vitality : vitality = 10000

  fun show_card CI = "I"
    | show_card CZero = "zero"
    | show_card CSucc = "succ"
    | show_card CDbl = "dbl"
    | show_card CGet = "get"
    | show_card CPut = "put"
    | show_card CS = "S"
    | show_card CK = "K"
    | show_card CInc = "inc"
    | show_card CDec = "dec"
    | show_card CAttack = "attack"
    | show_card CHelp = "help"
    | show_card CCopy = "copy"
    | show_card CRevive = "revive"
    | show_card CZombie = "zombie"

  fun show_comb (CVal v) = Int.toString v
    | show_comb (CVar v) = "x"
    | show_comb (CApp (a,b)) = (show_comb a) ^ "(" ^ (show_comb b) ^ ")"
    | show_comb (% c) = show_card c
    | show_comb (op & (a, b)) = (show_comb a) ^ "(" ^ (show_comb b) ^ ")"

  fun build_board () =
      B { f = Array.array (num_slots, %CI)
        , v = Array.array (num_slots, init_vitality)
        , f' = Array.array (num_slots, %CI)
        , v' = Array.array (num_slots, init_vitality)
        }

  fun copy_board (B {f, v, f', v'}) =
      B { f = Util.copyArray f, v = Util.copyArray v,
          f' = Util.copyArray f', v' = Util.copyArray v' }

  fun %% CZero = CVal 0
    | %% x = % x

  val card_to_int =
   fn CI      => 0
    | CZero   => 1
    | CSucc   => 2
    | CDbl    => 3
    | CGet    => 4
    | CPut    => 5
    | CS      => 6
    | CK      => 7
    | CInc    => 8
    | CDec    => 9
    | CAttack => 10
    | CHelp   => 11
    | CCopy   => 12
    | CRevive => 13
    | CZombie => 14

  val int_to_card =
   fn 0 => CI
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
      
end
