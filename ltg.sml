structure LTG =
struct
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
           CVal of int
         | CVar of Variable.var
         | CApp of comb * comb
         | & of comb * comb (* staged computation *)
         | % of card

  datatype app_dir = LeftApp | RightApp

  datatype board = B of { f : comb array, v: int array,
                          f': comb array, v': int array }

  val num_slots = 256
  val init_vitality = 10000

  fun show_card CI = "I"
    | show_card CZero = "Zero"
    | show_card CSucc = "Succ"
    | show_card CDbl = "Dbl"
    | show_card CGet = "Get"
    | show_card CPut = "Put"
    | show_card CS = "S"
    | show_card CK = "K"
    | show_card CInc = "Inc"
    | show_card CDec = "Dec"
    | show_card CAttack = "Attack"
    | show_card CHelp = "Help"
    | show_card CCopy = "Copy"
    | show_card CRevive = "Revive"
    | show_card CZombie = "Zombie"

  fun build_board () =
      B { f = Array.array (num_slots, %CI)
        , v = Array.array (num_slots, init_vitality)
        , f' = Array.array (num_slots, %CI)
        , v' = Array.array (num_slots, init_vitality)
        }

  fun copy_board (B {f, v, f', v'}) =
      B { f = Util.copyArray f, v = Util.copyArray v,
          f' = Util.copyArray f', v' = Util.copyArray v' }

end
