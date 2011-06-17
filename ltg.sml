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
