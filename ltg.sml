structure LTG =
struct
  datatype comb =
           CVal of int
         | CApp of comb * comb
         | & of comb * comb (* staged computation *)
         | CI
         (* | CZero *) (* Zero is just CVal 0 *)
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
         | CVar of Variable.var

  datatype app_dir = LeftApp | RightApp

  datatype board = B of { f: comb array, v: int array,
                          f': comb array, v': int array }

  val num_slots = 256
  val init_vitality = 10000
  fun build_board () =
      B { f=Array.array (num_slots, CI), v=Array.array (num_slots, init_vitality),
          f'=Array.array (num_slots, CI), v'=Array.array (num_slots, init_vitality) }

  fun copy_board (B {f, v, f', v'}) =
      B { f = Util.copyArray f, v = Util.copyArray v,
          f' = Util.copyArray f', v' = Util.copyArray v' }

end
