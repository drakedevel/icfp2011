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

  datatype app_dir = LeftApp | RightApp

  datatype board = B of { f: comb array, v: int array,
                          f': comb array, v': int array }

end
