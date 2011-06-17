structure LTG =
struct
  datatype comb =
           CVal of int
         | & of comb * comb
         | CI
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

end
