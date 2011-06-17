(* structure for compilation from named UTLC and SKI languages *)
structure Compile = 
struct

  structure U = UTLCNamed
  structure L = LTG
  structure V = Variable

  infix @
  val (op @) = L.CApp

  fun bracket x (C as (L.CVar y)) =
      if V.equal (x, y) then L.CI
      else L.CK @ C
    | bracket x (L.CApp (C1, C2)) = L.CS @ (bracket x C1) @ (bracket x C2)
    | bracket x C = L.CK @ C

  fun convertExpr (U.EVar x) = (L.CVar x)
    | convertExpr (U.EApp (e1, e2)) = (convertExpr e1) @ (convertExpr e2)
    | convertExpr (U.ELam (x, e)) = bracket x (convertExpr e)
      
end
