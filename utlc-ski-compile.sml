(* structure for compilation from named UTLC and SKI languages *)
structure Compile = 
struct

  structure U = UTLCNamed
  structure L = SKI
  structure V = Variable

  infix @
  val (op @) = L.EApp

  fun bracket x (C as (L.EVar y)) =
      if V.equal (x, y) then L.I
      else L.K @ C
    | bracket x (L.EApp (C1, C2)) = L.S @ (bracket x C1) @ (bracket x C2)
    | bracket x C = L.K @ C

  fun convertExpr (U.EVar x) = (L.EVar x)
    | convertExpr (U.EApp (e1, e2)) = (convertExpr e1) @ (convertExpr e2)
    | convertExpr (U.ELam (x, e)) = bracket x (convertExpr e)
      
end
