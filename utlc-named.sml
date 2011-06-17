(* untyped lambda calculus with named variables *)
structure UTLCNamed = 
struct
  datatype expr = 
      EVar of Variable.var
    | ELam of Variable.var * expr
    | EApp of expr * expr
    | % of LTG.card
end
