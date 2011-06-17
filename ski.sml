(* SKI combinator language *)
structure SKI = 
struct
  datatype expr = 
      S
    | K
    | I
    | EApp of expr * expr
    | EVar of Variable.var
end
