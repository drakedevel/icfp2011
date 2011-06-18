(* untyped lambda calculus with named variables *)
structure UTLCNamed =
struct

  structure V = Variable

  datatype expr =
      EVar of Variable.var
    | ELam of Variable.var * expr
    | EApp of expr * expr
    | EVal of int
    | % of LTG.card

  fun show e = case e of
      EVar x => V.name x
    | ELam (x,e) => "\\" ^ V.name x ^ " -> " ^ show e
    | EApp (e1,e2) => "(" ^ show e1 ^ ")(" ^ show e2 ^ ")"
    | EVal n => Int.toString n
    | %c => LTG.show_card c
end
