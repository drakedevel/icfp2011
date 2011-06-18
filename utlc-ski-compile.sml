(* structure for compilation from named UTLC and SKI languages *)
structure Compile = 
struct

  structure U = UTLCNamed
  structure L = LTG
  structure V = Variable

  infix @
  val (op @) = L.CApp

  local
    open LTG
    infix CApp
  in
    fun peep (%CS CApp %CK CApp %_) = %CI
      | peep (%CS CApp (%CK CApp %x) CApp %CI) = %x (* I have only tested this, not proven it correct *)
      | peep (%CK CApp %CI) = %CPut
      | peep (%CI CApp x) = x
      | peep x = x
  end

  local val % = L.% in
    fun bracket' x (C as L.CVar y) =
        if V.equal (x, y) then (% L.CI)
        else (% L.CK) @ C
      | bracket' x (L.CApp (C1, C2)) = peep (%L.CS @ bracket x C1) @ bracket x C2
      | bracket' x C = (% L.CK) @ C
    and bracket x C = peep (bracket' x C)
  end

(*
  fun isIdempotent x (U.EVar y) = not (V.equal (x, y))
    | isIdempotent x (U.EApp (e1, e2)) = (isIdempotent x e1) andalso (isIdempotent x e2)
    | isIdempotent x (U.ELam (x', e)) = isIdempotent x e
    | isIdempotent x (U.EVal _) = true
    | isIdempotent x (U.% L.CSucc) = true
    | isIdempotent x (U.% L.CDbl) = true
    | isIdempotent x (U.% L.CS) = true
    | isIdempotent x (U.% L.CK) = true
    | isIdempotent x (U.% L.CI) = true
    | isIdempotent x (U.% _) = false
*)
  fun isIdempotent x (U.EVar y) = not (V.equal (x, y))
    | isIdempotent x (U.ELam (x', e)) = isIdempotent x e
    | isIdempotent x (U.EApp (U.% L.CK, e2)) = isIdempotent x e2
    | isIdempotent x (U.EApp (U.EApp (U.% L.CS, e1), e2)) = 
      isIdempotent x e1 andalso isIdempotent x e2
    | isIdempotent x (U.EApp (U.% L.CS, e1)) = isIdempotent x e1
    | isIdempotent x (U.EApp _) = false
    | isIdempotent _ _ = true


  (* This only matters if you have a function which infinitely and side-effect-free
     infinite loops when called with any input. *)
  fun isHalting _ = true

  fun convertExpr' (U.EVar x) = L.CVar x
    | convertExpr' (U.EApp (e1, e2)) = (convertExpr e1) @ (convertExpr e2)
    | convertExpr' (U.ELam (x, e)) = if isIdempotent x e andalso isHalting e
                    then (L.% L.CK) @ convertExpr e
                    else peep (bracket x (convertExpr e))
    | convertExpr' (U.EVal n) = L.CVal n
    | convertExpr' (U.% c) = L.% c

  and convertExpr e = peep (convertExpr' e)

end
