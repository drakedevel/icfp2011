(* structure for compilation from named UTLC and SKI languages *)
structure Compile =
struct

  structure U = UTLCNamed
  structure L = LTG
  structure V = Variable

  infix @
  val (op @) = L.CApp

  local open LTG in

  end

  local
    open LTG
    open Util
    infix CApp
    infix &
  in

    type card_purity = bool * int (* purity, arity *)

    fun card_purity CI = (true, 1)
      | card_purity CZero = (true, 0)
      | card_purity CSucc = (true, 1)
      | card_purity CDbl = (true, 1)
      | card_purity CGet = (false, 1)
      | card_purity CPut = (true, 2)
      | card_purity CS = (true, 3)
      | card_purity CK = (true, 2)
      | card_purity CInc = (false, 1)
      | card_purity CDec = (false, 1)
      | card_purity CAttack = (false, 3)
      | card_purity CHelp = (false, 3)
      | card_purity CCopy = (false, 1)
      | card_purity CRevive = (false, 1)
      | card_purity CZombie = (false, 2)

    (* determines whether a card has effects when applied to its nth argument *)
    fun is_partially_applied_card_pure c n =
        let val (pure, arity) = card_purity c
        in n < arity orelse (n = arity andalso pure)
        end

    (* if this returns true, the expression is guaranteed side-effect-free.
     * however, not guaranteed to return true if the expression is pure.
     *)
    fun is_pure (f CApp x) = is_pure_func f andalso is_pure x
      | is_pure (% _) = true
      | is_pure (CVal _) = true
      | is_pure (_ & _) = true
      | is_pure (CVar _) = true

    and is_pure_func (%CS & f & g) = is_pure_func f andalso is_pure_func g
      (* application *)
      | is_pure_func (f CApp x) = is_pure_func f andalso is_pure x andalso is_pure_func (f & x)
      (* partial card application for cards not specially checked *)
      | is_pure_func (% c) = is_partially_applied_card_pure c 1
      | is_pure_func (%c & _) = is_partially_applied_card_pure c 2
      | is_pure_func (%c & _ & _) = is_partially_applied_card_pure c 3
      (* otherwise, assume not pure to be safe *)
      | is_pure_func _ = false

    fun peep (%CS CApp %CK CApp %_) = %CI
      | peep (%CS CApp (%CK CApp %x) CApp %CI) = %x (* I have only tested this, not proven it correct *)
      | peep (%CK CApp %CI) = %CPut
      | peep (e as %CPut CApp exp) = if is_pure exp
                                     then (Log.log ("replacing pure " ^ show_comb e
                                                    ^ " with " ^ show_comb (%CI));
                                           %CI)
                                     else e
      | peep (%CI CApp x) = x
      | peep x = x

    fun containsVar v (CVal _) = false
      | containsVar v (CVar v') = Variable.equal (v, v')
      | containsVar v (f CApp x) = containsVar v f orelse containsVar v x
      | containsVar v (% c) = false

  end

  local val % = L.% in
    fun bracket' x c =
        (* TODO running containsVar andalso is_pure at every level is unnecessary. *)
        if not (containsVar x c) andalso is_pure c then %L.CK @ c else
        case c
         of L.CVar y => if V.equal (x, y) then (% L.CI)
                        else (% L.CK) @ c
          | L.CApp (C1, C2) => peep (%L.CS @ bracket x C1) @ bracket x C2
          | _ => % L.CK @ c
    and bracket x c = bracket' x c
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
