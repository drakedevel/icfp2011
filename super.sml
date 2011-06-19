structure Super =
struct
  structure ListMUtil = MonadUtil(ListM)
  open ListMUtil infix <$> <*>
  open Util

  structure L = LTG

  val max = 65335

  fun clamp n = if n < 0 then 0 else if n > max then max else n

  datatype expr = ? of expr * expr
                | SVal of int
                | SFakeVar of int
                | SS
                | SK
                | SI
                | SZero
                | SSucc
                | SDbl
                | SGet
                | SPut
                | SInc
                | SDec
                | SAttack
                | SHelp
                | SCopy
                | SRevive
                | SZombie
  infix ?

  local
      val nex = ref 0
  in
      fun unique () = SFakeVar (!nex) before nex := !nex + 1
      fun reset () = nex := 0
  end 

  val all_exps = [SS,SK,SI,SZero,SSucc,SDbl,SGet,SPut,SInc,SDec,SAttack,SHelp,SCopy,SRevive,SZombie]
  val all_aps = (curry2 id) <$> [L.LeftApp, L.RightApp] <*> all_exps

  fun from_comb (L.CVal v) = SVal v
    | from_comb (L.CVar v) = raise Fail "CVar?"
    | from_comb (L.CApp (a, b)) = (from_comb a) ? (from_comb b)
    | from_comb (L.% c) = case c of
                              L.CS => SS
                            | L.CK => SK
                            | L.CI => SI
                            | L.CZero => SZero
                            | L.CSucc => SSucc
                            | L.CDbl => SDbl
                            | L.CGet => SGet
                            | L.CPut => SPut
                            | L.CInc => SInc
                            | L.CDec => SDec
                            | L.CAttack => SAttack
                            | L.CHelp => SHelp
                            | L.CCopy => SCopy
                            | L.CRevive => SRevive
                            | L.CZombie => SZombie

  fun to_comb (a ? b) = L.CApp (to_comb a, to_comb b)
    | to_comb (SVal v) = L.CVal v
    | to_comb SS = L.% L.CS
    | to_comb SK = L.% L.CK
    | to_comb SI = L.% L.CI
    | to_comb SZero = L.% L.CZero
    | to_comb SSucc = L.% L.CSucc
    | to_comb SDbl = L.% L.CDbl
    | to_comb SGet = L.% L.CGet
    | to_comb SPut = L.% L.CPut
    | to_comb SInc = L.% L.CInc
    | to_comb SDec = L.% L.CDec
    | to_comb SAttack = L.% L.CAttack
    | to_comb SHelp = L.% L.CHelp
    | to_comb SCopy = L.% L.CCopy
    | to_comb SRevive = L.% L.CRevive
    | to_comb SZombie = L.% L.CZombie

  local 
      exception NonTerminating;

      fun is_idempotent' (SVal _) = true
        | is_idempotent' (SFakeVar _) = true
        | is_idempotent' (SS ? _ ? _ ? _) = false
        | is_idempotent' (SK ? _ ? _) = false (* would have been reduced away *)
        | is_idempotent' (SZero) = true
        | is_idempotent' (SSucc ? x) = is_idempotent' x
        | is_idempotent' (SGet ? (SVal v)) = v < 256
        | is_idempotent' (SGet ? _) = false
        | is_idempotent' (SPut ? (SVal v)) = v < 256
        | is_idempotent' (SPut ? _ ? _) = false
        | is_idempotent' (SInc ? _) = false
        | is_idempotent' (SDec ? _) = false
        | is_idempotent' (SAttack ? _ ? _ ? _) = false
        | is_idempotent' (SHelp ? _ ? _ ? _) = false
        | is_idempotent' (SCopy ? (SVal v)) = v < 256
        | is_idempotent' (SCopy ? _) = false
        | is_idempotent' (SRevive ? _) = false
        | is_idempotent' (SZombie ? _ ? _) = false
        | is_idempotent' (a ? b) = is_idempotent' a andalso is_idempotent' b
        | is_idempotent' _ = true

      and is_idempotent n e = (reduce n e; is_idempotent' e)
          handle NonTerminating => false

      and reduce 0 _ = raise NonTerminating
        | reduce n (SS ? f ? g ? x) =
          let 
              val f' = reduce (n-1) f
              val g' = reduce (n-1) g
              val x' = reduce (n-1) x
              val fx = reduce (n-1) (f' ? x')
              val gx = reduce (n-1) (g' ? x')
          in
              if is_idempotent (n-1) x then
                  reduce (n-1) (fx ? gx)
              else
                  SS ? f' ? g' ? x'
          end
        | reduce n (x as (SK ? a ? i)) =
          if is_idempotent (n-1) i then
              reduce (n-1) a
          else
              x
        | reduce n (SI ? a) = reduce (n-1) a
        | reduce n SZero = SVal 0
        | reduce n (SSucc ? x) =
          let
              val x' = reduce (n-1) x
          in
              case x' of
                  SVal v => SVal (clamp (v + 1))
                | _ => SSucc ? x'
          end
        | reduce n (SDbl ? x) =
          let
              val x' = reduce (n-1) x
          in
              case x' of
                  SVal v => SVal (clamp (v * 2))
                | _ => SDbl ? x'
          end
        | reduce n (SGet ? x) = SGet ? (reduce (n-1) x)
        | reduce n (SPut ? x ? y) =
          let
              val x' = reduce (n-1) x
              val y' = reduce (n-1) y
          in
              if is_idempotent (n-1) x
              then y'
              else SPut ? x' ? y'
          end
        | reduce n (a ? b) = (reduce (n-1) a) ? (reduce (n-1) b)
        | reduce n e = e

      fun eval' n m x orig mut = let
          val x' = reduce 100 x
      in
          if n = m then
              orig
          else if x' = x then
              eval' (n + 1) m (x ? (unique ())) orig (mut + 1)
          else
              eval' (n + 1) (n + 4) x' (x', mut) mut
      end
  in
  fun eval x = eval' 0 3 x (x,0) 0 before reset ()
      handle x => (reset (); raise x)

  fun equivalent x y =
      let
          val x' = eval x
          val y' = eval y
      in
          x' = y'
      end
      handle NonTerminating => false
  end

  local
      fun fasteq x y' =
          let
              val x' = eval x
          in
              x' = y'
          end
          handle NonTerminting => false

      fun ap NONE (_, e) = SOME e
        | ap (SOME v) (L.LeftApp, e) = SOME (e ? v)
        | ap (SOME v) (L.RightApp, e) = SOME (v ? e)

      fun dfs outcome aps exp 0 = foldl (fn (e,a) => case a of
                                                         NONE => if fasteq (valOf (ap exp e)) outcome
                                                                 then SOME (ap exp e)
                                                                 else NONE
                                                       | SOME _ => a) NONE aps
        | dfs outcome aps exp n = foldl (fn (e,a) => case a of
                                                         NONE => dfs outcome aps (ap exp e) (n-1)
                                                       | SOME _ => a) NONE aps

      fun search' outcome lim upper =
          if lim = upper then
              NONE
          else
              ((*print ("Starting depth: " ^ (Int.toString (lim + 1)) ^ "\n");*)
               case (dfs outcome all_aps NONE lim) of
                   NONE => search' outcome (lim + 1) upper
                 | SOME x => x)
  in
      fun minimize outcome upper = (reset (); search' (eval outcome) 0 upper)
  end

  local
      fun ap v L.LeftApp e = (e ? v)
        | ap v L.RightApp e = (v ? e)

      fun exprs' 0 m l = exprs' 1 m all_exps
        | exprs' n m l = if n = m
                         then l
                         else exprs' (n+1) m (ap <$> l <*> [L.LeftApp,L.RightApp] <*> all_exps)
  in
      fun exprs m = Util.dedup (exprs' 0 m [])
  end

end
