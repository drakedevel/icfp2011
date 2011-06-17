structure Evaluator =
struct
  infixr 0 $ fun f $ x = f x

  open LTG
  infix &
  val ! = Array.sub
  infix 9 !
  fun sub x y = Array.sub (x, y)
  fun up x y z = Array.update (x, y, z)

  exception TooManyApps
  exception Stuck
  exception InvalidSlot
  exception TooBig
  exception NotDead

  fun switch_teams (B {f, v, f', v'}) = B {f=f', v=v', v'=v, f'=f}
  fun is_valid_slot n = n >= 0 andalso n <= 255
  fun is_alive vitality = vitality > 0
  val is_dead = not o is_alive

(*
  fun read_field slots n = get_field $ Array.sub (slots, n)
  fun read_vitality slots n = get_vitality $ Array.sub (slots, n)
  fun write_field slots n =
      let 
*)

  val max = 65335
  val max_slot = 255

  fun clamp n = if n < 0 then 0 else if n > max then max else n

  fun apply (B {f, v, f', v'}) expr zombie = let
      val ++ = if zombie then (op -) else (op +)
      val -- = if zombie then (op +) else (op -)
      infix 6 ++ --

      fun num f (CVal n) = f n
        | num _ _ = raise Stuck

      fun reduce e =
          (case e of
               CVal _ & _ => raise Stuck
             | CI & e => e
             | CSucc & e => num (fn n => CVal $ n+1) e
             | CDbl & e => num (fn n => CVal $ n*2) e
             | CGet & e => num (sub f) e
             | CPut & _ & e => e
             | CS & x & y & z => CApp (CApp (x, z), CApp (y, z))
             | CK & x & _ => x
             | CInc & e => num 
                           (fn i => let val n = v ! i
                                        val () = if is_dead n then ()
                                                 else up v i $ clamp $ n++1
                                    in CI end) e
             | CDec & e => num
                           (fn i => let val n = v ! (max_slot-i)
                                        val () = if is_dead n then ()
                                                 else up v (max_slot-i) $ clamp (n--1)
                                    in CI end) e
             (* attack and help have a bunch of corner cases. *)
             | CAttack & CVal i & arse & CVal n =>
               let val () = if v ! i < n then raise TooBig else
                            up v i $ (v ! i) - n
                   val (CVal j) = arse
                   val () = if is_dead $ v' ! j  then () else
                            up v' j $ clamp $ v' ! j -- (n * 9 div 10)
               in CI end
             | CAttack & _ & _ & _ => raise Stuck
             | CHelp & CVal i & arse & CVal n =>
               let val () = if v ! i < n then raise TooBig else
                            up v i $ (v ! i) - n
                   val (CVal j) = arse
                   val () = if is_dead $ v' ! j then () else
                            up v' j $ clamp $ v' ! j ++ (n * 11 div 10)
               in CI end
             | CHelp & _ & _ & _ => raise Stuck
             | CCopy & e => num (sub f') e
             | CRevive & e => num (fn i => (if is_dead (v ! i) then up v i 1 else (); CI)) e
             | CZombie & CVal i & x =>
               (if is_dead $ v' ! (max_slot-i) then () else raise NotDead;
                up f' (255-i) x;
                up v' (255-i) ~1;
                CI)
             | CZombie & _ & _ => raise Stuck
             | e => e)

      fun app (CApp _) 1000 = raise TooManyApps
        | app (CApp (e1, e2)) n =
          let val (e1', n') = app e1 n
              val (e2', n'') = app e2 n'
              val () = if n >= 1000 then raise TooManyApps else ()
          in app (reduce (e1' & e2')) (n+1) end
        | app e n = (e, n)


  in app expr 0 end

end
