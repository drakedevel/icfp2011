local open LTG in
signature EVALUATOR =
sig
  type move = app_dir * card * slotno
  val move : app_dir -> card -> slotno -> move (* smart ctor for moves *)

  val switch_teams : board -> board

  (* eval board K is_zombie ==> result
   *
   * evaluates combinator K in board. acts like a zombie iff is_zombie is true. Returns (SOME
   * result) or NONE on error.
   *)
  val eval : board -> comb -> bool -> comb option

  (* run_zombies board ==> ()
   *
   * Runs all zombies on the board. Use before turn. *)
  val run_zombies : board -> unit

  (* run_move board app_dir card slot ==> result
  n *
   * Returns (SOME result) or NONE on error.
   *)
  val run_move : board -> move -> comb option
end
end

structure Evaluator : EVALUATOR =
struct
  open Util infixr 0 $

  open LTG
  infix &
  val ! = Array.sub
  infix 9 !
  fun sub x y = Array.sub (x, y)
  fun up x y z = Array.update (x, y, z)

  exception EvalError of string
  val TooManyApps = EvalError "too many apps"
  val Stuck = EvalError "sexceptiontuck"
  val TooBig = EvalError "too big"
  val NotDead = EvalError "not dead"
  val Dead = EvalError "slot dead"

  type move = app_dir * card * slotno

  fun switch_teams (B {f, v, f', v'}) = B {f=f', v=v', v'=v, f'=f}

  (* smart ctor for moves *)
  fun move app_dir card slotno =
      if is_valid_slot slotno then (app_dir, card, slotno)
      else raise Fail "invalid move"

  fun clamp n = if n < 0 then 0 else if n > max then max else n

  fun eval (B {f, v, f', v'}) expr zombie = let
      val ++ = if zombie then (op -) else (op +)
      val -- = if zombie then (op +) else (op -)
      infix 6 ++ --

      fun num f (CVal n) = f n
        | num _ _ = raise Stuck

      fun reduce e =
          (case e of
               CVal _ & _ => raise Stuck
             | %CI & e => e
             | %CSucc & e => num (fn n => CVal $ n+1) e
             | %CDbl & e => num (fn n => CVal $ n*2) e
             | %CGet & e => num (sub f) e
             | %CPut & _ & e => e
             | %CS & x & y & z => CApp (CApp (x, z), CApp (y, z))
             | %CK & x & _ => x
             | %CInc & e => num 
                           (fn i => let val n = v ! i
                                        val () = if is_dead n then ()
                                                 else up v i $ clamp $ n++1
                                    in %CI end) e
             | %CDec & e => num
                           (fn i => let val n = v ! (max_slot-i)
                                        val () = if is_dead n then ()
                                                 else up v (max_slot-i) $ clamp (n--1)
                                    in %CI end) e
             (* attack and help have a bunch of corner cases. *)
             | %CAttack & CVal i & arse & CVal n =>
               let val () = if v ! i < n then raise TooBig else
                            up v i $ (v ! i) - n
                   val (CVal j) = arse
                   val () = if is_dead $ v' ! j  then () else
                            up v' j $ clamp $ v' ! j -- (n * 9 div 10)
               in %CI end
             | %CAttack & _ & _ & _ => raise Stuck
             | %CHelp & CVal i & arse & CVal n =>
               let val () = if v ! i < n then raise TooBig else
                            up v i $ (v ! i) - n
                   val (CVal j) = arse
                   val () = if is_dead $ v' ! j then () else
                            up v' j $ clamp $ v' ! j ++ (n * 11 div 10)
               in %CI end
             | %CHelp & _ & _ & _ => raise Stuck
             | %CCopy & e => num (sub f') e
             | %CRevive & e => num (fn i => (if is_dead (v ! i) then up v i 1 else (); %CI)) e
             | %CZombie & CVal i & x =>
               (if is_dead $ v' ! (max_slot-i) then () else raise NotDead;
                up f' (255-i) x;
                up v' (255-i) ~1;
                %CI)
             | %CZombie & _ & _ => raise Stuck
             | e => e)

      fun app (CApp _) 1000 = raise TooManyApps
        | app (CApp (e1, e2)) n =
          let val (e1', n') = app e1 n
              val (e2', n'') = app e2 n'
              val () = if n >= 1000 then raise TooManyApps else ()
          in app (reduce (e1' & e2')) (n+1) end
        | app e n = (e, n)

  in SOME $ #1 $ app expr 0
     handle EvalError _ => NONE end

  (* To be run before a turn. Runs all of the zombies *)
  fun run_zombies (board as B{f,v,...}) =
      let fun handle_zombie (i, ~1) =
              (eval board (CApp (f ! i, %CI)) true;
               up f i $ %CI;
               0)
            | handle_zombie (_, n) = n
      in Array.modifyi handle_zombie v end

  fun run_move (board as B{f,v,...}) (direction, card, slot_num) =
      let val slot = f ! slot_num
          val () = if is_dead $ v ! slot_num then raise Dead else ()
          val result = (eval board (case direction of
                                        LeftApp => CApp (%% card, slot)
                                      | RightApp => CApp (slot, %% card)) false)
          val () = up f slot_num $ getOpt (result, %CI)
      in result end


end
