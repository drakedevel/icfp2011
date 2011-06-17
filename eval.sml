signature EVALUATOR =
sig
  type move = LTG.app_dir * LTG.card * LTG.slotno
  val move : LTG.app_dir -> LTG.card -> LTG.slotno -> move (* smart ctor for moves *)
  val L : LTG.card -> LTG.slotno -> move               (* left app *)
  val R : LTG.slotno -> LTG.card -> move               (* right app *)

  val switch_teams : LTG.board -> LTG.board

  (* eval LTG.board K is_zombie ==> result
   *
   * evaluates LTG.combinator K in LTG.board. acts like a zombie iff is_zombie is true. Returns (SOME
   * result) or NONE on error.
   *)
  val eval : LTG.board -> LTG.comb -> bool -> LTG.comb option

  (* run_zombies LTG.board ==> ()
   *
   * Runs all zombies on the LTG.board. Use before turn. *)
  val run_zombies : LTG.board -> unit

  (* run_move LTG.board move ==> result
   *
   * Returns (SOME result) or NONE on error.
   *)
  val run_move : LTG.board -> move -> LTG.comb option

  (* run_moves LTG.board moves ==> () *)
  val run_moves : LTG.board -> move list -> unit
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
  fun move app_dir card slotno : move =
      if is_valid_slot slotno then (app_dir, card, slotno)
      else raise Fail "invalid move"

  val L = move LeftApp
  val R = flip (move RightApp)

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
             | %CSucc & e => num (fn n => CVal $ clamp $ n+1) e
             | %CDbl & e => num (fn n => CVal $ clamp $ n*2) e
             | %CGet & e => num (sub f) e
             | %CPut & _ => %CI (* NOTE unnecessary but makes output cleaner *)
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
                   val j = num id arse
                   val () = if is_dead $ v' ! j  then () else
                            up v' j $ clamp $ v' ! j -- (n * 9 div 10)
               in %CI end
             | %CAttack & _ & _ & _ => raise Stuck
             | %CHelp & CVal i & arse & CVal n =>
               let val () = if v ! i < n then raise TooBig else
                            up v i $ (v ! i) - n
                   val j = num id arse
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
     handle _ => NONE end

  (* To be run before a turn. Runs all of the zombies *)
  fun run_zombies (board as B{f,v,...}) =
      let fun handle_zombie (i, ~1) =
              (ignore $ eval board (CApp (f ! i, %CI)) true;
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

  fun run_moves board moves = List.app (const () o run_move board) moves

end
