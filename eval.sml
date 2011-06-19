signature DIFF =
sig
  (* represents what changed during an evaluation. *)
  type t

  val empty : t

  (* changes which side a diff is being viewed from. *)
  val flip : t -> t

  (* combines two diffs. *)
  val combine : t -> t -> t

  (* killed (ours, theirs) *)
  val killed : t -> LTG.slotno list * LTG.slotno list

  (* cleared (ours, theirs) *)
  val cleared : t -> LTG.slotno list * LTG.slotno list
end

signature EVALUATOR =
sig
  type move = LTG.app_dir * LTG.card * LTG.slotno
  val move : LTG.app_dir -> LTG.card -> LTG.slotno -> move (* smart ctor for moves *)
  val L : LTG.card -> LTG.slotno -> move               (* left app *)
  val R : LTG.slotno -> LTG.card -> move               (* right app *)

  structure Diff : DIFF

  val switch_teams : LTG.board -> LTG.board

  (* evaluates, modifying a given diff appropriately *)
  val evalWithDiff : LTG.board -> LTG.comb -> bool -> Diff.t ref -> LTG.comb option

  (* eval LTG.board K is_zombie ==> result
   *
   * evaluates LTG.combinator K in LTG.board. acts like a zombie iff is_zombie is true. Returns
   * (SOME result) or NONE on error, tupled with the diff.
   *)
  val eval : LTG.board -> LTG.comb -> bool -> LTG.comb option * Diff.t

  (* run_zombies LTG.board ==> ()
   *
   * Runs all zombies on the LTG.board. Use before turn. *)
  val run_zombies : LTG.board -> Diff.t

  (* play_card board move ==> (result, diff)
   *
   * Use run_zombies before this.
   *)
  val play_card : LTG.board -> move -> LTG.comb option * Diff.t

  (* run_move_and_swap board move ==> (result, diff)
   *
   * runs the move, then swaps and runs zombies.
   *)
  (* val run_move_and_swap : LTG.board -> move -> LTG.comb option * diff *)

  (* run_move_old LTG.board move ==> result
   *
   * Returns (SOME result) or NONE on error.
   *
   * XXX DO NOT USE, does not allow you to react to zombies
   *)
  val run_move_old : LTG.board -> move -> LTG.comb option

  (* run_moves LTG.board moves ==> ()
   *
   * XXX don't use, assumes opponent does nothing
   *)
  (* val run_moves_assuming_dumb_opponent : LTG.board -> move list -> unit *)
end

structure Evaluator : EVALUATOR =
struct
  open Util infixr 0 $

  open LTG
  infix &
  fun !! (x,y) = valOf $ IntMap.find (!x,y)
  infix 9 !!
  fun sub x y = valOf $ IntMap.find (!x, y)
  fun up x y z = x := IntMap.insert (!x, y, z)

  exception EvalError of string
  val TooManyApps = EvalError "too many apps"
  val Stuck = EvalError "stuck"
  val TooBig = EvalError "too big"
  val NotDead = EvalError "not dead"
  val Dead = EvalError "slot dead"
  val NotNum = EvalError "not a number"

  type move = app_dir * card * slotno

  fun switch_teams (B {f, v, f', v'}) = B {f=f', v=v', v'=v, f'=f}

  (* diff - Maps slotnumber to (old vitality, new vitality, changed field value). *)
  structure Diff : DIFF =
  struct
    type halfdiff = (vitality * vitality * bool) IntMap.map
    type t = halfdiff * halfdiff

    val empty : t = (IntMap.empty, IntMap.empty)
    fun flip ((x,y) : t) : t = (y,x)
    fun combine ((x1,x2) : t) ((y1,y2) : t) : t =
        let fun combine ((oldvit, _, changed1), (_, newvit, changed2)) =
                (oldvit, newvit, changed1 orelse changed2)
        in mapBoth (IntMap.unionWith combine) ((x1,y1),(x2,y2))
        end

    fun keysFilter p = IntMap.keys o IntMap.filter p

    fun killed (x : t) = mapBoth (keysFilter (fn (_,newv,_) => newv <= 0)) x
    fun cleared (x : t) = mapBoth (keysFilter (fn (_,_,cleared) => cleared)) x
  end

  (* smart ctor for moves *)
  fun move app_dir card slotno : move =
      if is_valid_slot slotno then (app_dir, card, slotno)
      else raise Fail "invalid move"

  val L = move LeftApp
  val R = flip (move RightApp)

  fun clamp n = if n < 0 then 0 else if n > max then max else n

  fun evalWithDiff (B {f, v, f', v'}) expr zombie diff = let
      (* maps from slots to old vitalities *)
      val ref (ours, theirs) = diff
      val changesOurs = ref ours
      val changesTheirs = ref theirs

      fun setVit vits hdiff idx newvit =
          let val oldvit = IntMap.look' (!vits) idx
              val _ = IntMap.bind (!vits) idx newvit
              val zombified = newvit = ~1
              val _ = hdiff := IntMap.bind (!hdiff) idx
                  (case IntMap.look (!hdiff) idx
                    of NONE => (oldvit, newvit, zombified)
                     | SOME (oldvit, _, cleared) => (oldvit, newvit, cleared orelse zombified))
          in ()
          end

      val upV = setVit v changesOurs
      val upV' = setVit v' changesTheirs

      val ++ = if zombie then (op -) else (op +)
      val -- = if zombie then (op +) else (op -)
      infix 6 ++ --

      fun num f (CVal n) = f n
        | num _ _ = raise NotNum

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
                           (fn i => let val n = v !! i
                                        val () = if is_dead n then ()
                                                 else upV i $ clamp $ n++1
                                    in %CI end) e
             | %CDec & e => num
                           (fn i => let val n = v !! (max_slot-i)
                                        val () = if is_dead n then ()
                                                 else upV (max_slot-i) $ clamp (n--1)
                                    in %CI end) e
             (* attack and help have a bunch of corner cases. *)
             | %CAttack & CVal i & arse & CVal n =>
               let val () = if v !! i < n then raise TooBig else
                            upV i $ (v !! i) - n
                   val j = max_slot - num id arse
                   val () = if is_dead $ v' !! j  then () else
                            upV' j $ clamp $ v' !! j -- (n * 9 div 10)
               in %CI end
             | %CAttack & _ & _ & _ => raise Stuck
             | %CHelp & CVal i & arse & CVal n =>
               let val () = if v !! i < n then raise TooBig else
                            upV i $ (v !! i) - n
                   val j = num id arse
                   val () = if is_dead $ v' !! j then () else
                            upV' j $ clamp $ v' !! j ++ (n * 11 div 10)
               in %CI end
             | %CHelp & _ & _ & _ => raise Stuck
             | %CCopy & e => num (sub f') e
             | %CRevive & e => num (fn i => (if is_dead (v !! i) then upV i 1 else (); %CI)) e
             | %CZombie & CVal i & x =>
               (if is_dead $ v' !! (max_slot-i) then () else raise NotDead;
                up f' (max_slot - i) x;
                upV' (max_slot - i) ~1;
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

      fun error e = Print.esay ("ERR: " ^ e ^ (if zombie then "Z" else "?"))

      val result = SOME $ #1 $ app expr 0
          handle EvalError s => (error s; NONE)
               | e => (error (exnMessage e); NONE)

      val _ = diff := (!changesOurs, !changesTheirs)

  in result
  end

  fun eval board comb isZombie =
      let val diff = ref Diff.empty
          val result = evalWithDiff board comb isZombie diff
      in (result, !diff)
      end

  (* To be run before a turn. Runs all of the zombies *)
  fun run_zombies (board as B{f,v,...}) =
      let val diff = ref Diff.empty
          fun handle_zombie (i, ~1) =
              (evalWithDiff board (CApp (f !! i, %CI)) true diff;
               up f i $ %CI;
               0)
            | handle_zombie (_, n) = n
      (* note: mapi required by spec to be in-order *)
      in (v := IntMap.mapi handle_zombie (!v); !diff) end

  fun play_card (board as B{f,v,...}) (direction, card, slot_num) =
      let val slot = f !! slot_num
          val (result, diff) =
              if is_dead $ v !! slot_num then (NONE, Diff.empty) else
              (eval board (case direction of
                                        LeftApp => CApp (%% card, slot)
                                      | RightApp => CApp (slot, %% card)) false)
          val () = up f slot_num $ getOpt (result, %CI)
      in (result, diff) end

  fun run_move_old board move =
      (Print.esay "WARN: run_move_old invoked; DEPRECATED"; (* see sig *)
       run_zombies board;
       first $ play_card board move)

  (* fun run_moves board moves = List.app (ignore o run_move_old board) moves *)

end
