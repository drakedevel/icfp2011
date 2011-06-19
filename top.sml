structure Top =
struct
  open Util infixr 0 $
  open LTG
  open Evaluator
  infix 7 ?
  val (op ?) = CApp
  fun !! (x,y) = valOf $ IntMap.find (!x,y)
  infix 9 !!

  datatype state_change = SDied | SRevived | SNothing

  val allocator = Allocator.new ()
  structure M = Allocator
  structure A = Analysis
  structure Diff = Evaluator.Diff
  structure BM = LTG.BoardMap

  datatype state = Start
                 | BuildingAttack of move list * slotno list * (board -> state)
                 | RunningAttack of move list * slotno list * (board -> state)
  datatype data = D of {state: state, analysis: A.anal_comb}

  fun allocHealthy a (B{v,...}) n = M.allocFilter (fn i => (v !! i) > n) a
  val threshold = 8192

  val loadInt = Load.int
  val load = Load.load

  fun is_zombo_killable i = i > 8192
  fun nextpow2 v n = if v < n
                     then nextpow2 (v * 2) n
                     else v
  fun yieldify (lo, hi) =
      let
          val p = (nextpow2 1 lo)
      in
          if p <= hi then
              p
          else
              lo
      end

  fun zamboni _ (lo, hi) k 256 = ((lo, hi), k)
    | zamboni (b as B{v',...}) (lo, hi) k i =
      let val k' = v' !! i
          val lo' = Int.max (lo, 1 + (10 * k' div 21))
          val hi' = Int.min (hi, k')
      in if lo' > hi' then ((lo, hi), k) else
         zamboni b (lo', hi') (k+1) (i+1)
      end
  fun cmp_ranges ((_, len), (_, len')) = Int.compare (len, len')
  fun find_largest_range b =
      max_elem cmp_ranges $ map (zamboni b (0, max) 0) $ upto num_slots

  (* Let's fire off a job... *)
  fun nato (regs, reload_reg, tr, yield_reg) b =
      case find_largest_range b of
          NONE => raise Fail "Fucked"
        | SOME (start, ((lo, hi), _)) =>
          let
              val yield = yieldify (lo, hi)
          in
              RunningAttack (loadInt yield_reg yield, regs, b29 (regs, reload_reg, tr, yield_reg) start)
          end
      
  and b29 (regs,reload_reg,tr,yield_reg) start b =
      RunningAttack ((loadInt tr start) @ [R reload_reg CZero], regs, nato (regs, reload_reg, tr, yield_reg))

  fun wonton_snipe (regs,reload_reg,tr) (B {v'=v',v=v,...}) =
  case max_elem Int.compare $ sequenceLengths is_zombo_killable $ BM.elems (!v') of
      SOME (slot,vit) =>
      RunningAttack ((loadInt tr slot) @ [R reload_reg CZero], regs, wonton_snipe (regs,reload_reg,tr))
    (* FIXME: we need to do something reasonable here. this is completely useless. *)
    | NONE => RunningAttack (loadInt tr 0 @ [R reload_reg CZero], regs, wonton_snipe (regs,reload_reg,tr))

  fun build_attack b random old  = let
      val a = if random andalso not old then allocator else Allocator.cheap allocator
      val ((snipe,tr,reload_reg,yield_reg),zomb,reload,regs) = 
          if old then Terms.zombocanic_old a else
          Terms.zombocanic_selectable_yield a (allocHealthy a b threshold) (allocHealthy a b threshold)

      val shoot = R reload_reg CZero
      fun continue_snipe _ = 
          RunningAttack (Load.int tr 66 @ [shoot, L CDbl tr, shoot] 
                         @ Load.int tr (66*3) @ [shoot], regs, wonton_snipe (regs,reload_reg,tr))
      fun load_resnipe _ = BuildingAttack (reload, regs, if old then (continue_snipe) else (nato (regs,reload_reg,tr,yield_reg)))
      fun pull_trigger _ = RunningAttack ([R snipe CZero], regs, load_resnipe)

      val initialize = BuildingAttack (zomb, regs, pull_trigger)
  in 
      initialize
  end

  val frees = M.freeMany allocator
  fun logic {state, analysis} diff board = let 
      val (analysis', (revives, kills), (our_diffs, their_diffs)) =
          A.update analysis board
      val (_ , dead) = Diff.killed diff
      val bring_out_yer_dead = List.filter (flip contains $ dead)

      (* ignoring our attack regs doesn't really work *)
      fun check_attack_regs regs = not $ null $ bring_out_yer_dead $ List.drop (regs, 2)
      fun restart_attack regs = (frees regs; step $ build_attack board true false)

      and step (Start) = step $ build_attack board false true
        | step (BuildingAttack ([], _, next)) =
          step $ next board
        | step (BuildingAttack (x::xs, regs, next)) =
          if check_attack_regs regs then restart_attack regs else
          (x, BuildingAttack (xs, regs, next))

        | step (RunningAttack (x::xs, regs, next)) =
          if check_attack_regs regs then restart_attack regs else
          (x, RunningAttack(xs, regs, next))
        | step (RunningAttack ([], _, next)) =
          step $ next board

      val (move, state') = step state

  in ({state=state', analysis=analysis'}, move) end
                                               
  local
      fun proponent state b diff = let
          val () = M.update allocator b
          val (state', mv) = logic state diff b
      in
          ReaderWriter.put_move mv;
          run_move b mv;
          opponent state' (switch_teams b)
      end
      and opponent state b = let
          val mv = ReaderWriter.get_move ()
          val diff = run_move b mv
      in
          proponent state (switch_teams b) diff
      end
  in
      fun takedown_main (name, args) =
      let val _ = if args = ["1"] then ignore (ReaderWriter.get_move ()) else ()
        val a = allocator
        val moves = (Terms.take_him_down a 2) @ (Terms.take_him_down a 3) @
                    (Terms.take_him_down a 4) @ (Util.replicate 100000 (Evaluator.L LTG.CI 72))
      in map (fn x => (ReaderWriter.put_move x; ReaderWriter.get_move ())) moves;
         raise Fail "Fuck God Dead"
      end
      fun main (name, args) = 
          let val board = build_board ()
              val state = {state = Start, analysis = A.init_state}
          in
              case args of
                  ["0"] => proponent state board Evaluator.Diff.empty
                | ["1"] => opponent state board
                | _ => raise Fail "Incorrect arguments"
          end
      fun main' args = main args
          handle e => (Print.esay ("Unhandled exception: " ^ exnMessage e);
                       OS.Process.failure)
  end

end
