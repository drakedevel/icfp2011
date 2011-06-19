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

  datatype state = Start
                 | BuildingAttack of move list * slotno list * (board -> state)
                 | RunningAttack of move list * slotno list * (board -> state)
  datatype data = D of {state: state, analysis: A.anal_comb}

  fun allocHealthy a (B{v,...}) n = M.allocFilter (fn i => (v !! i) > n) a
  val threshold = 8192

  (* Let's fire off a job... *)
  fun wonton_snipe (regs,reload_reg,tr) (B {v'=v',v=v,...}) =
  case LTG.BoardMap.firsti (LTG.BoardMap.filter (fn x => x > 0) (!v')) of
      SOME (slot,vit) =>
      RunningAttack (Load.int tr slot @ [R reload_reg CZero], regs, wonton_snipe (regs,reload_reg,tr))
    | NONE _ => RunningAttack (Load.int tr 0 @ [R reload_reg CZero], regs, wonton_snipe (regs,reload_reg,tr))
      end

  fun build_attack b random old  = let
      val a = if random andalso not old then allocator else Allocator.cheap allocator
      val ((snipe,tr,reload_reg),zomb,reload,regs) = 
          if old then Terms.zombocanic_old a else
          Terms.zombocanic a (allocHealthy a b threshold) (allocHealthy a b threshold)

      val shoot = R reload_reg CZero
      fun continue_snipe _ = 
          RunningAttack (Load.int tr 66 @ [shoot, L CDbl tr, shoot] 
                         @ Load.int tr (66*3) @ [shoot], regs, wonton_snipe (regs,reload_reg,tr))
      fun load_resnipe _ = BuildingAttack (reload, regs, continue_snipe)
      fun pull_trigger _ = RunningAttack ([R snipe CZero], regs, load_resnipe)

      val initialize = BuildingAttack (zomb, regs, pull_trigger)
  in 
      initialize
  end

  val frees = M.freeMany allocator
  fun logic {state, analysis} diff board = let 
      val (analysis', (revives, kills), (our_diffs, their_diffs)) =
          A.update analysis board
      val (dead , _) = Diff.cleared diff
      val bring_out_yer_dead = List.filter (flip contains $ dead)


      fun step (Start) = step $ build_attack board false true
        | step (BuildingAttack ([], _, next)) =
          step $ next board
        | step (BuildingAttack (x::xs, regs, next)) =
          (x, BuildingAttack (xs, regs, next))

        | step (RunningAttack (x::xs, regs, next)) =
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
        val moves = (Terms.take_him_down 2) @ (Terms.take_him_down 3) @
      (Terms.take_him_down 4) @ (Terms.take_him_down
          0) @ (Util.replicate 100000 (Evaluator.L LTG.CI 72))
      in map (fn x => (ReaderWriter.put_move x; ReaderWriter.get_move ()))
      moves; raise Fail "Fuck God Dead"
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
