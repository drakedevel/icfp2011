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
                 | BuildingAttack of move list * slotno list * (unit -> state)
                 | RunningAttack of move list * slotno list * (unit -> state)
  datatype data = D of {state: state, analysis: A.anal_comb}

  (* Let's fire off a job... *)
  fun build_attack () = let
      val ((snipe,tr,reload_reg),zomb,reload,regs) = Terms.zombocanic_old allocator

(*      val careful_snipe = Load.int tr 66 @ [R reload_reg CZero, L CDbl tr, R reload_reg CZero]*)
      val shoot = R reload_reg CZero
      fun wonton_snipe 0 () = 
          RunningAttack (Load.int tr 0 @ [shoot], regs, wonton_snipe 0)
        | wonton_snipe n () =
          RunningAttack ([L CSucc tr, shoot], regs, wonton_snipe (n+1))
      fun continue_snipe () = 
          RunningAttack (Load.int tr 66 @ [shoot, L CDbl tr, shoot] 
                         @ Load.int tr (66*3) @ [shoot], regs, wonton_snipe 0)
      fun load_resnipe () = BuildingAttack (reload, regs, continue_snipe)
      fun pull_trigger  () = RunningAttack ([R snipe CZero], regs, load_resnipe)

      val initialize = BuildingAttack (zomb, regs, pull_trigger)

  in 
      initialize(*BuildingAttack (attack, regs, re_snipe (66*3))*)
  end

  val frees = M.freeMany allocator
  fun logic {state, analysis} diff board = let 
      val (analysis', (revives, kills), (our_diffs, their_diffs)) =
          A.update analysis board
      val (dead , _) = Diff.cleared diff
      val bring_out_yer_dead = List.filter (flip contains $ dead)


      fun step (Start) = step $ build_attack ()
        (* can we repeat the attack better?
        | step (BuildingAttack ([], regs)) =
          (frees regs; step $ build_attack ())
         *)
        | step (BuildingAttack ([], _, next)) =
          step $ next ()
        | step (BuildingAttack (x::xs, regs, next)) =
          (x, BuildingAttack (xs, regs, next))

        | step (RunningAttack (x::xs, regs, next)) =
          (x, RunningAttack(xs, regs, next))
        | step (RunningAttack ([], _, next)) =
          step $ next ()

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
