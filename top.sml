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

  datatype state = Start
                 | BuildingAttack of move list * slotno list * (unit -> state)
                 | RunningAttack of move list * slotno list * (unit -> state)
  datatype data = D of {state: state, analysis: A.anal_comb}

  (* Let's fire off a job... *)
  fun build_attack () = let
      fun fire () = ignore (Job.schedule [R 1 CGet, R 1 CZero, R 1 CZero] Job.RForever)

      val ((snipe,tr,reload_reg),zomb,reload,regs) = Terms.zombocanic allocator
      val do_snipe = R snipe CZero :: reload
      val careful_snipe = Load.int tr 66 @ [R reload_reg CZero, L CDbl tr, R reload_reg CZero]
      fun re_snipe x () = 
          RunningAttack (Load.int tr x @ [R reload_reg CZero], regs, re_snipe $ (x+1) mod (256))
      val attack = zomb @ do_snipe @ careful_snipe
  in 
      BuildingAttack (attack, regs, re_snipe (66*3))
  end

  val frees = M.freeMany allocator
  fun logic {state, analysis} board = let 
      val (analysis', (revives, kills), (our_diffs, their_diffs)) =
          A.update analysis board

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
      fun proponent state b = let
          val (state', mv) = logic state b
      in
          ReaderWriter.put_move mv;
          run_move b mv;
          opponent state' (switch_teams b)
      end
      and opponent state b = let
          val mv = ReaderWriter.get_move ()
      in
          run_move b mv;
          proponent state (switch_teams b)
      end
  in
      fun main (name, args) = 
          let val board = build_board ()
              val state = {state = Start, analysis = A.init_state}
          in
              case args of
                  ["0"] => proponent state board
                | ["1"] => opponent state board
                | _ => raise Fail "Incorrect arguments"
          end
      fun main' args = main args
          handle e => (Print.esay ("Unhandled exception: " ^ exnMessage e);
                       OS.Process.failure)
  end

end
