structure Top =
struct
  open Util infixr 0 $
  open LTG
  open Evaluator
  infix 7 ?
  val (op ?) = CApp
  fun !! (x,y) = valOf $ IntMap.find (!x,y)
  infix 9 !!

  val rand = Random.rand (0x1337BABE, 0x1337D00D)

  fun randDir () = case (Random.randRange (0,1) rand) of
               0 => LeftApp
             | 1 => RightApp
             | _ => raise Fail "wtf"

  fun randCard () = int_to_card (Random.randRange (0,14) rand)

  fun randSlot () = Random.randRange (0,255) rand

  fun randMove b = let
      val b' = copy_board b
      val dir = randDir ()
      val card = randCard ()
      val slot = randSlot ()
      val mv = move dir card slot
      val res = run_move_old b mv
  in
      case res of
      SOME _ => (b, mv)
    | NONE => randMove b'
  end
(*
  local
      fun proponent b = let
          val (b', mv) = randMove b
          val _ = ReaderWriter.put_move mv
      in opponent b' end

      and opponent b = let
          val mv = ReaderWriter.get_move ()
          val _ = run_move_old b mv
      in proponent b end
  in
      fun random_main (name, args) = case args of
                  ["0"] => proponent (build_board ())
                | ["1"] => opponent (build_board ())
                | _ => raise Fail "what do you want from me?"
                  (*handle _ => OS.Process.success*)
  end

  local
      fun xbox () =
      let val alloc = Allocator.new ()
          val [0,1] = Allocator.allocMany alloc 2
          val payload = %CS ? (%CGet ? %CZero) ? (%CGet ? %CZero)
          val copy_back = %CGet ? CVal 1
      in Load.load alloc 1 payload @ Load.load alloc 0 copy_back end
      val xbox_moves = xbox ()

      fun proponent (mv::mvs) = let
          val _ = ReaderWriter.put_move mv
      in opponent mvs end
        | proponent [] = proponent xbox_moves

      and opponent mvs = let
          val mv = ReaderWriter.get_move ()
      in proponent mvs end
  in
      fun xbox_main (name, args) = case args of
                  ["0"] => proponent []
                | ["1"] => opponent []
                | _ => raise Fail "what do you want from me?"
                  (*handle _ => OS.Process.success*)
  end

  local
      val setup = Terms.load' Terms.repeat_kill
      val noobing = [ R 0 CI ]

      fun proponent b (mv::mvs) = let
          val _ = ReaderWriter.put_move mv
          val _ = run_move_old b mv
      in opponent b mvs end
        | proponent b [] = proponent b noobing

      and opponent b mvs = let
          val mv = ReaderWriter.get_move ()
          val _ = run_move_old b mv
      in proponent b mvs end
  in
      fun noob_main (name, args) = case args of
                  ["0"] => proponent (build_board ()) setup
                | ["1"] => opponent (build_board ()) setup
                | _ => raise Fail "what do you want from me?"
                  (*handle _ => OS.Process.success*)
  end
*)

  datatype state_change = SDied | SRevived | SNothing

  fun diff_boards (old as B{v=v1,v'=v1',...}) (new as B{v=v2,v'=v2',...}) =
    let val (mine, theirs) = (v1, v1')
        fun diff (i, vitality) = vitality - v1 !! i
    in
        (IntMap.mapi diff (!mine), IntMap.mapi diff (!theirs))
    end

  val fuck_it_threshold = 100
  fun expr_size e =
      let fun sz e n =
              if n >= fuck_it_threshold then raise Fail "fuck it" else
              case e of
                  CVal _ => n+1
                | CVar _ => n+1
                | CApp (e1, e2) => sz e1 $ sz e2 (n+1)
                | & (e1, e2) => sz e1 $ sz e2 (n+1)
                | %c => n+1
    in
          SOME (sz e 0)
          handle _ => NONE
    end

  fun get_board_info old_board new_board =
      let val (my_diff, their_diff) = diff_boards old_board new_board
      in (my_diff, their_diff) end


  val allocator = Allocator.new ()
  structure M = Allocator
  structure A = Analysis

  datatype state = Start
                 | BuildingAttack of move list * slotno list * (unit -> state)
                 | RunningAttack of move list * slotno list * (unit -> state)
  datatype data = D of {state: state, analysis: A.anal_comb}

(*(x+1) mod (256))*)
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
