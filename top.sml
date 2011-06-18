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
      val res = run_move b mv
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
          val _ = run_move b mv
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
          val _ = run_move b mv
      in opponent b mvs end
        | proponent b [] = proponent b noobing

      and opponent b mvs = let
          val mv = ReaderWriter.get_move ()
          val _ = run_move b mv
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

  fun logic info = ()

  (* Let's fire off a job... *)
(*
  fun fire () = ignore (Job.schedule [R 1 CGet, R 1 CZero, R 1 CZero] Job.RForever)

  fun addNoobing () = ignore (Job.schedule [ R 0 CI ] Job.RForever)
  *)                    
  (*val _ = Job.schedule (Terms.load' Terms.repeat_kill) (Job.ROnce addNoobing)*)
  (*val _  = Job.schedule (Terms.nyan_cat) (Job.ROnce fire)*)
local
  open UTLCNamed
  val (op ?) = EApp

in

  fun addNoobing () = ignore (Job.schedule [ R 0 CSucc ] Job.RForever)
  val asser = Terms.load' (Terms.repeat_lam_ctr ? Terms.zombie_helper 0 ? %CZero)
  fun loader () = Job.schedule (asser) (Job.ROnce addNoobing)
  fun initial_attack i = Terms.load' (%CAttack ? EVal i ? %CZero ? EVal 6000)
  val _ = Job.schedule (initial_attack 5) 
          (Job.ROnce (fn () => ignore $ Job.schedule (initial_attack 6) 
                                      (Job.ROnce (ignore o loader))))

end

  local
      fun proponent b_old b = let
          val () = logic (get_board_info b_old b)
          val mv = Job.get_move ()
      in
          ReaderWriter.put_move mv;
          run_move b mv;
          opponent (switch_teams b)
      end
      and opponent b = let
          val mv = ReaderWriter.get_move ()
          val b' = copy_board b
      in
          run_move b mv;
          proponent (switch_teams b') (switch_teams b)
      end
  in
      fun main (name, args) = case args of
             ["0"] => proponent (build_board ()) (build_board ())
           | ["1"] => opponent (build_board ())
           | _ => raise Fail "Incorrect arguments"
      fun main' args = main args
          handle e => (Print.esay ("Unhandled exception: " ^ exnMessage e);
                       OS.Process.failure)
  end

end
