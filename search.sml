signature SEARCH =
sig
    val moves : int -> int -> LTG.slotno -> int -> Evaluator.move list -> Evaluator.move list
    val search : LTG.board -> LTG.comb -> Evaluator.move list
end

structure Search : SEARCH =
struct
  open Evaluator
  open LTG
  structure ListMUtil = MonadUtil(ListM)
  open ListMUtil
  infix 4 <$> <*>

  (* dir card slot *)
  fun moves dirno cardno slotno slots list =
      if slotno >= slots
      then moves dirno (cardno + 1) 0 slots list
      else if cardno > 14
      then moves (dirno + 1) 0 0 slots list
      else if dirno > 1
      then list
      else
          let
              val dir = case dirno of
                            0 => LeftApp
                          | _ => RightApp
              val card = int_to_card cardno
              val slot = slotno
              val mv = move dir card slot
          in
              moves dirno cardno (slotno + 1) slots (mv :: list)
          end

  local
      (*val amvs = moves 0 0 0 1 []*)
      val amvs = move <$> [LeftApp,RightApp] <*> [CGet, CPut, CZero, CSucc, CDbl] <*> [0,1]

      fun ap b mv = let
          val b' = copy_board b
      in
          run_move_old b' mv; b'
      end

      fun ap' b mv = let
          val b' = copy_board b
      in
          run_move_old b' mv
      end

      fun dfs (b : board) outcome mvs chain 0 = foldl (fn (y,x) => case x of
                                                             NONE => (case (ap' b y) of
                                                                          NONE => NONE
                                                                        | SOME r' => (if r' = outcome
                                                                                      then SOME (y :: chain)
                                                                                      else NONE))
                                                           | SOME x' => x) NONE mvs
        | dfs b outcome mvs chain n = foldl (fn (y,x) => case x of 
                                                             NONE => dfs (ap b y) outcome mvs (y :: chain) (n-1)
                                                           | SOME x' => x) NONE mvs

      fun search' b outcome lim =
          case (dfs b outcome amvs [] lim) of
              NONE => (print (Int.toString lim); search' b outcome (lim + 1))
            | SOME x => x

  in
      fun search b outcome = search' b outcome 0
  end
end
