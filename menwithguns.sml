structure MenWithGuns =
struct
  open Evaluator
  open LTG
  structure ListMUtil = MonadUtil(ListM)
  open ListMUtil infix <$> <*>
  open Util

  datatype gun = GUN of int
               | NO_GUN

  val cards = [CS,CK,CI,CZero,CSucc,CDbl,CGet,CPut,CInc,CDec,CAttack,CHelp,CCopy,CRevive,CZombie]

  local
      fun idemplay b move = let val b' = copy_board b in (play_card b' move; b') end
      fun has_gun b cell = let
          val steps = map (play_card (copy_board b)) ((move RightApp) <$> cards <*> [cell])
          val carnage = map (second o Diff.killed o second) steps
      in
          isSome (List.find (fn x => not ((length x) = 0)) carnage)
      end
      fun has_gun_in' b cell 0 = if (has_gun b cell) then GUN 0 else NO_GUN
        | has_gun_in' b cell n =
          let
              val work = map (idemplay b) (move <$> [LeftApp, RightApp] <*> cards <*> [cell])
          in
              if List.exists (fn (GUN _) => true | _ => false) (map (fn x => has_gun_in' x cell (n-1)) work)
              then GUN n
              else NO_GUN
          end
  in 
      fun has_gun_in inb cell n = has_gun_in' (switch_teams inb) cell n
      fun guns_in inb n = map (fn x => has_gun_in' (switch_teams inb) x n) (upto 255)
  end
end
