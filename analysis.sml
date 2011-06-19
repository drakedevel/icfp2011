structure Analysis =
struct
open LTG

datatype expr_size = SizeDepth of int | XBOX
datatype analyzed_enemy_state = Unknown | Dull | Asset
datatype analyzed_our_state = Untouched | Targeted | Normal
(* healscore, modscore, size, thing to report if queried *)
datatype tracked = EnemyState of (real * real * expr_size *
                                  analyzed_enemy_state)
                 (* hitscore, health, thing to report if queried *)
                 | OurState of (real * analyzed_our_state)

type anal_comb = tracked IntMap.map * tracked IntMap.map * LTG.board

local fun inserter x = fn (i, m) => IntMap.insert(m, i, x)
    fun init_all x = foldl (inserter x) IntMap.empty (Util.upto(255))
in
val init_state = (init_all (OurState (0.0, Untouched)), 
                  init_all (EnemyState (0.0, 0.0, (SizeDepth 0), Dull)), build_board ())
end
val xbox_size = 100
fun sizeOf e =
    let fun sz e n = if n >= xbox_size then raise Fail "XBOX" else
                     case e of
                         CVal _ => n + 1
                       | CVar _ => n + 1
                       | CApp (e1, e2) => sz e1 (sz e2 (n + 1))
                       | & (e1, e2) => sz e1 (sz e2 (n + 1))
                       | % _ => n + 1
    in SizeDepth (sz e 0)
       handle _ => XBOX
    end
fun us (B {v = z,...}) = !z
fun them (B {v' = z,...}) = !z
fun szDiff XBOX _ = XBOX
  | szDiff _ XBOX = XBOX
  | szDiff (SizeDepth x) (SizeDepth y) = SizeDepth (abs (x - y))
local val decay_rate = 0.95
    fun decay_helper (EnemyState(a, b, c, d)) = 
        EnemyState((decay_rate * a), (decay_rate * b), c, d)
      | decay_helper (OurState(a, b)) = OurState((decay_rate * a), b)
    fun decay state = IntMap.map decay_helper state
    fun diff_health' m1 m2 = IntMap.intersectWith (fn (x, y) => y - x) (!m1, !m2)
    fun diff_health ((B {v = v1, v' = v1',...}), (B {v = v2, v' = v2',...})) =
        ((diff_health' v1 v2), (diff_health' v1' v2'))
    val attack_thresh = 200.0
    fun update_us hits (i, m) =
        let val (OurState(v, b)) = IntMap.lookup(m, i)
            val adjust = case IntMap.find(hits, i) of
                             SOME(x) =>
                             Real.fromInt(~x)
                           | NONE    => 0.0
            val score = adjust + v
            val label = if score < 1.0 then
                            Untouched else (if score >
                                               attack_thresh then Targeted else
                                            Normal) in
            IntMap.insert(m, i,
                          OurState(score, label))
        end
    fun update_them(heals, board) (i, m) =
        let val (EnemyState(healscore, modscore, sz, _)) = IntMap.lookup(m, i)
            val healscore' = case IntMap.find(heals, i) of
                                 SOME(x) => Real.fromInt(x) + healscore
                               | NONE    => healscore
            val (B {f' = enemyVals,...}) = board 
            val sz' = sizeOf (IntMap.lookup(!enemyVals, i))
            val modscore' = case szDiff sz sz'
                             of
                                XBOX => modscore + 2.0
                              | SizeDepth(0) => modscore
                              | SizeDepth(_) => modscore + 1.0
            val label = if modscore' * 100.0 + healscore' > 200.0 then Asset else
                        (if modscore' < 1.0 andalso healscore' < 1.0 then Dull else Unknown)
        in IntMap.insert(m, i, EnemyState(healscore', modscore', sz', label))
        end
    val health = Util.curry2 IntMap.lookup
in
(* update :: (state, board) -> (state, (revives, kills)) *)
fun update (us_state, them_state, old_board) board =
    let val state' as (us_state', them_state', _) = (decay us_state, decay
                                                                         them_state, board)
        val (our_diffs, their_diffs) = diff_health(old_board, board)
        val hits = IntMap.filter (fn x => x < 0) our_diffs
        val heals = IntMap.filter (fn x => x > 0) their_diffs
        val kills = IntMap.filteri (fn (k, _) => (0 <> (health (us old_board) k)))
                                   hits
        val revives = IntMap.filteri (fn (k,_) => (0 = (health (them old_board)
                                                               k))) heals
        val us_state'' = foldr (update_us hits) us_state' (Util.upto(255))
        val them_state'' = foldr (update_them(heals, board)) them_state'
                                 (Util.upto(255))
    in ((us_state'', them_state'', copy_board board),
        (IntMap.listKeys(revives), IntMap.listKeys(kills)), (our_diffs,
        their_diffs))
        end
end
fun analysis_us (m, _, _) i = let val OurState(_, l) = IntMap.lookup m in l end
fun analysis_them (_, m, _) i = let val EnemyState(_, _, _, l) = IntMap.lookup m 
                                in l end
end
