structure Terms =
struct
local
    open Util infixr 0 $
    open LTG
    open UTLCNamed
    val ` = EVar
    val ? = EApp
    infix 0 $$
    val (op $$) = CApp
    infix 7 ?
    val vars = map Variable.named
    val [x, y, z, n, m, f, n', m', e, a, b,
         x', x'', x''', y', y'', y''', k, w,
         j, i, l, c, d, q, r, u, v, 
         a', b', a'', b'', h ] =
        vars ["x", "y", "z", "n", "m", "f", "n'", "m'", "e", "a", "b",
              "x'", "x''", "x'''", "y'", "y''", "y'''", "k", "w",
              "j", "i", "l", "c", "d", "q", "r", "u", "v",
              "a'", "b'", "a''", "b''", "h" ]
    val S = %CS
    val K = %CK
    val I = %CI
in

  (* these two shouldn't work CBV *)
  val Y1 = S ? S ? S ? (S ? (K ? (S ? S ? (S ? (S ? S ? K)))) ? K)
  val Y2 = ELam (f, ELam (x, `x ? `x) ?
                         ELam (x, `f ? (`x ? `x)))

  (* this might? *)
  val Y = ((S ? S) ? K) ? ((S ? (K? ((S ? S) ? (S ? ((S ? S) ? K))))) ? K)
  (* this should *)
  val Y3 = ELam (f, ELam (x, `x ? `x) ?
                         ELam (x, `f ? ELam (y, `x ? `x ? `y)))

  fun repeat_n n = let fun e n = if n = 0 then `x else `f ? (e (n - 1)) in ELam (f, ELam (x, e n)) end

  fun dec_n n = %CDec ? EVal n
  fun inc_n n = %CInc ? EVal n
  fun copy_n n = %CGet ? EVal n

  val noobY = S ? I ? I

  val seq = %CPut
  val seqL = Util.foldr1 (fn (e1, e2) => seq ? e1 ? e2)

  fun repeat F = noobY ? ELam (h, ELam (x, seq ? (F ? `x) ? (`h ? `h)))
  val repeat_lam = ELam (f, repeat (`f))

  fun thunk E = ELam (x, E)

  fun repeat_n n = 
      let fun e n = if n = 0 then `x else `f ? (e (n - 1)) 
      in ELam (f, ELam (x, e n)) end

  val repeat_kill = repeat_lam ? thunk (seqL [dec_n 0, dec_n 1, dec_n 2])
  val ski = Compile.convertExpr
  local open LTG in
  fun spin n x = 
      %CS $$ (%CS $$ ( %CK $$ %CGet ) $$ (%CK $$ (CVal n))) $$ (%CS $$ x $$ %CSucc)
  fun reshoot n x =
      %CS $$ (%CK $$ (%CS $$ ( %CK $$ %CGet ) $$ (%CK $$ (CVal n)))) $$ x
  end


  fun make_spin n x = 
      let
          val alloc = Allocator.new ();
          val _ = Allocator.use alloc n in
          Load.load alloc n (spin n x)
      end
  val nyan_cat = make_spin 0 (LTG.%CDec)
  fun load e = Load.load (Allocator.new ()) 0 e
  val load' = load o ski
  fun n_cats n = repeat_lam ? ((repeat_n n) ? (thunk $ seqL $ map dec_n [0, 1,
    42]))

end
end
