structure Terms =
struct
local
    open Util infixr 0 $
    open LTG
    open UTLCNamed
    val ` = EVar
    val ? = EApp
    infix 7 ?
    val vars = map Variable.named
    val [x, y, z, n, m, f, n', m', e, a, b,
         x', x'', x''', y', y'', y''', k, w,
         j, i, l, c, d, q, r, u, v, 
         a', b', a'', b'', h, g, z', z'', z''' ] =
        vars ["x", "y", "z", "n", "m", "f", "n'", "m'", "e", "a", "b",
              "x'", "x''", "x'''", "y'", "y''", "y'''", "k", "w",
              "j", "i", "l", "c", "d", "q", "r", "u", "v",
              "a'", "b'", "a''", "b''", "h", "g", "z'", "z''", "z'''" ]
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
  fun repeat_ctr F = noobY ? ELam (h, ELam (n, ELam (g, seq ? (F ? `n) ? (`h ? `h ? (`g ? `n)))))
  val repeat_lam_ctr = ELam (f, repeat_ctr (`f))

  fun thunk E = ELam (x, E)
(* Precondition:
* Health ourDead and theirDead are zero
* benignCommand and dickingCommand each contain a thunk
* Postcondition:
* Every turn, dickingCommand will be executed on the remote host, and
* benignCommand will be executed locally.
*)
  fun zombocom ourDead theirDead benignCommand dickingCommand = noobY ? ELam (f,
    %CZombie ? EVal (255 - theirDead) ? (ELam (z, ELam(z', %CZombie ? EVal
    (255 - ourDead) ? (ELam(z'', ELam(z''', `f ? `f) ? ((%CGet ?
    EVal(benignCommand) ? `z''))))) ? (%CCopy ? (EVal(dickingCommand)) ? `z))))

  fun repeat_n n = 
      let fun e n = if n = 0 then `x else `f ? (e (n - 1)) 
      in ELam (f, ELam (x, e n)) end

  val repeat_kill = repeat_lam ? thunk (seqL [dec_n 0, dec_n 1, dec_n 2])

  (* Code for an attack *)
  fun attack_from j n = ELam (i, %CAttack ? `i ? EVal j ? EVal n)

  fun be_a_helper amt i = thunk (%CHelp ? i ? i ? EVal amt)
  fun zombie_helper target = ELam (i, %CZombie ? EVal target ? be_a_helper 10000 (`i))


  val ski = Compile.convertExpr
  fun spin n x = 
      ski (S ? (S ? (K ? %CGet) ? (K ? (EVal n))) ? (S ? x ? %CSucc))

  fun spin' n x = 
      ski (S ? (S ? (K ? %CCopy) ? (K ? (EVal n))) ? (S ? x ? %CSucc))

  fun reshoot n x =
      ski (S ? (K ? (S ? ( K ? %CGet ) ? (K ? (EVal n)))) ? x)

  fun gun n f = ski (S ? (K ? f) ? ((S ? (K ? %CGet) ? (K ? EVal n))))

  (* fun make_spin n x = 
   *     let
   *         val alloc = Allocator.new ();
   *         val _ = Allocator.use alloc n in
   *         Load.load alloc n (spin n x)
   *     end
   * val nyan_cat = make_spin 0 (%CDec)
   * val volcanic =
   *     let
   *         val a = Allocator.new ();
   *         val gr = Allocator.alloc a;
   *         val sr = Allocator.alloc a;
   *         val tr = Allocator.alloc a;
   *         val gun_arg = (S ? (%CGet) ? ((S ? (K ? %CGet) ? (K ? EVal tr))))
   *         val gun = ski (S ? (K ? gun_arg) ? ((K ? EVal sr)))
   *         val gun' =  Load.load a gr gun
   *         val volc = Load.load a sr (spin sr (%CDec))
   *     in
   *         (tr, gun' @ volc)
   *     end *)

  val snipe =
      S  ?(S ? (S ? (%CAttack ? (%CSucc ? (%CGet ? EVal 1))) ? (K ? (%CGet ? EVal 0))) ? 
          (S ? (%CAttack ? (%CGet ? (EVal 1))) ? (K ? (%CGet ? EVal 0))))?
         (S ? %CZombie ? (S ? (K ? %CGet) ? %CSucc ))
  (*fastload, don't bother to left-apply CPut*)
  fun fint reg x = List.tl (Load.int reg x)

  val zombocanic =
      let
          val (L,R) = (Evaluator.L,Evaluator.R)
          val a = Allocator.new ();
          (*steal 0, so it does not hurt when they snipe it*)
          val _ =  Allocator.alloc a;
          val gr = Allocator.alloc a;
          val sr = Allocator.alloc a;
          val target_reg = Allocator.alloc a;
          val snipe_reg = Allocator.alloc a;
          val reshoot_reg = Allocator.alloc a;
          val reshooter = 
              Load.load a  reshoot_reg (reshoot reshoot_reg (S ? %CDec ? (S ? %CZombie ? (S ? (K ? %CGet) ? %CSucc ))))
          val snipe = Load.load a snipe_reg (ski snipe)
          (*ugh. this is inside out.  the EVal sr gets passed in as
           * the first argument to leftmost Copy
           *)
          val gun_arg = (S ? (%CCopy(*sr*)) ? ((S ? (K ? %CCopy) ? (K ? EVal target_reg))))
          val gun = ski (S ? (K ? gun_arg) ? ((K ? EVal sr)))
          val gun' =  Load.load a gr gun
          val volc = Load.load a sr (spin' sr (S?(S? %CHelp?I)?(K?(%CGet ? EVal 0))))
          fun rep 0 _ = []
            | rep n x = x::rep (n-1) x
      in
          ((snipe_reg, target_reg,reshoot_reg),
           fint 0 16 @ [R 1 CGet, R 1 CZero] @ rep 9 (L CDbl 0) @
           snipe @ volc @ gun' @Load.int target_reg 0, reshooter)
      end

  (* fun load e = Load.load (Allocator.new ()) 0 e
   * 
   * fun load_n e n = Load.load (Allocator.new ()) n e
   * val load' = load o ski
   * fun n_cats n = repeat_lam ? ((repeat_n n) ? (thunk $ seqL $ map dec_n [0, 1,
   *   42])) *)

end
end
