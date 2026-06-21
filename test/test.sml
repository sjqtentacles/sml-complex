(* test.sml

   Strict-TDD suite for `Complex`. Real values are compared with an explicit
   epsilon tolerance (`approx`/`approxC`) since the library is built on
   floating-point `real`; the same `eps` is shared by every check so failures
   are legible and identical across MLton and Poly/ML. *)

structure Tests =
struct

  structure C = Complex

  val eps = 1E~9
  val pi  = Math.pi

  (* Tolerance helpers. *)
  fun approx eps (a, b) = Real.abs (a - b) <= eps
  fun approxC eps (z, w) =
    approx eps (C.re z, C.re w) andalso approx eps (C.im z, C.im w)

  (* Sum a list of complex numbers (left fold over `add`). *)
  fun sumC xs = List.foldl C.add (C.complex (0.0, 0.0)) xs

  (* The n complex n-th roots of unity: exp(2*pi*i*k/n), k = 0 .. n-1. *)
  fun rootsOfUnity n =
    List.tabulate
      (n, fn k => C.exp (C.complex (0.0, 2.0 * pi * real k / real n)))

  fun runAll () =
    let
      val i = C.complex (0.0, 1.0)

      (* ---- Constructors / accessors ---- *)
      val () = Harness.section "Constructors and accessors"
      val () = Harness.check "re" (approx eps (3.0, C.re (C.complex (3.0, ~4.0))))
      val () = Harness.check "im" (approx eps (~4.0, C.im (C.complex (3.0, ~4.0))))
      val () = Harness.check "i = (0, 1)"
                 (approxC eps (i, C.complex (0.0, 1.0)))

      (* ---- Field operations ---- *)
      val () = Harness.section "Arithmetic"
      val () = Harness.check "add"
                 (approxC eps
                    (C.add (C.complex (1.0, 2.0), C.complex (3.0, ~5.0)),
                     C.complex (4.0, ~3.0)))
      val () = Harness.check "sub"
                 (approxC eps
                    (C.sub (C.complex (1.0, 2.0), C.complex (3.0, ~5.0)),
                     C.complex (~2.0, 7.0)))
      val () = Harness.check "mul (i*i = ~1)"
                 (approxC eps (C.mul (i, i), C.complex (~1.0, 0.0)))
      val () = Harness.check "divide ((1+i)/(1+i) = 1)"
                 (approxC eps
                    (C.divide (C.complex (1.0, 1.0), C.complex (1.0, 1.0)),
                     C.complex (1.0, 0.0)))
      val () = Harness.check "scale"
                 (approxC eps
                    (C.scale (2.0, C.complex (1.5, ~3.0)), C.complex (3.0, ~6.0)))

      (* ---- mul matches polar-form multiplication ---- *)
      val () = Harness.section "Polar-form multiplication"
      val z1 = C.complex (1.0, 2.0)
      val z2 = C.complex (3.0, ~1.0)
      val prod = C.mul (z1, z2)
      val () = Harness.check "modulus multiplies"
                 (approx eps (C.abs prod, C.abs z1 * C.abs z2))
      val () = Harness.check "mul = fromPolar (moduli*, args+)"
                 (approxC eps
                    (prod,
                     C.fromPolar
                       {r = C.abs z1 * C.abs z2, theta = C.arg z1 + C.arg z2}))

      (* ---- Euler's identity ---- *)
      val () = Harness.section "Euler's identity"
      val () = Harness.check "exp(i*pi) = ~1"
                 (approxC eps (C.exp (C.complex (0.0, pi)), C.complex (~1.0, 0.0)))
      val () = Harness.check "exp(0) = 1"
                 (approxC eps (C.exp (C.complex (0.0, 0.0)), C.complex (1.0, 0.0)))

      (* ---- n-th roots of unity sum to zero ---- *)
      val () = Harness.section "Roots of unity sum to zero"
      val () = Harness.check "n = 3" (approxC eps (sumC (rootsOfUnity 3), C.complex (0.0, 0.0)))
      val () = Harness.check "n = 5" (approxC eps (sumC (rootsOfUnity 5), C.complex (0.0, 0.0)))
      val () = Harness.check "n = 8" (approxC eps (sumC (rootsOfUnity 8), C.complex (0.0, 0.0)))

      (* ---- sqrt / ln / exp inverse relationships ---- *)
      val () = Harness.section "sqrt, ln, exp"
      val zr = C.complex (3.0, 4.0)
      val () = Harness.check "sqrt(z)^2 = z"
                 (approxC eps (let val s = C.sqrt zr in C.mul (s, s) end, zr))
      val () = Harness.check "sqrt(~1) = i"
                 (approxC eps (C.sqrt (C.complex (~1.0, 0.0)), i))
      val zl = C.complex (0.5, 1.0)   (* im in principal range (~pi, pi] *)
      val () = Harness.check "ln(exp z) = z"
                 (approxC eps (C.ln (C.exp zl), zl))
      val () = Harness.check "exp(ln z) = z"
                 (approxC eps (C.exp (C.ln zr), zr))

      (* ---- pow ---- *)
      val () = Harness.section "pow"
      val () = Harness.check "z^2 = z*z"
                 (approxC eps
                    (C.pow (zr, C.complex (2.0, 0.0)), C.mul (zr, zr)))
      val () = Harness.check "z^1 = z"
                 (approxC eps (C.pow (zr, C.complex (1.0, 0.0)), zr))

      (* ---- conj / abs / arg identities ---- *)
      val () = Harness.section "conj, abs, arg identities"
      val () = Harness.check "conj(conj z) = z"
                 (approxC eps (C.conj (C.conj zr), zr))
      val () = Harness.check "abs(conj z) = abs z"
                 (approx eps (C.abs (C.conj zr), C.abs zr))
      val () = Harness.check "arg(conj z) = ~arg z"
                 (approx eps (C.arg (C.conj zr), ~ (C.arg zr)))
      val () = Harness.check "z * conj z = |z|^2 (real)"
                 (approxC eps
                    (C.mul (zr, C.conj zr),
                     C.complex (C.abs zr * C.abs zr, 0.0)))
      val () = Harness.check "abs = hypot"
                 (approx eps (C.abs zr, 5.0))
      val () = Harness.check "arg(1+i) = pi/4"
                 (approx eps (C.arg (C.complex (1.0, 1.0)), pi / 4.0))

      (* ---- polar round-trip ---- *)
      val () = Harness.section "Polar round-trip"
      val {r, theta} = C.toPolar zr
      val () = Harness.check "fromPolar o toPolar = id"
                 (approxC eps (C.fromPolar {r = r, theta = theta}, zr))

      (* ---- toString ---- *)
      val () = Harness.section "toString"
      val () = Harness.checkString "positive imaginary"
                 ("1.0 + 2.0i", C.toString (C.complex (1.0, 2.0)))
      val () = Harness.checkString "negative imaginary"
                 ("3.0 - 4.0i", C.toString (C.complex (3.0, ~4.0)))
    in
      ()
    end

  fun run () = (Harness.reset (); runAll (); Harness.run ())
end
