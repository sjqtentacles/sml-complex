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

      (* ---- trigonometric functions ---- *)
      val () = Harness.section "Trigonometric functions"
      (* Real arguments agree with Math.{sin,cos,tan}. *)
      val () = Harness.check "sin(real) = Math.sin"
                 (approxC eps
                    (C.sin (C.complex (0.7, 0.0)),
                     C.complex (Math.sin 0.7, 0.0)))
      val () = Harness.check "cos(real) = Math.cos"
                 (approxC eps
                    (C.cos (C.complex (0.7, 0.0)),
                     C.complex (Math.cos 0.7, 0.0)))
      (* sin i = i sinh 1, cos i = cosh 1 (canonical values). *)
      val () = Harness.check "sin(i) = i*sinh(1)"
                 (approxC eps
                    (C.sin i, C.complex (0.0, 1.1752011936438014)))
      val () = Harness.check "cos(i) = cosh(1)"
                 (approxC eps
                    (C.cos i, C.complex (1.5430806348152437, 0.0)))
      val () = Harness.check "tan = sin/cos"
                 (approxC eps
                    (C.tan zl, C.divide (C.sin zl, C.cos zl)))
      (* Pythagorean identity sin^2 z + cos^2 z = 1. *)
      val () = Harness.check "sin^2 + cos^2 = 1"
                 (approxC eps
                    (C.add (C.mul (C.sin zl, C.sin zl),
                            C.mul (C.cos zl, C.cos zl)),
                     C.complex (1.0, 0.0)))

      (* ---- hyperbolic functions ---- *)
      val () = Harness.section "Hyperbolic functions"
      val () = Harness.check "sinh(i) = i*sin(1)"
                 (approxC eps
                    (C.sinh i, C.complex (0.0, Math.sin 1.0)))
      val () = Harness.check "cosh(i) = cos(1)"
                 (approxC eps
                    (C.cosh i, C.complex (Math.cos 1.0, 0.0)))
      val () = Harness.check "cosh^2 - sinh^2 = 1"
                 (approxC eps
                    (C.sub (C.mul (C.cosh zl, C.cosh zl),
                            C.mul (C.sinh zl, C.sinh zl)),
                     C.complex (1.0, 0.0)))
      val () = Harness.check "tanh = sinh/cosh"
                 (approxC eps
                    (C.tanh zl, C.divide (C.sinh zl, C.cosh zl)))
      (* sin(iz) = i sinh(z). *)
      val () = Harness.check "sin(iz) = i*sinh(z)"
                 (approxC eps
                    (C.sin (C.mul (i, zl)), C.mul (i, C.sinh zl)))

      (* ---- inverse trigonometric functions ---- *)
      val () = Harness.section "Inverse trigonometric functions"
      val () = Harness.check "atan(1) = pi/4"
                 (approxC eps
                    (C.atan (C.complex (1.0, 0.0)), C.complex (pi / 4.0, 0.0)))
      val () = Harness.check "asin(sin z) = z"
                 (approxC eps (C.asin (C.sin zl), zl))
      val () = Harness.check "acos(cos z) = z"
                 (approxC eps (C.acos (C.cos zl), zl))
      val () = Harness.check "atan(tan z) = z"
                 (approxC eps (C.atan (C.tan zl), zl))
      (* asin of a real > 1 has a known complex principal value. *)
      val () = Harness.check "asin(2) principal value"
                 (approxC eps
                    (C.asin (C.complex (2.0, 0.0)),
                     C.complex (1.5707963267948966, ~1.3169578969248166)))

      (* ---- inverse hyperbolic functions ---- *)
      val () = Harness.section "Inverse hyperbolic functions"
      val () = Harness.check "asinh(sinh z) = z"
                 (approxC eps (C.asinh (C.sinh zl), zl))
      val () = Harness.check "acosh(cosh z) = z"
                 (approxC eps (C.acosh (C.cosh zl), zl))
      val () = Harness.check "atanh(tanh z) = z"
                 (approxC eps (C.atanh (C.tanh zl), zl))
      val () = Harness.check "atanh(0.5) = 0.5*ln 3"
                 (approxC eps
                    (C.atanh (C.complex (0.5, 0.0)),
                     C.complex (0.5 * Math.ln 3.0, 0.0)))

      (* ---- nthRoots ---- *)
      val () = Harness.section "nthRoots"
      (* Each n-th root, raised to the n-th power, returns z. *)
      val cubeRoots = C.nthRoots (zr, 3)
      val () = Harness.checkInt "nthRoots count" (3, List.length cubeRoots)
      val () = Harness.check "each cube root ^3 = z"
                 (List.all
                    (fn w => approxC eps (C.pow (w, C.complex (3.0, 0.0)), zr))
                    cubeRoots)
      (* The n n-th roots sum to zero for n >= 2. *)
      val () = Harness.check "n-th roots sum to 0"
                 (approxC eps (sumC (C.nthRoots (C.complex (8.0, 0.0), 5)),
                               C.complex (0.0, 0.0)))
      (* Cube roots of 8 have modulus 2; principal root is 2. *)
      val () = Harness.check "principal cube root of 8 = 2"
                 (approxC eps
                    (hd (C.nthRoots (C.complex (8.0, 0.0), 3)),
                     C.complex (2.0, 0.0)))
      val () = Harness.checkRaises "nthRoots 0 raises"
                 (fn () => C.nthRoots (zr, 0))

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
                 ("1.000000 + 2.000000i", C.toString (C.complex (1.0, 2.0)))
      val () = Harness.checkString "negative imaginary"
                 ("3.000000 - 4.000000i", C.toString (C.complex (3.0, ~4.0)))
    in
      ()
    end

  fun run () = (Harness.reset (); runAll (); Harness.run ())
end
