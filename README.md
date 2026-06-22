# sml-complex

Complex numbers over `real` in pure Standard ML. A small, dependency-free
`structure Complex :> COMPLEX` with the field operations plus the principal
branches of `exp`, `ln`, `sqrt`, `pow`, the trigonometric / hyperbolic
functions and their inverses, `nthRoots`, and conversions to and from polar
form. Builds and tests with both **MLton** and **Poly/ML**.

The representation is abstract: build values with `complex` or `fromPolar`,
inspect them with `re`/`im` or `toPolar`. Everything is pure and deterministic
(no FFI, threads, or clock), so the two compilers produce byte-identical test
output.

## Usage

```sml
val z = Complex.complex (3.0, 4.0)     (* 3 + 4i              *)
val m = Complex.abs z                  (* 5.0  (modulus)      *)
val a = Complex.arg z                  (* atan2 (4, 3)        *)

(* Euler's identity: exp(i*pi) = ~1 (+ rounding) *)
val e = Complex.exp (Complex.complex (0.0, Math.pi))
val () = print (Complex.toString e ^ "\n")   (* -1.000000 + 0.000000i *)

(* z^w via exp (w * ln z) *)
val w = Complex.pow (z, Complex.complex (2.0, 0.0))   (* z^2 = z*z *)
```

## API (`signature COMPLEX`)

```sml
type t
val complex   : real * real -> t
val re        : t -> real
val im        : t -> real
val conj      : t -> t
val abs       : t -> real                       (* modulus |z|        *)
val arg       : t -> real                       (* argument, (~pi,pi] *)
val add       : t * t -> t
val sub       : t * t -> t
val mul       : t * t -> t
val divide    : t * t -> t
val scale     : real * t -> t
val exp       : t -> t
val ln        : t -> t
val sqrt      : t -> t                           (* principal root     *)
val pow       : t * t -> t
val sin       : t -> t
val cos       : t -> t
val tan       : t -> t
val sinh      : t -> t
val cosh      : t -> t
val tanh      : t -> t
val asin      : t -> t                           (* principal branches *)
val acos      : t -> t
val atan      : t -> t
val asinh     : t -> t
val acosh     : t -> t
val atanh     : t -> t
val nthRoots  : t * int -> t list                (* n distinct n-th roots *)
val fromPolar : {r : real, theta : real} -> t
val toPolar   : t -> {r : real, theta : real}
val toString  : t -> string
```

The transcendentals use the standard principal-branch definitions:
`exp (a + bi) = e^a (cos b + i sin b)`, `ln z = ln |z| + i arg z`, principal
`sqrt`, and `pow (z, w) = exp (w * ln z)`. The trig and hyperbolic functions
follow the usual identities (e.g. `sin (a + bi) = sin a cosh b + i cos a sinh b`,
`sinh (a + bi) = sinh a cos b + i cosh a sin b`); the inverses use the principal
branches (`asin z = -i ln (iz + sqrt (1 - z^2))`, `acos z = pi/2 - asin z`,
`atan z = (i/2)(ln (1 - iz) - ln (1 + iz))`, and the analogous inverse hyperbolic
forms). `nthRoots (z, n)` returns the `n` solutions of `w^n = z`.

## Building and testing

```
make test       # build + run the suite under MLton
make test-poly  # build + run the suite under Poly/ML
make all-tests  # both compilers
make example    # build + run examples/demo.sml
make clean      # remove bin/
```

Both compilers run the same strict-TDD suite (`test/test.sml`), which uses an
explicit epsilon tolerance (`approx`/`approxC`, `eps = 1e-9`) since the library
is built on floating-point `real`. Highlights:

- **Euler's identity:** `exp(i*pi) ~= ~1`.
- **Polar multiplication:** `mul` agrees with multiplying moduli and adding
  arguments.
- **Roots of unity:** the `n` values `exp(2*pi*i*k/n)` sum to `~= 0`
  (`n = 3, 5, 8`).
- **Inverses/identities:** `sqrt(z)^2 ~= z`, `ln(exp z) ~= z`, and the
  `conj`/`abs`/`arg` identities.
- **Trig/hyperbolic:** the Pythagorean identity `sin^2 z + cos^2 z = 1`,
  `cosh^2 z - sinh^2 z = 1`, canonical values (`sin i = i sinh 1`,
  `cos i = cosh 1`), and round-trips `asin(sin z) ~= z`, `atanh(tanh z) ~= z`.
- **Roots:** every value of `nthRoots (z, n)` raised to the `n`-th power
  returns `z`, and the `n` roots sum to `~= 0`.

### Poly/ML note

CI builds Poly/ML 5.9.1 from source rather than using the Ubuntu package
(Poly/ML 5.7.1), whose X86 code generator crashes (`asGenReg raised while
compiling`) on heavy real-arithmetic code. See `.github/workflows/ci.yml`.

## License

MIT
