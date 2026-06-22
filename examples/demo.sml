(* demo.sml

   A tiny tour of the `Complex` library: Euler's identity and the fifth roots
   of unity. Build and run with `make example`. *)

structure C = Complex

val pi = Math.pi

fun line s = print (s ^ "\n")

(* The n complex n-th roots of unity: exp(2*pi*i*k/n), k = 0 .. n-1. *)
fun rootsOfUnity n =
  List.tabulate
    (n, fn k => C.exp (C.complex (0.0, 2.0 * pi * real k / real n)))

val () = line "Euler's identity:"
val () = line ("  exp(i*pi) = " ^ C.toString (C.exp (C.complex (0.0, pi))))

val () = line ""
val () = line "Fifth roots of unity:"
val () =
  List.app
    (fn z => line ("  " ^ C.toString z))
    (rootsOfUnity 5)

val () = line ""
val () = line ("Their sum = "
               ^ C.toString
                   (List.foldl C.add (C.complex (0.0, 0.0)) (rootsOfUnity 5)))

val () = line ""
val () = line "Trigonometry of i:"
val i = C.complex (0.0, 1.0)
val () = line ("  sin(i) = " ^ C.toString (C.sin i))
val () = line ("  cos(i) = " ^ C.toString (C.cos i))
val () = line ("  atan(1) = " ^ C.toString (C.atan (C.complex (1.0, 0.0))))

val () = line ""
val () = line "Cube roots of 8:"
val () = List.app (fn z => line ("  " ^ C.toString z)) (C.nthRoots (C.complex (8.0, 0.0), 3))
