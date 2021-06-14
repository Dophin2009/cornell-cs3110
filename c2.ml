(* values *)
let _ = 7 * (1 + 2 + 3) (* 42:int *)

let _ = "CS " ^ string_of_int 3110 (* "CS 3110":string *)

(* operators *)
let _ = 42 * 10

let _ = 3.14 +. 2.

let rec pow a b = if b = 0 then 1. else a *. pow a (b - 1)

let _ = pow 4.2 7

(* equality *)
let _ = 42 = 42

let _ = "hi" = "hi" (* true *)

let _ = "hi" == "hi" (* false *)

(* if *)
let _ = if 2 > 1 then 42 else 7

(* double fun *)
let double x = x * 2

(* more fun *)
let cubef x = x *. x *. x

let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

let circlea r = Float.pi *. pow r 2

(* rms *)
let rms x y = sqrt (((x *. x) +. (y *. y)) /. 2.)

(* date fun *)
let isvalid d m =
  if d < 1 then false
  else
    match m with
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> d <= 31
    | 2 -> d <= 28
    | 4 | 6 | 9 | 11 -> d <= 30
    | _ -> false

(* fib *)
let rec fib n =
  if n <= 0 then 0 else if n = 1 then 1 else fib (n - 1) + fib (n - 2)

(* fib fast *)
let fib_fast n =
  let rec fib_fast_h c n2 n1 =
    match c with 1 -> n1 | _ -> fib_fast_h (c - 1) n1 (n2 + n1)
  in
  fib_fast_h n 0 1

(* poly types *)
let f x = if x then x else x (* bool -> bool *)

let g x y = if y then x else x (* y -> 'a -> 'a *)

let h x y z = if x then y else z (* bool -> 'a -> 'a -> 'a *)

(* divide *)
let divide n d = n /. d

(* associativity *)
let add x y = x + y

let _ = add 5 1 (* int *)

let add5 = add 5 (* function *)

let _ = (add 5) 1 (* int *)

(* average *)
let ( +/. ) a b = (a +. b) /. 2.
