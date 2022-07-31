(* 7.1 *)

3.14, 2.71;;
3, true;;
3, "a", 3.14;;
(3, "a"), 3.14

(* 7.2 *)
let _ = match (3, 5) with a, b -> a + b
let add pair = match pair with a, b -> a + b
let _ = add (3, 5)
let add (a, b) = a + b

(* let f pair = match pair with
   (a, a) -> a + a ;; *)

let add2 a b = a + b;;

add2 3 5

(* 7.3 *)

(* 目的：ふたつの整数の組 pair を受け取りその要素の和を返す *)
(* add : int * int -> int *)
let add pair = match pair with a, b -> 0

(* テスト *)
let test1 = add (0, 0) = 0
let test2 = add (3, 5) = 8
let test3 = add (3, -5) = -2

(* 7.4 *)
