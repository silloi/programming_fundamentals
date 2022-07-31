(* 目的：二次方程式 ax^2 + bx + c = 0 の係数 a, b, c（いずれも実数）を受け取ったら判別式の値を返す*)
(* hanbetsushiki : float -> float -> float -> float *)
let hanbetsushiki a b c = (b ** 2.0) -. (4. *. a *. c)

(* 目的：二次方程式 ax^2 + bx + c = 0 の係数 a, b, c（いずれも実数）を受け取ったら解の個数を返す *)
(* kai_no_kosuu : float -> float -> float -> int *)
let kai_no_kosuu a b c =
  if hanbetsushiki a b c > 0. then 2
  else if hanbetsushiki a b c = 0. then 1
  else 0

(* テスト *)
let test1 = kai_no_kosuu 1. 2. 0. = 2
let test2 = kai_no_kosuu 1. 2. 1. = 1
let test3 = kai_no_kosuu 1. 2. 2. = 0
