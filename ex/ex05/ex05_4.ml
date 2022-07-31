(* 目的：二次方程式 ax^2 + bx + c = 0 の係数 a, b, c（いずれも実数）を受け取ったら判別式の値を返す*)
(* hanbetsushiki : float -> float -> float -> float*)
let hanbetsushiki a b c = (b ** 2.0) -. (4. *. a *. c)

(* テスト *)
let test1 = hanbetsushiki 1. 2. 0. = 4.
let test2 = hanbetsushiki 1. 2. 1. = 0.
let test3 = hanbetsushiki 1. 2. 2. = -4.
