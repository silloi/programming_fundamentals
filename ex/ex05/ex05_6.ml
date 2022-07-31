(* 目的：二次方程式 ax^2 + bx + c = 0 の係数 a, b, c（いずれも実数）を受け取ったら判別式の値を返す*)
(* hanbetsushiki : float -> float -> float -> float *)
let hanbetsushiki a b c = (b ** 2.0) -. (4. *. a *. c)

(* 目的：二次方程式 ax^2 + bx + c = 0 の係数 a, b, c（いずれも実数）が虚数解を持つかどうかを返す *)
(* kyosuukai : float -> float -> float -> bool *)
let kyosuukai a b c = hanbetsushiki a b c < 0.0

(* テスト *)
let test1 = kyosuukai 1. 2. 0. = false
let test2 = kyosuukai 1. 2. 1. = false
let test3 = kyosuukai 1. 2. 2. = true
