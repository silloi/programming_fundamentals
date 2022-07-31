(* 5.1 *)

let yugu_jikyu = 980

(* 5.2 *)

let _ = if true then 1 else 2
let _ = if false then 1 else 2
let _ = if 1 < 2 then 3 else 4
let _ = if 3 + 4 < 5 then 1 else 2

(* let _ = if 1 then 2 else 3 *)
(* let _ = if 3 + 4 > 5 then 1 else "2" *)

(* 5.3 *)

(* 時給（円） *)
let jikyu = 950

(* 基本給（円） *)
let kihonkyu = 100

(* 優遇時給（円） *)
let yugu_jikyu = 980

(* 優遇時給（円） *)
let yugu_jikyu = 980

(* 目的：働いた時間 x に応じたアルバイト代を計算する *)
(* kyuyo : int -> int *)
let kyuyo x =
  if x < 30 then kihonkyu + (jikyu * x) else kihonkyu + (x * yugu_jikyu)

(* 5.4 *)

let kyuyo x = kihonkyu + (x * if x < 30 then jikyu else yugu_jikyu)

(* 5.5 *)

(* 目的：受け取った実数 x の絶対値を計算する *)
(* abs_value : float -> float *)
let abs_value x = if x > 0.0 then x else -.x

(* テスト *)
let test1 = abs_value 2.8 = 2.8
let test2 = abs_value (-2.8) = 2.8
let test1 = abs_value 0.0 = 0.0

(* 5.6 *)

(* 目的：現在の気温 t から快適度を表す文字列を計算する *)
(* kion : int -> string *)
let kion t = if t < 15 then "普通" else if t <= 25 then "快適" else "普通"

(* テスト *)
let test1 = kion 7 = "普通"
let test2 = kion 15 = "快適"
let test3 = kion 20 = "快適"
let test4 = kion 25 = "快適"
let test5 = kion 28 = "普通"

(* 目的：現在の気温 t が 15 以上 25 以下かどうかをチェックする *)
(* kaiteki : int - > bool *)
let kaiteki t = 15 <= t && t <= 25

(* テスト *)
let test1 = kaiteki 7 = false
let test2 = kaiteki 15 = true
let test3 = kaiteki 20 = true
let test4 = kaiteki 25 = true
let test5 = kaiteki 28 = false

(* 目的：現在の気温 t から快適度を表す文字列を計算する *)
(* kion : int -> string *)
let kion t = if kaiteki t then "快適" else "普通"

(* テスト *)
let test1 = kion 7 = "普通"
let test2 = kion 15 = "快適"
let test3 = kion 20 = "快適"
let test4 = kion 25 = "快適"
let test5 = kion 28 = "普通"

(* 5.7 *)
let kyuyo x = kihonkyu + (x * if x < 30 then jikyu else yugu_jikyu)
