(* 4.1 *)

let jikyu = 950
let kihonkyu = 100;;

(25 * jikyu) + (28 * jikyu) + (31 * jikyu);;
kihonkyu + (25 * jikyu) + (kihonkyu + (28 * jikyu)) + (kihonkyu + (31 * jikyu))

(* 4.2 *)

let f x = 3 * x;;

f 4

let g x y = (x * x) + (y * y) - 4;;

g 3 2

let kyuyo x = kihonkyu + (x * jikyu);;

kyuyo 25 + kyuyo 28 + kyuyo 31

(* 4.3 *)

let f x = 3 * x
let pi = 3.1415
let g x y = (x * x) + (y * y) - 4
let f x = 3 * x

(* 4.4 *)

let f x = 3 * x

(* f 2.5 ;; *)

let f2 x = 3.0 *. x;;

f2 2.5

(* 4.5 *)

let jikyu = 950
let kihonkyu = 100
let kyuyo x = kihonkyu + (x * jikyu);;

kyuyo 25 + kyuyo 28 + kyuyo 31

(* 4.6 *)

(* 目的：働いた時間 x に応じたアルバイト代を計算する *)
(* kyuyo : int -> int *)
let kyuyo x = 0

(* テスト *)
let test1 = kyuyo 25 = 23850
let test2 = kyuyo 28 = 26700
let test3 = kyuyo 31 = 29550

(* 時給（円） *)
let jikyu = 950

(* 基本給（円） *)
let kihonkyu = 100

(* 目的：働いた時間 x に応じたアルバイト代を計算する *)
(* kyuyo : int -> int *)
let kyuyo x = kihonkyu + (jikyu * x)

(* テスト *)
let test1 = kyuyo 25 = 23850
let test2 = kyuyo 28 = 26700
let test3 = kyuyo 31 = 29550
