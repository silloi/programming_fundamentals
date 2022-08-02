(* 11.1 *)

(* 自然数は
     - 0       0、あるいは
     - n + 1   ひとつ小さい自然数 n に 1 を加えたもの
               （n が自己参照のケース）
   という形 *)

(* 11.2 *)

(* 目的：自然数 n の階乗を求める *)
(* fac : int -> int *)
let rec fac n = if n = 0 then 1 else n * fac (n - 1)

(* テスト *)
let test1 = fac 0 = 1
let test2 = fac 1 = 1
let test3 = fac 2 = 2
let test4 = fac 3 = 6
let test5 = fac 4 = 24
let test6 = fac 10 = 3628800

(* 11.3 *)

(* 目的：自然数 m と n を受け取ったら m の n 上を求める *)
(* power : int -> int -> int *)
let rec power m n = if n = 0 then 1 else m * power m (n - 1)

(* テスト *)
let test1 = power 3 0 = 1
let test1 = power 3 1 = 3
let test1 = power 3 2 = 9
let test1 = power 3 3 = 27
