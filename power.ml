(* 目的：自然数 m と n を受け取ったら m の n 上を求める *)
(* power : int -> int -> int *)
let rec power m n = if n = 0 then 1 else m * power m (n - 1)

(* テスト *)
let test1 = power 3 0 = 1
let test1 = power 3 1 = 3
let test1 = power 3 2 = 9
let test1 = power 3 3 = 27
