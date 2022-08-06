(* 目的：自然数 m と n の最大公約数を求める *)
(* gcd : int -> int -> int *)
let rec gcd m n =
  let m, n = if m >= n then (m, n) else (n, m) in
  if n = 0 then m else gcd n (m mod n)

(* テスト *)
let test1 = gcd 2 1 = 1
let test2 = gcd 2 4 = 2
let test3 = gcd 4 12 = 4
let test4 = gcd 36 27 = 9
