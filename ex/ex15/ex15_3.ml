(* 先頭の要素が素数である自然数のリスト lst の先頭を除いた残りから、先頭の要素で割り切れるものを取り除く。これを繰り返す *)
(* sieve : int list -> int list *)
let rec sieve lst =
  match lst with
  | [] -> []
  | first :: rest ->
      let indivisible lst = List.filter (fun x -> x mod first > 0) lst in
      first :: sieve (indivisible rest)

(* テスト *)
let test1 = sieve [ 2 ] = [ 2 ]
let test2 = sieve [ 2; 3 ] = [ 2; 3 ]
let test3 = sieve [ 2; 3; 4 ] = [ 2; 3 ]
let test4 = sieve [ 2; 3; 4; 5; 6; 7; 8; 9 ] = [ 2; 3; 5; 7 ]

(* 目的：自然数 n 以下の素数のリストを求める *)
(* prime : int -> int list *)
let rec prime n =
  (* 目的：2 から n までのリストを作る *)
  (* enumerate_from_2 : int -> int list *)
  let rec enumerate_from_m m n =
    if m > n then [] else m :: enumerate_from_m (m + 1) n
  in
  let enum_from_2 = enumerate_from_m 2 n in
  sieve enum_from_2

(* テスト *)
let test1 = prime 2 = [ 2 ]
let test2 = prime 3 = [ 2; 3 ]
let test3 = prime 4 = [ 2; 3 ]
let test4 = prime 9 = [ 2; 3; 5; 7 ]
