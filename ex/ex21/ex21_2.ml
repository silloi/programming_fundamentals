(* 先頭の要素が素数である自然数のリスト lst の先頭を除いた残りから、先頭の要素で割り切れるものを取り除く。これを繰り返す *)
(* sieve : int list -> int list *)
let rec sieve lst =
  match lst with
  | [] -> []
  | first :: rest ->
      let indivisible lst = List.filter (fun x -> x mod first > 0) lst in
      print_int (List.length lst);
      print_newline ();
      first :: sieve (indivisible rest)

(* テスト *)
let test1 = sieve [ 2 ] = [ 2 ]
let test2 = sieve [ 2; 3 ] = [ 2; 3 ]
let test3 = sieve [ 2; 3; 4 ] = [ 2; 3 ]
let test4 = sieve [ 2; 3; 4; 5; 6; 7; 8; 9 ] = [ 2; 3; 5; 7 ]
