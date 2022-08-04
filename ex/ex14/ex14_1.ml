(* 目的：整数 n が偶数かどうか調べる *)
(* is_even : int -> bool *)
let is_even n = n mod 2 = 0

(* テスト *)
let test1 = is_even 0 = true
let test2 = is_even 1 = false
let test3 = is_even 2 = true
let test4 = is_even (-2) = true

(* 目的：整数のリスト lst を受け取ったら偶数の要素のみを含むリストを返す *)
(* even : int list -> int list *)
let even lst = List.filter is_even lst

(* テスト *)
let test1 = even [] = []
let test2 = even [ 2 ] = [ 2 ]
let test3 = even [ 1; 3 ] = []
let test4 = even [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] = [ 2; 4; 6; 8; 10 ]
