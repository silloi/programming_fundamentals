(* 目的：リスト lst1 と lst2 の長さが同じかどうか判定する *)
(* equal_length : 'a list -> 'b list -> bool *)
let rec equal_length lst1 lst2 =
  match (lst1, lst2) with
  | [], [] -> true
  | [], first2 :: rest2 -> false
  | first1 :: rest1, [] -> false
  | first1 :: rest1, first2 :: rest2 ->
      if equal_length rest1 rest2 then true else false

(* テスト *)
let test1 = equal_length [] [] = true
let test2 = equal_length [] [ 1; 2 ] = false
let test3 = equal_length [ 1; 2 ] [] = false
let test4 = equal_length [ 1; 3 ] [ 2; 4 ] = true
let test5 = equal_length [ 2; 4 ] [ 1; 3 ] = true
let test6 = equal_length [ 1; 3; 5 ] [ 2; 4 ] = false
