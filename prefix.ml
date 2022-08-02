(* 目的：受け取った lst の要素それぞれの先頭に n をくっつける *)
(* add_to_each n list : int -> int list list -> int list list *)
let rec add_to_each n lst =
  match lst with
  | [] -> []
  | first :: rest -> (n :: first) :: add_to_each n rest (* add_to_each n list *)

(* テスト *)
let test1 = add_to_each 1 [] = []
let test2 = add_to_each 1 [ [ 2 ] ] = [ [ 1; 2 ] ]
let test3 = add_to_each 1 [ [ 2 ]; [ 2; 3 ] ] = [ [ 1; 2 ]; [ 1; 2; 3 ] ]

let test4 =
  add_to_each 1 [ [ 2 ]; [ 2; 3 ]; [ 2; 3; 4 ] ]
  = [ [ 1; 2 ]; [ 1; 2; 3 ]; [ 1; 2; 3; 4 ] ]

(* 目的：受け取った list の接頭語のリストを求める *)
(* prefix : int list lit -> int list list *)
let rec prefix lst =
  match lst with
  | [] -> []
  | first :: rest -> [ first ] :: add_to_each first (prefix rest)

(* テスト *)
let test5 = prefix [] = []
let test6 = prefix [ 1 ] = [ [ 1 ] ]
let test7 = prefix [ 1; 2 ] = [ [ 1 ]; [ 1; 2 ] ]

let test8 =
  prefix [ 1; 2; 3; 4 ] = [ [ 1 ]; [ 1; 2 ]; [ 1; 2; 3 ]; [ 1; 2; 3; 4 ] ]
