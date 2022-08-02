(* 目的：昇順のリスト lst に n を挿入したリストを返す*)
(* insert int list -> n -> int list *)
let rec insert lst n =
  match lst with
  | [] -> [ n ]
  | first :: rest ->
      if first < n then first :: insert rest n else n :: first :: rest

(* テスト *)
let test1 = insert [] 1 = [ 1 ]
let test2 = insert [ 1 ] 2 = [ 1; 2 ]
let test3 = insert [ 2 ] 1 = [ 1; 2 ]
let test4 = insert [ 1; 3; 5 ] 3 = [ 1; 3; 3; 5 ]
let test5 = insert [ 1; 3; 5 ] 4 = [ 1; 3; 4; 5 ]