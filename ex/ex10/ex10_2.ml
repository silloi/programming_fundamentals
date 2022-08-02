(* 目的：昇順のリスト lst に n を挿入したリストを返す*)
(* insert int list -> n -> int list *)
let rec insert lst n =
  match lst with
  | [] -> [ n ]
  | first :: rest ->
      if first < n then first :: insert rest n else n :: first :: rest

(* 目的：整数のリスト lst を昇順に整列して返す *)
(* ins_sort : int list -> int list *)
let rec ins_sort lst =
  match lst with [] -> [] | first :: rest -> insert (ins_sort rest) first

(* テスト *)
let test1 = ins_sort [] = []
let test2 = ins_sort [ 1 ] = [ 1 ]
let test3 = ins_sort [ 1; 2 ] = [ 1; 2 ]
let test4 = ins_sort [ 2; 1 ] = [ 1; 2 ]
let test1 = ins_sort [ 5; 3; 8; 1; 7; 4 ] = [ 1; 3; 4; 5; 7; 8 ]
