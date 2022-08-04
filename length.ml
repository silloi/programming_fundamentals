(* 目的：first は無視して rest_result に 1 を加える *)
let add_one first rest_result = 1 + rest_result

(* 目的：受け取ったリスト lst の長さを求める *)
(* length : 'a list -> int *)
let rec length lst = match lst with [] -> 0 | first :: rest -> 1 + length rest
let length lst = List.fold_right add_one lst 0

let length lst =
  List.fold_right (fun first rest_result -> 1 + rest_result) lst 0
