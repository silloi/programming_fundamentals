(* 整数のリスト lst でそれまでの数の合計からなるリストを返す *)
(* sum_list : int list -> int list *)
let sum_list lst =
  (* 整数のリスト lst でそれまでの数の合計からなるリストを返す *)
  (* ここで total0 はそれまでの数の和 *)
  (* hojo : int list -> int -> int list *)
  let rec hojo lst total0 =
    match lst with
    | [] -> []
    | first :: rest -> (total0 + first) :: hojo rest (total0 + first)
  in
  hojo lst 0

(* テスト *)
let test1 = sum_list [ 1 ] = [ 1 ]
let test2 = sum_list [ 1; 2 ] = [ 1; 3 ]
let test3 = sum_list [ 1; 2; 3 ] = [ 1; 3; 6 ]
