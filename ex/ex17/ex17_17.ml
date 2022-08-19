(* 目的：受け取った first と rest の中の最小値のうち小さい方を返す *)
(* minimum : int -> int list -> int *)
let rec minimum first rest0 =
  match rest0 with
  | [] -> first
  | second :: rest ->
      (* let min_rest = minimum rest in *)
      if first <= second then minimum first rest else minimum second rest

(* テスト *)
let test1 = minimum 3 [] = 3
let test2 = minimum 1 [ 2 ] = 1
let test3 = minimum 3 [ 2 ] = 2
let test4 = minimum 3 [ 2; 6; 4; 1; 8 ] = 1
