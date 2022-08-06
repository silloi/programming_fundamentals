(* init を初期値として、リスト lst の要素を左から順に f を施し込む *)
(* fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
let rec fold_left f init lst =
  match lst with [] -> init | first :: rest -> fold_left f (f init first) rest

(* テスト *)
let test1 = fold_left (fun x init -> init) 0 [ 1; 2; 3 ] = 3
let test2 = fold_left ( + ) 0 [ 1; 2; 3 ] = 6

let test3 =
  fold_left (fun rest_result first -> first :: rest_result) [] [ 3; 2; 1 ]
  = [ 1; 2; 3 ]

let test4 = fold_left ( mod ) 70 [ 8; 3 ] = 0
