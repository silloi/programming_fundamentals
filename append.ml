(* 目的：first を rest_result の先頭に加える *)
(* cons : 'a -> 'a list -> 'a list *)
let cons first rest_result = first :: rest_result

(* 目的：lst1 と lst2 を受け取りそれらを結合したリストを返す *)
(* append : 'a list -> 'a list -> 'a list *)
let rec append lst1 lst2 =
  match lst1 with [] -> lst2 | first :: rest -> first :: append rest lst2

let append lst1 lst2 = List.fold_right cons lst1 lst2

let append lst1 lst2 =
  List.fold_right (fun first rest_result -> first :: rest_result) lst1 lst2

(* テスト *)
let test1 = append [] [] = []
let test2 = append [] [ 1; 2 ] = [ 1; 2 ]
let test3 = append [ 1; 2 ] [] = [ 1; 2 ]
let test4 = append [ 1; 2 ] [ 3; 4 ] = [ 1; 2; 3; 4 ]

let test5 =
  append [ "a"; "b"; "c"; "d"; "e" ] [ "f"; "g" ]
  = [ "a"; "b"; "c"; "d"; "e"; "f"; "g" ]
