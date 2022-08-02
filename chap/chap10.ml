(* 10.1 *)

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

(* 10.2 *)

(* 目的：受け取った lst の中の最小値を返す *)
(* minimum : int list -> int *)
let rec minimum lst =
  match lst with
  | [] -> max_int
  | first :: rest -> if first <= minimum rest then first else minimum rest

(* テスト *)
let test1 = minimum [ 3 ] = 3
let test2 = minimum [ 1; 2 ] = 1
let test3 = minimum [ 3; 2 ] = 2
let test4 = minimum [ 3; 2; 6; 4; 1; 8 ] = 1

(* 10.3 *)
;;

let x = 2 in
x + x

(* let _ = x + 3;; *)

let x = 3;;

let x = 2 in
x + x
;;

x + 3;;

let x = 3 in
let y = 4 in
x + y

(* 目的：受け取った lst の中の最小値を返す *)
(* minimum : int list -> int *)
let rec minimum lst =
  match lst with
  | [] -> max_int
  | first :: rest ->
      let min_rest = minimum rest in
      if first <= min_rest then first else min_rest

(* 10.4 *)

(* 学生ひとり分のデータ（名前、点数、成績）を表す型 *)
type gakusei_t = {
  namae : string; (* 名前 *)
  tensuu : int; (* 点数 *)
  seiseki : string; (* 成績 *)
}

(* 目的：学生リスト lst のうち各成績の人数を集計する *)
(* shukei : gakusei_t list -> int * int * int * int *)
let rec shukei lst =
  match lst with
  | [] -> (0, 0, 0, 0)
  | { namae = n; tensuu = t; seiseki = s } :: rest ->
      let a, b, c, d = shukei rest in
      if s = "A" then (a + 1, b, c, d)
      else if s = "B" then (a, b + 1, c, d)
      else if s = "C" then (a, b, c + 1, d)
      else (a, b, c, d + 1)

(* 10.5 *)

(* 目的：lst1 と lst2 を受け取りそれらを結合したリストを返す *)
(* append : 'a list -> 'a list -> 'a list *)
let rec append lst1 lst2 =
  match lst1 with [] -> lst2 | first :: rest -> first :: append rest lst2

(* テスト *)
let test1 = append [] [] = []
let test2 = append [] [ 1; 2 ] = [ 1; 2 ]
let test3 = append [ 1; 2 ] [] = [ 1; 2 ]
let test4 = append [ 1; 2 ] [ 3; 4 ] = [ 1; 2; 3; 4 ]

let test5 =
  append [ "a"; "b"; "c"; "d"; "e" ] [ "f"; "g" ]
  = [ "a"; "b"; "c"; "d"; "e"; "f"; "g" ]

(* 10. 6 *)

(* 目的：昇順に並んでいるリスト lst1 と lst2 をマージする *)
(* merge : 'a list -> 'a list -> 'a list *)
let rec merge lst1 lst2 =
  match (lst1, lst2) with
  | [], [] -> []
  | [], first2 :: rest2 -> lst2
  | first1 :: rest1, [] -> lst1
  | first1 :: rest1, first2 :: rest2 ->
      if first1 < first2 then first1 :: merge rest1 lst2
      else first2 :: merge lst1 rest2

(* テスト *)
let test1 = merge [] [] = []
let test2 = append [] [ 1; 2 ] = [ 1; 2 ]
let test3 = append [ 1; 2 ] [] = [ 1; 2 ]
let test4 = append [ 1; 3 ] [ 2; 4 ] = [ 1; 2; 3; 4 ]
let test5 = append [ 2; 4 ] [ 1; 3 ] = [ 1; 2; 3; 4 ]
let test4 = append [ 1; 4 ] [ 1; 3 ] = [ 1; 1; 3; 4 ]
