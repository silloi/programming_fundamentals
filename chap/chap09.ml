(* 9.1 *)

(* "日曜日" :: "月曜日" :: "火曜日" :: "水曜日" :: "木曜日" :: "金曜日" :: "土曜日" :: [] ;; *)

(* "日曜日" :: ("月曜日" :: ("火曜日" ::( "水曜日" :: ("木曜日" :: ("金曜日" :: ("土曜日" :: [])))))) ;; *)

(* 9.2 *)

(* 1 :: 2 :: 3 :: [] ;; *)

[ 1; 2; 3 ];;
1 :: [ 2; 3 ];;
1 :: 2 :: [ 3 ];;
[ true; false; true ];;
[]

let lst = [ 1; 2; 3; 4; 5 ];;

[ [ 1; 3 ]; [ 2 ]; [ 4; 1; 3; 5 ]; []; [ 2; 5 ] ]

(* 9.3 *)

let _ = match [] with [] -> 0 | first :: rest -> first
let _ = match [ 1; 2; 3 ] with [] -> 0 | first :: rest -> first

(* let _ = match [1; 2; 3] with first :: rest -> first *)

(* let _ = match [] with first :: rest -> first *)

(* let _ = match [] with [] -> [] *)

(* 9.4 *)

(* int list は
     - []              空リスト、あるいは
     - first :: rest   最初の要素が first で残りのリストが rest
   という形 *)

(* 目的：受け取ったリスト lst に 0 が含まれているかを調べる *)
(* contain_zero : int list -> bool *)
let rec contain_zero lst =
  match lst with
  | [] -> false
  | first :: rest -> if first = 0 then true else contain_zero rest

(* テスト *)
let test1 = contain_zero [] = false
let test2 = contain_zero [ 0; 2 ] = true
let test3 = contain_zero [ 1; 2 ] = false
let test4 = contain_zero [ 1; 2; 3; 0; 5; 6; 7 ] = true
let test5 = contain_zero [ 1; 2; 3; 4; 5; 6; 7 ] = false

(* 9.5 *)

(* int list は
     - []              空リスト、あるいは
     - first :: rest   最初の要素が first で残りのリストが rest
                      （rest が自己参照のケース）
   という形 *)

(* 目的：受け取ったリスト lst の各要素の和を求める *)
(* sum : int list -> int *)
let rec sum lst = match lst with [] -> 0 | first :: rest -> first + sum rest

(* テスト *)
let test1 = sum [] = 0
let test2 = sum [ 2 ] = 2
let test3 = sum [ 1; 3 ] = 4
let test4 = sum [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] = 55

(* 9.6 *)

(* 学生ひとり分のデータ（名前、点数、成績）を表す型 *)
type gakusei_t = {
  namae : string; (* 名前 *)
  tensuu : int; (* 点数 *)
  seiseki : string; (* 成績 *)
}

(* gakusei_t list は
     - []              空リスト、あるいは
     - first :: rest   最初の要素が first で残りのリストが rest
                       （first は gakusei_t 型、
                         rest が自己参照のケース）
   という形 *)

(* gakusei_t list 型のデータの例 *)
let lst1 = []
let lst2 = [ { namae = "asai"; tensuu = 70; seiseki = "B" } ]

let lst3 =
  [
    { namae = "asai"; tensuu = 70; seiseki = "B" };
    { namae = "kaneko"; tensuu = 85; seiseki = "A" };
  ]

let lst4 =
  [
    { namae = "yoshida"; tensuu = 80; seiseki = "A" };
    { namae = "asai"; tensuu = 70; seiseki = "B" };
    { namae = "kaneko"; tensuu = 85; seiseki = "A" };
  ]

(* 目的：学生リスト lst のうち成績が A の人の数を返す *)
(* count_A : gakusei_t list -> int *)
let rec count_A lst =
  match lst with
  | [] -> 0
  | { namae = n; tensuu = t; seiseki = s } :: rest ->
      if s = "A" then 1 + count_A rest else count_A rest

(* テスト *)
let test1 = count_A lst1 = 0
let test2 = count_A lst2 = 0
let test3 = count_A lst3 = 1
let test4 = count_A lst4 = 2
