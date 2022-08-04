(* 13.1 *)

(* 学生ひとり分のデータ（名前、点数、成績）を表す型 *)
type gakusei_t = {
  namae : string; (* 名前 *)
  tensuu : int; (* 点数 *)
  seiseki : string; (* 成績 *)
}

(* gakusei_t list 型のデータの例 *)
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

(* 目的：学生リスト lst のうち成績が B の人の数を返す *)
(* count_B : gakusei_t list -> int *)
let rec count_B lst =
  match lst with
  | [] -> 0
  | { namae = n; tensuu = t; seiseki = s } :: rest ->
      if s = "B" then 1 + count_B rest else count_B rest

(* 目的：学生リスト lst のうち成績が seiseki0 の人の数を返す *)
(* count : gakusei_t list -> string -> int *)
let rec count lst seiseki0 =
  match lst with
  | [] -> 0
  | { namae = n; tensuu = t; seiseki = s } :: rest ->
      if s = seiseki0 then 1 + count rest seiseki0 else count rest seiseki0
;;

count lst4 "A";;
count lst4 "B"

(* 目的：学生リスト lst のうち成績が A の人の数を返す *)
(* count_A : gakusei_t list -> int *)
let count_A lst = count lst "A"

(* 目的：学生リスト lst のうち成績が B の人の数を返す *)
(* count_B : gakusei_t list -> int *)
let count_B lst = count lst "B"

(* 13.2 *)

(* 目的：実数のリスト lst を受け取り各要素の平方根のリストを返す *)
(* map_sqrt : float list -> float list *)
let rec map_sqrt lst =
  match lst with [] -> [] | first :: rest -> sqrt first :: map_sqrt rest

(* 学生ひとり分のデータ（名前、点数、成績）を表す型 *)
type gakusei_t = {
  namae : string; (* 名前 *)
  tensuu : int; (* 点数 *)
  seiseki : string; (* 成績 *)
}

(* 目的：学生のデータ gakusei を受け取り成績のついたデータを返す *)
(* hyouka : gakusei_t -> gakusei_t *)
let hyouka gakusei =
  match gakusei with
  | { namae = n; tensuu = t; seiseki = s } ->
      if t >= 80 then { namae = n; tensuu = t; seiseki = "A" }
      else if t >= 70 then { namae = n; tensuu = t; seiseki = "B" }
      else if t >= 60 then { namae = n; tensuu = t; seiseki = "C" }
      else { namae = n; tensuu = t; seiseki = "D" }

(* 目的：学生のリスト lst を受け取り成績を入れたリストを返す *)
(* map_hyouka : gakusei_t list -> gakusei_t list *)
let rec map_hyouka lst =
  match lst with [] -> [] | first :: rest -> hyouka first :: map_hyouka rest

(* 目的：関数 f とリスト lst を受け取り f を施したリストを返す *)
let rec map f lst =
  match lst with [] -> [] | first :: rest -> f first :: map f rest
;;

map sqrt [ 2.0; 3.0 ];;
map sin [ 2.0; 3.0 ]

(* 目的：実数のリスト lst を受け取り各要素の平方根のリストを返す *)
(* map_sqrt : float list -> float list *)
let map_sqrt lst = map sqrt lst

(* 目的：学生のリスト lst を受け取り成績を入れたリストを返す *)
(* map_hyouka : gakusei_t list -> gakusei_t list *)
let map_hyouka lst = map hyouka lst

(* 13.3 *)

(* 目的：受け取ったリスト lst の長さを求める *)
(* length : 'a list -> int *)
let rec length lst = match lst with [] -> 0 | first :: rest -> 1 + length rest
let is_zero n = n = 0;;

map is_zero [ 2; 1; 0; -1; -2 ];;

(* 13.4 *)

3;;
sqrt

let twice7 f = f (f 7)
let add3 x = x + 3;;

twice7 add3

(* 13.5 *)

let twice f =
  let g x = f (f x) in
  g

let time2 x = x * 2
let add6 = twice add3;;

add6 8;;
add6 9
