(* 学生ひとり分のデータ（名前、点数、成績）を表す型 *)
type gakusei_t = {
  namae : string; (* 名前 *)
  tensuu : int; (* 点数 *)
  seiseki : string; (* 成績 *)
}

(* gakusei_t 型のデータの例 *)
let gakusei0 = { namae = ""; tensuu = 0; seiseki = "" }
let gakusei1 = { namae = "asai"; tensuu = 70; seiseki = "B" }
let gakusei2 = { namae = "kaneko"; tensuu = 85; seiseki = "A" }
let gakusei3 = { namae = "yoshida"; tensuu = 80; seiseki = "A" }

(* 目的：学生 gakusei の成績が A かどうか調べる *)
(* is_seiseki_A : gakusei_t -> bool *)
let is_seiseki_A gakusei =
  match gakusei with { namae = n; tensuu = t; seiseki = s } -> s = "A"

let test1 = is_seiseki_A gakusei0 = false
let test2 = is_seiseki_A gakusei1 = false
let test3 = is_seiseki_A gakusei2 = true

(* gakusei_t list 型のデータの例 *)
let lst1 = []
let lst2 = [ gakusei1 ]
let lst3 = [ gakusei1; gakusei2 ]
let lst4 = [ gakusei3; gakusei1; gakusei2 ]

(* 目的：学生リスト lst のうち成績が A の人の数を返す *)
(* count_A : gakusei_t list -> int *)
let count_A lst = List.length (List.filter is_seiseki_A lst)

(* テスト *)
let test1 = count_A lst1 = 0
let test2 = count_A lst2 = 0
let test3 = count_A lst3 = 1
let test4 = count_A lst4 = 2
