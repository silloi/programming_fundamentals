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

(* 目的：学生 first の得点と rest_result を合計する *)
(* add_tensuu : gakusei_t -> int -> int *)
let add_tensuu first rest_result =
  match first with { namae = n; tensuu = t; seiseki = s } -> t + rest_result

(* テスト *)
let test1 = add_tensuu gakusei0 0 = 0
let test2 = add_tensuu gakusei1 0 = 70
let test3 = add_tensuu gakusei1 70 = 155
let test4 = add_tensuu gakusei2 85 = 165

(* 目的：学生のリスト lst を受け取ったら全員の得点の合計を返す *)
(* gakusei_sum : gakusei_t list -> int *)
let gakusei_sum lst = List.fold_right add_tensuu lst 0

(* テスト *)
let test1 = gakusei_sum [ gakusei0 ] = 0
let test2 = gakusei_sum [ gakusei0; gakusei1 ] = 70
let test3 = gakusei_sum [ gakusei0; gakusei1; gakusei2 ] = 155
let test2 = gakusei_sum [ gakusei0; gakusei1; gakusei2; gakusei3 ] = 235
