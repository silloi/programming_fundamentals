(* 学生ひとり分のデータ（名前、点数、成績）を表す型 *)
type gakusei_t = {
  namae : string; (* 名前 *)
  tensuu : int; (* 点数 *)
  seiseki : string; (* 成績 *)
}

(* 目的：gakusei_t 型のリストの中から最高得点を取った人のレコードを返す *)
(* gakusei_max : gakusei_t list -> gakusei_t *)
let rec gakusei_max lst =
  match lst with
  | [] -> { namae = ""; tensuu = min_int; seiseki = "" }
  | ({ namae = gakusei_n; tensuu = gakusei_t; seiseki = gakuei_s } as gakusei)
    :: rest -> (
      let gakusei_max_rest = gakusei_max rest in
      match gakusei_max_rest with
      | { namae = rest_n; tensuu = rest_t; seiseki = rest_s } ->
          if gakusei_t >= rest_t then gakusei else gakusei_max_rest)

(* テスト *)
let test1 =
  gakusei_max [ { namae = "asai"; tensuu = 90; seiseki = "A" } ]
  = { namae = "asai"; tensuu = 90; seiseki = "A" }

let test2 =
  gakusei_max
    [
      { namae = "asai"; tensuu = 70; seiseki = "B" };
      { namae = "kaneko"; tensuu = 85; seiseki = "A" };
    ]
  = { namae = "kaneko"; tensuu = 85; seiseki = "A" }

let test3 =
  gakusei_max
    [
      { namae = "kaneko"; tensuu = 85; seiseki = "A" };
      { namae = "asai"; tensuu = 70; seiseki = "B" };
    ]
  = { namae = "kaneko"; tensuu = 85; seiseki = "A" }

let test4 =
  gakusei_max
    [
      { namae = "asai"; tensuu = 70; seiseki = "B" };
      { namae = "kaneko"; tensuu = 85; seiseki = "A" };
      { namae = "yoshida"; tensuu = 80; seiseki = "A" };
    ]
  = { namae = "kaneko"; tensuu = 85; seiseki = "A" }