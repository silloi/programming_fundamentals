(* 学生ひとり分のデータ（名前、点数、成績）を表す型 *)
type gakusei_t = {
  namae : string; (* 名前 *)
  tensuu : int; (* 点数 *)
  seiseki : string; (* 成績 *)
}

(* 目的：tensuu フィールドについて昇順の学生のリスト lst に gakusei を挿入したリストを返す*)
(* insert gakusei_t list -> gakusei_t -> gakusei_t list *)
let rec gakusei_insert lst gakusei =
  match lst with
  | [] -> [ gakusei ]
  | ({ namae = first_n; tensuu = first_t; seiseki = first_s } as first) :: rest
    -> (
      match gakusei with
      | { namae = gakusei_n; tensuu = gakusei_t; seiseki = gakusei_s } ->
          if first_t < gakusei_t then first :: gakusei_insert rest gakusei
          else gakusei :: first :: rest)

(* テスト *)
let test1 =
  gakusei_insert [] { namae = "asai"; tensuu = 90; seiseki = "" }
  = [ { namae = "asai"; tensuu = 90; seiseki = "" } ]

let test2 =
  gakusei_insert
    [ { namae = "asai"; tensuu = 70; seiseki = "B" } ]
    { namae = "kaneko"; tensuu = 85; seiseki = "A" }
  = [
      { namae = "asai"; tensuu = 70; seiseki = "B" };
      { namae = "kaneko"; tensuu = 85; seiseki = "A" };
    ]

let test3 =
  gakusei_insert
    [ { namae = "kaneko"; tensuu = 85; seiseki = "A" } ]
    { namae = "asai"; tensuu = 70; seiseki = "B" }
  = [
      { namae = "asai"; tensuu = 70; seiseki = "B" };
      { namae = "kaneko"; tensuu = 85; seiseki = "A" };
    ]

let test4 =
  gakusei_insert
    [
      { namae = "asai"; tensuu = 70; seiseki = "B" };
      { namae = "kaneko"; tensuu = 85; seiseki = "A" };
    ]
    { namae = "yoshida"; tensuu = 80; seiseki = "A" }
  = [
      { namae = "asai"; tensuu = 70; seiseki = "B" };
      { namae = "yoshida"; tensuu = 80; seiseki = "A" };
      { namae = "kaneko"; tensuu = 85; seiseki = "A" };
    ]

(* 目的：学生のデータのリスト lst を受け取ったら tensuu フィールドの順に整列して返す *)
(* gakusei_sort : gakusei_t list -> gakusei_t list *)
let rec gakusei_sort lst =
  match lst with
  | [] -> []
  | first :: rest -> gakusei_insert (gakusei_sort rest) first

(* テスト *)
let test5 = gakusei_sort [] = []

let test6 =
  gakusei_sort [ { namae = "asai"; tensuu = 90; seiseki = "" } ]
  = [ { namae = "asai"; tensuu = 90; seiseki = "" } ]

let test7 =
  gakusei_sort
    [
      { namae = "asai"; tensuu = 70; seiseki = "B" };
      { namae = "kaneko"; tensuu = 85; seiseki = "A" };
    ]
  = [
      { namae = "asai"; tensuu = 70; seiseki = "B" };
      { namae = "kaneko"; tensuu = 85; seiseki = "A" };
    ]

let test8 =
  gakusei_sort
    [
      { namae = "kaneko"; tensuu = 85; seiseki = "A" };
      { namae = "asai"; tensuu = 70; seiseki = "B" };
    ]
  = [
      { namae = "asai"; tensuu = 70; seiseki = "B" };
      { namae = "kaneko"; tensuu = 85; seiseki = "A" };
    ]

let test9 =
  gakusei_sort
    [
      { namae = "yoshida"; tensuu = 80; seiseki = "A" };
      { namae = "kaneko"; tensuu = 85; seiseki = "A" };
      { namae = "asai"; tensuu = 70; seiseki = "B" };
    ]
  = [
      { namae = "asai"; tensuu = 70; seiseki = "B" };
      { namae = "yoshida"; tensuu = 80; seiseki = "A" };
      { namae = "kaneko"; tensuu = 85; seiseki = "A" };
    ]
