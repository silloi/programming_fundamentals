(* 学生ひとり分のデータ（名前、点数、成績）を表す型 *)
type gakusei_t = {
  namae : string; (* 名前 *)
  tensuu : int; (* 点数 *)
  seiseki : string; (* 成績 *)
}

(* 目的：学生リスト lst のうち成績が seiseki0 の人の数を返す *)
(* count : gakusei_t list -> string -> int *)
let count lst seiseki0 =
  let is_seiseki seiseki =
    match seiseki with { namae = n; tensuu = t; seiseki = s } -> s = seiseki0
  in
  let lst_seiseki = List.filter is_seiseki lst in
  List.length lst_seiseki
