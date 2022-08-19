(* 年号を表す型 *)
type nengou_t =
  | Meiji of int (* 明治 *)
  | Taisho of int (* 大正 *)
  | Showa of int (* 昭和 *)
  | Heisei of int (* 平成 *)

(* 目的：年号を受け取ったら対応する西暦年を返す *)
(* to_seireki : nengou_t -> int *)
let to_seireki nengou =
  match nengou with
  | Meiji n -> n + 1867
  | Taisho n -> n + 1911
  | Showa n -> n + 1925
  | Heisei n -> n + 1988

(* 誕生年 tanjo と現在の年 kotoshi から年齢を求める *)
(* nenrei : nengou_t -> nengou_t -> int *)
let nenrei tanjo kotoshi =
  let seireki_tanjo = to_seireki tanjo in
  let seireki_kotoshi = to_seireki kotoshi in
  let diff = seireki_kotoshi - seireki_tanjo in
  if diff >= 0 then diff else min_int

(* テスト *)
let test1 = nenrei (Showa 20) (Heisei 5) = 48
let test2 = nenrei (Showa 20) (Showa 25) = 5
let test3 = nenrei (Heisei 5) (Heisei 30) = 25
let test4 = nenrei (Meiji 1) (Showa 63) = 120
