(* 日を示す引数を取る、1 月から 12 月までを表す構成子を持つ型 *)
type year_t =
  | January of int (* 1月 *)
  | February of int (* 2月 *)
  | March of int (* 3月 *)
  | April of int (* 4月 *)
  | May of int (* 5月 *)
  | June of int (* 6月 *)
  | July of int (* 7月 *)
  | August of int (* 8月 *)
  | September of int (* 9月 *)
  | October of int (* 10月 *)
  | November of int (* 11月 *)
  | December of int (* 12月 *)
