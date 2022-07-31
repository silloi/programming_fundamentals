let tsuru_no_ashi x = x * 2
let kame_no_ashi y = y * 4

(* 目的：鶴の数 x と亀の数 y を受け取ったら足の数の合計を返す *)
(* tsurukame_no_ashi : int -> int -> int *)
let tsurukame_no_ashi tsuru kame = tsuru_no_ashi tsuru + kame_no_ashi kame

(* 目的：鶴と亀の数の合計 x と足の数の合計 y を受け取ったら鶴の数を返す *)
(* tsurukame : int -> int -> int *)
let tsurukame kazu ashi = (tsurukame_no_ashi 0 kazu - ashi) / 2

(* テスト *)
let test1 = tsurukame 4 12 = 2
let test2 = tsurukame 4 14 = 1
let test3 = tsurukame 4 10 = 3
