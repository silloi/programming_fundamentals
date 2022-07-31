let tsuru_no_ashi x = x * 2
let kame_no_ashi y = y * 4

(* 目的：鶴の数 x と亀の数 y を受け取ったら足の数の合計を返す *)
(* tsurukame_no_ashi : int -> int -> int *)
let tsurukame_no_ashi tsuru kame = tsuru_no_ashi tsuru + kame_no_ashi kame

(* テスト *)
let test1 = tsurukame_no_ashi 2 2 = 12
let test2 = tsurukame_no_ashi 2 5 = 24
let test3 = tsurukame_no_ashi 5 2 = 18
