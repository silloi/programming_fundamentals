(* 目的：鶴の数 x を与えられたら、足の本数を返す *)
(* tsuru_no_ashi : int -> int *)
let tsuru_no_ashi x = x * 2

(* テスト *)
let test1 = tsuru_no_ashi 2 = 4
let test2 = tsuru_no_ashi 5 = 10
let test3 = tsuru_no_ashi 10 = 20

(* 目的：亀の数 y を与えられたら、足の本数を返す *)
(* kame_no_ashi : int -> int *)
let tsuru_no_ashi y = y * 4

(* テスト *)
let test1 = tsuru_no_ashi 2 = 8
let test2 = tsuru_no_ashi 5 = 20
let test3 = tsuru_no_ashi 10 = 40
