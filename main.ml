(* メイン関数 *)
(* main : string -> string -> unit *)
let main kiten shuten =
  let result = Metro.dijkstra kiten shuten in
  Metro.print_eki result
(* 結果を表示する *)

(* メイン関数の呼び出し *)
let _ = main Sys.argv.(1) Sys.argv.(2)
