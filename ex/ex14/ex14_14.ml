(* 目的：文字列のリストを受け取ったら、要素を前から順に全部くっつけた文字列を返す *)
(* concat : string list -> string *)
let concat lst = List.fold_right ( ^ ) lst ""
