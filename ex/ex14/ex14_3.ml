(* 目的：文字列 first と rest_result を結合する *)
(* concat_string : string -> string -> string *)
let concat_string first rest_result = first ^ rest_result
let test1 = concat_string "" "" = ""
let test2 = concat_string "あ" "" = "あ"
let test3 = concat_string "" "い" = "い"
let test4 = concat_string "あ" "い" = "あい"

(* 目的：文字列のリストを受け取ったら、要素を前から順に全部くっつけた文字列を返す *)
(* concat : string list -> string *)
let concat lst = List.fold_right concat_string lst ""

(* テスト *)
let test1 = concat [] = ""
let test2 = concat [ "春" ] = "春"
let test3 = concat [ "春"; "夏" ] = "春夏"
let test4 = concat [ "春"; "夏"; "秋"; "冬" ] = "春夏秋冬"
let test5 = concat [ "春"; "夏"; ""; "秋"; "冬" ] = "春夏秋冬"
