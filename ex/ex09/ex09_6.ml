(* string list は
     - []              空リスト、あるいは
     - first :: rest   最初の要素が first で残りのリストが rest
                       （rest が自己参照のケース）
   という形 *)

(* 目的：文字列のリストを受け取ったら、要素を前から順に全部くっつけた文字列を返す *)
(* concat : string list -> string *)
let rec concat lst =
  match lst with [] -> "" | first :: rest -> first ^ concat rest

(* テスト *)
let test1 = concat [] = ""
let test2 = concat [ "春" ] = "春"
let test3 = concat [ "春"; "夏" ] = "春夏"
let test4 = concat [ "春"; "夏"; "秋"; "冬" ] = "春夏秋冬"
let test5 = concat [ "春"; "夏"; ""; "秋"; "冬" ] = "春夏秋冬"
