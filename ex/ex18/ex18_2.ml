(* 八百屋においてある野菜と値段のリストの例 *)
let yaoya_list = [ ("トマト", 300); ("たまねぎ", 200); ("にんじん", 150); ("ほうれん草", 200) ]

(* 目的：item の値段を調べる *)
(* price : string -> (string * int) list -> int option *)
let rec price item yaoya_list =
  match yaoya_list with
  | [] -> None
  | (yasai, nedan) :: rest ->
      if item = yasai then Some nedan else price item rest

(* 目的：野菜のリスト yasai_list の中で八百屋のリスト yaoya_list にない野菜の数を返す *)
(* count_urikire_yasai : string list -> (string * int) list -> int *)
let rec count_urikire_yasai yasai_list yaoya_list =
  match yasai_list with
  | [] -> 0
  | first :: rest -> (
      match price first yaoya_list with
      | None -> 1 + count_urikire_yasai rest yaoya_list
      | Some p -> count_urikire_yasai rest yaoya_list)

let test1 = count_urikire_yasai [ "トマト" ] yaoya_list = 0
let test2 = count_urikire_yasai [ "トマト"; "たまねぎ" ] yaoya_list = 0

let test3 = count_urikire_yasai [ "トマト"; "たまねぎ"; "じゃがいも" ] yaoya_list = 1
