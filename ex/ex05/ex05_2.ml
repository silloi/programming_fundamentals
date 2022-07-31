(* 目的：時間 t が午前か午後かを返す *)
(* jikan : int -> string *)
let jikan t = if t < 12 then "午前" else "午後"

(* テスト *)
let test1 = jikan 0 = "午前"
let test2 = jikan 6 = "午前"
let test3 = jikan 12 = "午後"
let test4 = jikan 18 = "午後"
let test5 = jikan 24 = "午後"
