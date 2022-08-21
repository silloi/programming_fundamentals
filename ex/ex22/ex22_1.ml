let counter = ref (-1)

(* 呼ばれるごとに文字列 s に異なる数字をつけた文字列を返す *)
(* gensym : string -> string *)
let gensym s =
  counter := !counter + 1;
  s ^ string_of_int !counter

(* テスト *)
let test1 = gensym "a" = "a0"
let test2 = gensym "a" = "a1"
let test3 = gensym "x" = "x2"
