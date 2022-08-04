(* 目的：整数のリスト lst を受け取ったら偶数の要素のみを含むリストを返す *)
(* even : int list -> int list *)
let even lst = List.filter (fun n -> n mod 2 = 0) lst

(* 学生ひとり分のデータ（名前、点数、成績）を表す型 *)
type gakusei_t = {
  namae : string; (* 名前 *)
  tensuu : int; (* 点数 *)
  seiseki : string; (* 成績 *)
}

(* 目的：学生リスト lst のうち成績が A の人の数を返す *)
(* count_A : gakusei_t list -> int *)
let count_A lst =
  List.length
    (List.filter
       (fun gakusei ->
         match gakusei with { namae = n; tensuu = t; seiseki = s } -> s = "A")
       lst)

(* 目的：文字列のリストを受け取ったら、要素を前から順に全部くっつけた文字列を返す *)
(* concat : string list -> string *)
let concat lst =
  List.fold_right (fun first rest_result -> first ^ rest_result) lst ""

(* 目的：学生のリスト lst を受け取ったら全員の得点の合計を返す *)
(* gakusei_sum : gakusei_t list -> int *)
let gakusei_sum lst =
  List.fold_right
    (fun first rest_result ->
      match first with
      | { namae = n; tensuu = t; seiseki = s } -> t + rest_result)
    lst 0
