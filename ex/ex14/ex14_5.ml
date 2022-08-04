(* 目的：整数のリスト lst を受け取ったら偶数の要素のみを含むリストを返す *)
(* even : int list -> int list *)
let even lst =
  (* 目的：整数 n が偶数かどうか調べる *)
  (* is_even : int -> bool *)
  let is_even n = n mod 2 = 0 in
  List.filter is_even lst

(* 学生ひとり分のデータ（名前、点数、成績）を表す型 *)
type gakusei_t = {
  namae : string; (* 名前 *)
  tensuu : int; (* 点数 *)
  seiseki : string; (* 成績 *)
}

(* 目的：学生リスト lst のうち成績が A の人の数を返す *)
(* count_A : gakusei_t list -> int *)
let count_A lst =
  (* 目的：学生 gakusei の成績が A かどうか調べる *)
  (* is_seiseki_A : gakusei_t -> bool *)
  let is_seiseki_A gakusei =
    match gakusei with { namae = n; tensuu = t; seiseki = s } -> s = "A"
  in
  List.length (List.filter is_seiseki_A lst)

(* 目的：文字列のリストを受け取ったら、要素を前から順に全部くっつけた文字列を返す *)
(* concat : string list -> string *)
let concat lst =
  (* 目的：文字列 first と rest_result を結合する *)
  (* concat_string : string -> string -> string *)
  let concat_string first rest_result = first ^ rest_result in
  List.fold_right concat_string lst ""

(* 目的：学生のリスト lst を受け取ったら全員の得点の合計を返す *)
(* gakusei_sum : gakusei_t list -> int *)
let gakusei_sum lst =
  (* 目的：学生 first の得点と rest_result を合計する *)
  (* add_tensuu : gakusei_t -> int -> int *)
  let add_tensuu first rest_result =
    match first with { namae = n; tensuu = t; seiseki = s } -> t + rest_result
  in
  List.fold_right add_tensuu lst 0
