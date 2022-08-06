(* 15.1 *)

(* 目的：受け取ったリスト lst の各要素の和を求める *)
(* sum : int list -> int *)
let rec sum lst = match lst with [] -> 0 | first :: rest -> first + sum rest

(* 15.2 *)

(* 目的：lst の中から n より小さい要素のみを取り出す *)
(* take_less : int -> int list -> int list *)
let take_less n lst = []

(* 目的：lst の中から n より大きい要素のみを取り出す *)
(* take_greater : int -> int list -> int list *)
let take_greater n lst = []

(* 目的：受け取った lst をクイックソートを使って昇順に整列する *)
(* quick_sort : int list -> int list *)
let rec quick_sort lst =
  match lst with
  | [] -> []
  | first :: rest ->
      quick_sort (take_less first rest)
      @ [ first ]
      @ quick_sort (take_greater first rest)

(* テスト *)
let test1 = quick_sort [] = []
let test2 = quick_sort [ 1 ] = [ 1 ]
let test3 = quick_sort [ 1; 2 ] = [ 1; 2 ]
let test4 = quick_sort [ 2; 1 ] = [ 1; 2 ]
let test5 = quick_sort [ 5; 4; 9; 8; 2; 3 ] = [ 2; 3; 4; 5; 8; 9 ]

(* 15.3 *)

(* 目的：lst の中から n より小さい要素のみを取り出す *)
(* take_less : int -> int list -> int list *)
let take_less n lst = List.filter (fun item -> item < n) lst

(* 目的：lst の中から n より大きい要素のみを取り出す *)
(* take_greater : int -> int list -> int list *)
let take_greater n lst = List.filter (fun item -> item > n) lst

(* 目的：lst の中から n より p である要素のみを取り出す *)
(* take : int -> int list -> (int -> int -> bool) -> int list *)
let take n lst p = List.filter (fun item -> p item n) lst

(* 目的：受け取った lst をクイックソートを使って昇順に整列する *)
(* quick_sort : int list -> int list *)
let rec quick_sort lst =
  (* 目的：lst の中から n より p である要素のみを取り出す *)
  (* take : int -> int list -> (int -> int -> bool) -> int list *)
  let take n lst p = List.filter (fun item -> p item n) lst in
  (* 目的：lst の中から n より小さい要素のみを取り出す *)
  (* take_less : int -> int list -> int list *)
  let take_less n lst = take n lst ( < ) in
  (* 目的：lst の中から n より大きい要素のみを取り出す *)
  (* take_greater : int -> int list -> int list *)
  let take_greater n lst = take n lst ( > ) in
  match lst with
  | [] -> []
  | first :: rest ->
      quick_sort (take_less first rest)
      @ [ first ]
      @ quick_sort (take_greater first rest)

(* テスト *)
let test1 = quick_sort [] = []
let test2 = quick_sort [ 1 ] = [ 1 ]
let test3 = quick_sort [ 1; 2 ] = [ 1; 2 ]
let test4 = quick_sort [ 2; 1 ] = [ 1; 2 ]
let test5 = quick_sort [ 5; 4; 9; 8; 2; 3 ] = [ 2; 3; 4; 5; 8; 9 ]

(* 15.4 *)

(* 目的：級数の第 n 項の値を求める *)
(* dai_n_kou : int -> int *)
let rec dai_n_kou n = if n = 0 then 1.0 else dai_n_kou (n - 1) /. float_of_int n

(* 目的：e の近似値を求める *)
(* e : int -> float *)
let rec e n =
  if dai_n_kou n < 0.00001 then dai_n_kou n else dai_n_kou n +. e (n + 1)

let rec e n =
  let d = dai_n_kou n in
  if d < 0.00001 then d else d +. e (n + 1)
