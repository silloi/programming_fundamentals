(* 14.1 *)

(* 目的：受け取ったリスト lst から正の要素のみを取り出す *)
(* filter_positive : int list -> int list *)
let rec filter_positive lst =
  match lst with
  | [] -> []
  | first :: rest ->
      if first > 0 then first :: filter_positive rest else filter_positive rest

(* 整数 n が 3 で割ると 1 余るかを調べる *)
(* is_mod3_1 : int -> bool *)
let is_mod3_1 n = n mod 3 = 1

(* 目的：リスト lst から 3 で割ると 1 余る要素のみを取り出す *)
(* filter_mod3_1 : int list -> int list *)
let rec filter_mod3_1 lst =
  match lst with
  | [] -> []
  | first :: rest ->
      if is_mod3_1 first then first :: filter_mod3_1 rest
      else filter_mod3_1 rest

(* リスト lst の中から条件 p を満たす要素のみを取り出す *)
(* filter : ('a -> bool) -> 'a list -> 'a list *)
let rec filter p lst =
  match lst with
  | [] -> []
  | first :: rest -> if p first then first :: filter p rest else filter p rest

(* 目的：リスト lst から 3 で割ると 1 余る要素のみを取り出す *)
(* filter_mod3_1 : int list -> int list *)
let filter_mod3_1 lst = filter is_mod3_1 lst

(* 目的：整数 n が正かどうかを調べる *)
(* let is_positive : int -> bool *)
let is_positive n = n > 0

(* 目的：受け取ったリスト lst から正の要素のみを取り出す *)
(* filter_positive : int list -> int list *)
let filter_positive lst = filter is_positive lst

(* 14.2 *)

(* 目的：受け取ったリスト lst の各要素の和を求める *)
(* sum : int list -> int *)
let rec sum lst = match lst with [] -> 0 | first :: rest -> first + sum rest

(* 目的：受け取ったリスト lst の長さを求める *)
(* length : 'a list -> int *)
let rec length lst = match lst with [] -> 0 | first :: rest -> 1 + length rest

(* 目的：lst1 と lst2 を受け取りそれらを結合したリストを返す *)
(* append : 'a list -> 'a list -> 'a list *)
let rec append lst1 lst2 =
  match lst1 with [] -> lst2 | first :: rest -> first :: append rest lst2

(* 目的：init から初めて lst の要素を右から順に f を施し込む *)
(* fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
let rec fold_right f lst init =
  match lst with
  | [] -> init
  | first :: rest -> f first (fold_right f rest init)

(* 目的：first と rest_result を加える *)
(* add_int : int -> int -> int *)
let add_int first rest_result = first + rest_result

(* 目的：受け取ったリスト lst の各要素の和を求める *)
(* sum : int list -> int *)
let sum lst = fold_right add_int lst 0

(* 目的：first は無視して rest_result に 1 を加える *)
let add_one first rest_result = 1 + rest_result

(* 目的：受け取ったリスト lst の長さを求める *)
(* length : 'a list -> int *)
let length lst = fold_right add_one lst 0

(* 目的：first を rest_result の先頭に加える *)
(* cons : 'a -> 'a list -> 'a list *)
let cons first rest_result = first :: rest_result

(* 目的：lst1 と lst2 を受け取りそれらを結合したリストを返す *)
(* append : 'a list -> 'a list -> 'a list *)
let append lst1 lst2 = fold_right cons lst1 lst2

(* 14.3 *)

(* 目的：受け取ったリスト lst の各要素の和を求める *)
(* sum : int list -> int *)
let sum lst =
  (* 目的：first と rest_result を加える *)
  (* add_int : int -> int -> int *)
  let add_int first rest_result = first + rest_result in
  fold_right add_int lst 0
;;

(* 14.4 *)

fun x -> x + 1;;
(fun x -> x + 1) 5

let add1 x = x + 1
let add1 x = x + 1

(* 目的：受け取ったリスト lst の各要素の和を求める *)
(* sum : int list -> int *)
let sum lst = fold_right (fun first rest_result -> first + rest_result) lst 0

(* 目的：受け取ったリスト lst の長さを求める *)
(* length : 'a list -> int *)
let length lst = fold_right (fun first rest_result -> 1 + rest_result) lst 0

(* 目的：lst1 と lst2 を受け取りそれらを結合したリストを返す *)
(* append : 'a list -> 'a list -> 'a list *)
let append lst1 lst2 =
  fold_right (fun first rest_result -> first :: rest_result) lst1 lst2
;;

(* 14.5 *)

3 + 5

(* 目的：受け取ったリスト lst の各要素の和を求める *)
(* sum : int list -> int *)
let sum lst = fold_right ( + ) lst 0

(* 14.6 *)

(* 目的：n から 1 までのリストを作る *)
(* enumerate : int -> int list *)
let rec enumerate n = if n = 0 then [] else n :: enumerate (n - 1);;

enumerate 5

(* 目的：n の約数のリストを返す *)
(* divisor : int -> int list *)
let divisor n = filter (fun x -> n mod x = 0) (enumerate n)

(* 目的：m 以下の完全数のリストを返す *)
(* perfect -> int -> int list *)
let perfect m =
  filter (fun n -> fold_right ( + ) (divisor n) 0 - n = n) (enumerate m)
