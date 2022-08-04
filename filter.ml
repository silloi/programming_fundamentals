(* リスト lst の中から条件 p を満たす要素のみを取り出す *)
(* filter : ('a -> bool) -> 'a list -> 'a list *)
let rec filter p lst =
  match lst with
  | [] -> []
  | first :: rest -> if p first then first :: filter p rest else filter p rest
