(* 赤か黒かを示す型 *)
type color_t = Red | Black

(* 赤黒木を表す型 *)
type ('a, 'b) rb_tree_t =
  | Empty (* 空の木 *)
  | Node of ('a, 'b) rb_tree_t * 'a * 'b * color_t * ('a, 'b) rb_tree_t

(* 目的：rb_tree の中のキー k に対応する値を探して返す *)
(* みつからなければ例外 Not_found を起こす *)
(* search : ('a, 'b) rb_tree_t -> 'a -> 'b *)
let rec search rb_tree k =
  match rb_tree with
  | Empty -> raise Not_found
  | Node (left, key, value, color, right) ->
      if k = key then value
      else if k < key then search left k
      else search right k

(* 赤黒木の例 *)
let rb_tree =
  Node
    ( Node (Empty, 10, "x", Black, Empty),
      13,
      "y",
      Red,
      Node (Empty, 15, "z", Black, Empty) )

(* テスト *)
let test1 = search rb_tree 10 = "x"
let test2 = search rb_tree 13 = "y"
let test3 = search rb_tree 15 = "z"
