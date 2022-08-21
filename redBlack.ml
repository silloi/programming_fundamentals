(* 赤黒木を現すモジュール *)

(* 赤か黒かを示す型 *)
type color_t = Red | Black

(* 赤黒木を表す型 *)
type ('a, 'b) t =
  | Empty (* 空の木 *)
  | Node of ('a, 'b) t * 'a * 'b * color_t * ('a, 'b) t

(* 空の木 *)
let empty = Empty

(* 木 rb_tree を受け取ったら赤黒木になるようバランスする *)
(* balance : ('a, 'b) RedBlack.t -> ('a, 'b) RedBlack.t *)
let balance rb_tree =
  match rb_tree with
  | Empty -> Empty
  | Node (Node (Node (a, xk, xv, Red, b), yk, yv, Red, c), zk, zv, Black, d)
  | Node (Node (a, xk, xv, Red, Node (b, yk, yv, Red, c)), zk, zv, Black, d)
  | Node (a, xk, xv, Black, Node (Node (b, yk, yv, Red, c), zk, zv, Red, d))
  | Node (a, xk, xv, Black, Node (b, yk, yv, Red, Node (c, zk, zv, Red, d))) ->
      Node (Node (a, xk, xv, Black, b), yk, yv, Red, Node (c, zk, zv, Black, d))
  | _ -> rb_tree

(* 赤黒木 tree にキー key と値 value を挿入した赤黒木を返す *)
(* insert : ('a, 'b) rb_tree -> 'a -> 'b -> ('a, 'b) rb_tree *)
let insert rb_tree key value =
  let rec ins rb_tree =
    match rb_tree with
    | Empty -> Node (Empty, key, value, Red, Empty)
    | Node (left, k, v, color, right) ->
        if key = k then Node (left, k, value, color, right)
        else if key < k then balance (Node (ins left, k, v, color, right))
        else balance (Node (left, k, v, color, ins right))
  in
  match ins rb_tree with
  | Empty -> assert false (* 絶対に空ではない *)
  | Node (left, k, v, color, right) -> Node (left, k, v, Black, right)

(* 目的：rb_tree の中のキー k に対応する値を探して返す *)
(* みつからなければ例外 Not_found を起こす *)
(* search : ('a, 'b) RedBlack.t -> 'a -> 'b *)
let rec search rb_tree k =
  match rb_tree with
  | Empty -> raise Not_found
  | Node (left, key, value, color, right) ->
      if k = key then value
      else if k < key then search left k
      else search right k
