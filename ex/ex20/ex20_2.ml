(* 赤か黒かを示す型 *)
type color_t = Red | Black

(* 赤黒木を表す型 *)
type ('a, 'b) rb_tree_t =
  | Empty (* 空の木 *)
  | Node of ('a, 'b) rb_tree_t * 'a * 'b * color_t * ('a, 'b) rb_tree_t

(* 木 rb_tree を受け取ったら赤黒木になるようバランスする *)
(* balance : ('a, 'b) rb_tree_t -> ('a, 'b) rb_tree_t *)
let balance rb_tree =
  match rb_tree with
  | Empty -> Empty
  | Node (Node (Node (a, xk, xv, Red, b), yk, yv, Red, c), zk, zv, Black, d)
  | Node (Node (a, xk, xv, Red, Node (b, yk, yv, Red, c)), zk, zv, Black, d)
  | Node (a, xk, xv, Black, Node (Node (b, yk, yv, Red, c), zk, zv, Red, d))
  | Node (a, xk, xv, Black, Node (b, yk, yv, Red, Node (c, zk, zv, Red, d))) ->
      Node (Node (a, xk, xv, Black, b), yk, yv, Red, Node (c, zk, zv, Black, d))
  | _ -> rb_tree
