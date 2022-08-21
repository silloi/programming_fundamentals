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

(* テスト *)
let rb_tree0 = Empty
let rb_tree1 = insert rb_tree0 10 "x"
let rb_tree2 = insert rb_tree1 13 "y"
let rb_tree3 = insert rb_tree2 15 "z"
let test1 = rb_tree1 = Node (Empty, 10, "x", Black, Empty)

let test2 =
  rb_tree2 = Node (Empty, 10, "x", Black, Node (Empty, 13, "y", Red, Empty))

let test3 =
  rb_tree3
  = Node
      ( Node (Empty, 10, "x", Black, Empty),
        13,
        "y",
        Black,
        Node (Empty, 15, "z", Black, Empty) )
