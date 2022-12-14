(* 木を表す型 *)
type tree_t =
  | Empty (* 空の木 *)
  | Leaf of int (* 葉 *)
  | Node of tree_t * int * tree_t (* 節 *)

(* tree は
     - Empty              空の木、あるいは
     - Leaf (n)           値が n の葉、あるいは
     - Node (t1, n, t2)   左の木が t1、値が n、右の木が t2 であるような節
                          （t1 と t2 が自己参照のケース）
   という形 *)

(* 木の例 *)
let tree1 = Empty
let tree2 = Leaf 3
let tree3 = Node (tree1, 4, tree2)
let tree4 = Node (tree2, 5, tree3)

(* 目的：木 tree の節や葉に入っている値すべてに関数 f を適用した木を返す *)
(* tree_map : int -> int -> tree_t -> tree_t *)
let rec tree_map f tree =
  match tree with
  | Empty -> Empty
  | Leaf n -> Leaf (f n)
  | Node (t1, n, t2) -> Node (tree_map f t1, f n, tree_map f t2)

(* テスト *)
let test1 = tree_map (fun x -> x + 1) tree1 = Empty
let test2 = tree_map (fun x -> x + 1) tree2 = Leaf 4
let tree3 = tree_map (fun x -> x + 1) tree3 = Node (Empty, 5, Leaf 4)

let tree4 =
  tree_map (fun x -> x + 1) tree4 = Node (Leaf 4, 6, Node (Empty, 5, Leaf 4))
