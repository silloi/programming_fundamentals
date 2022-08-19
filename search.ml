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
   という形
*)

(* 目的：data が 2 分探索木 tree に含まれているかを調べる *)
(* search : tree_t -> int -> bool *)
let rec search tree data =
  match tree with
  | Empty -> false
  | Leaf n -> data = n
  | Node (t1, n, t2) ->
      if data = n then true
      else if data < n then search t1 data
      else search t2 data

(* 2 分探索木の例 *)
let tree1 = Empty
let tree2 = Leaf 3
let tree3 = Node (Leaf 1, 2, Leaf 3)
let tree4 = Node (Empty, 7, Leaf 9)
let tree5 = Node (tree3, 6, tree4)

(* テスト *)
let test1 = search tree1 3 = false
let test2 = search tree2 3 = true
let test3 = search tree2 4 = false
let test4 = search tree5 6 = true
let test5 = search tree5 2 = true
let test6 = search tree5 1 = true
let test7 = search tree5 4 = false
let test8 = search tree5 7 = true
let test9 = search tree5 8 = false
