(* ヒープの添字の型 *)
type index_t = int ref

(* 最小値を求める値が 'a 型で
   そのほかの付加情報が 'b 型であるヒープの型 *)
type ('a, 'b) t = int ref * (index_t * 'a * 'b) array

(* insert したときにヒープが一杯だと raise される例外 *)
exception Full

(* split_top したときにヒープが空だと raise される例外 *)
exception Empty

(* index_t 型を持つダミーの値 *)
let example_index = ref (-1)

(* ヒープのサイズと 'a 型と 'b 型のダミーの値を受け取ったら空のヒープを返す *)
(* create : int -> 'a -> 'b -> ('a, 'b) t *)
let create size a b = (ref 0, Array.init size (fun _ -> (example_index, a, b)))

(* ヒープに新しい要素を追加する *)
(* ヒープは（破壊的に）書き変わる *)
(* insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t *)
let insert heap a b =
  (* 目的：新しい要素を配列の最後に付け加える *)
  (* add_to_last : ('a, 'b) t -> 'a -> 'b -> index_t * ('a, 'b) t *)
  let add_to_last heap a b =
    let last_idx, arr = heap in
    arr.(!last_idx + 1) <- (ref (!last_idx + 1), a, b);
    (ref (!last_idx + 1), arr)
  in
  let last_idx, data = add_to_last heap a b in
  (* 目的：その要素とその親とを比べ、自分の方が小さかったら親と入れ替える。これを繰り返す *)
  (* swap_to_parent : ('a, 'b) t -> ('a, 'b) t *)
  let rec swap_to_parent heap idx =
    let _, arr = heap in
    if !idx = 0 then heap
    else
      let ego = arr.(!idx) in
      let parent_idx = (!idx + 1) / 2 in
      let parent = arr.(parent_idx) in
      if parent < ego then (idx, arr)
      else (
        arr.(parent_idx) <- ego;
        arr.(!idx) <- parent;
        swap_to_parent heap (ref parent_idx))
  in
  swap_to_parent (last_idx, data) last_idx

(* ヒープの index 番目の要素を返す *)
(* get : ('a, 'b) t -> index_t -> 'a * 'b *)
let get heap idx =
  let _, arr = heap in
  let _, a, b = arr.(idx) in
  (a, b)

(* ヒープの index 番目の値を更新したヒープを返す *)
(* ヒープは（破壊的に）書き変わる *)
(* set : ('a, 'b) t -> index_t -> 'a -> 'b -> ('a, 'b) t *)
let set heap idx a b =
  let _, arr = heap in
  arr.(!idx) <- (idx, a, b);
  heap

(* 最小の値を持つものとそれを取り除いたヒープの組を返す *)
(* ヒープは（破壊的に）書き変わる *)
(* split_top : ('a, 'b) t -> ('a * 'b) * ('a, 'b) t *)
let split_top heap =
  let top_a, top_b = get heap 0 in
  (* 目的：配列の最初の要素（値の要素）を、配列の最後の要素で上書きする *)
  (* swap_root_with_last : ('a, 'b) t -> ('a, 'b) t *)
  let swap_root_with_last heap =
    let last_idx, _ = heap in
    let last_a, last_b = get heap !last_idx in
    last_idx := -1;
    set heap (ref 0) last_a last_b
  in
  let heap = swap_root_with_last heap in
  (* 目的：index 番目の値がふたつの子より大きければ、そのうち小さい方と交換する *)
  (* swap_ego_with_minor_child : ('a, 'b) t -> index_t -> ('a, 'b) t *)
  let rec swap_ego_with_minor_child heap idx =
    let ego_a, ego_b = get heap idx in
    let child1_idx, child2_idx = ((2 * idx) + 1, (2 * idx) + 2) in
    let (child1_a, _), (child2_a, _) =
      (get heap child1_idx, get heap child2_idx)
    in
    if ego_a < child1_a && ego_a < child2_a then heap
    else if child1_a < child2_a then swap_ego_with_minor_child heap child1_idx
    else swap_ego_with_minor_child heap child2_idx
  in
  ((top_a, top_b), swap_ego_with_minor_child heap 0)
