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

(* current_index と parent_index の要素を入れ換える *)
let swap array current_index parent_index =
  let ((index_ref_c, value_c, info_c) as entry_c) = array.(current_index) in
  let ((index_ref_p, value_p, info_p) as entry_p) = array.(parent_index) in
  array.(current_index) <- entry_p;
  array.(parent_index) <- entry_c;
  index_ref_c := parent_index;
  (* 入れ換えにともなって index も付け変える *)
  index_ref_p := current_index;
  ()

(* 下方向に向かってヒープの条件を満たすように要素の入れ換えを行う *)
let rec adjust_child num array current_index =
  if current_index >= num then ()
  else
    let _, v, _ = array.(current_index) in
    let child1_index = (2 * current_index) + 1 in
    let child2_index = child1_index + 1 in
    if child1_index >= num then ()
    else
      let _, v1, _ = array.(child1_index) in
      if child2_index >= num then
        if v <= v1 then ()
        else (
          swap array current_index child1_index;
          adjust_child num array child1_index)
      else
        let _, v2, _ = array.(child2_index) in
        if v <= v1 && v <= v2 then ()
        else if v1 < v2 then (
          swap array current_index child1_index;
          adjust_child num array child1_index)
        else (
          swap array current_index child2_index;
          adjust_child num array child2_index)

(* 上方向に向かってヒープの条件を満たすように要素の入れ換えを行う *)
let rec adjust_parent array current_index =
  if current_index = 0 then ()
  else
    let _, value_c, _ = array.(current_index) in
    let parent_index = (current_index - 1) / 2 in
    let _, value_p, _ = array.(parent_index) in
    if value_c < value_p then (
      swap array current_index parent_index;
      adjust_parent array parent_index)
    else ()

(* ヒープに新しい要素を追加する *)
(* これ以上、入らないときは Full を raise する *)
(* ヒープは（破壊的に）書き変わる *)
(* insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t *)
let insert (num_ref, array) v info =
  if !num_ref >= Array.length array then raise Full
  else
    let index = ref !num_ref in
    array.(!num_ref) <- (index, v, info);
    adjust_parent array !num_ref;
    num_ref := !num_ref + 1;
    (index, (num_ref, array))

(* ヒープの !index_ref 番目の要素を返す *)
(* index が無効であれば Not_found を raise する *)
(* get : ('a, 'b) t -> index_t -> 'a * 'b *)
let get (num_ref, array) index_ref =
  if 0 <= !index_ref && !index_ref < !num_ref then
    let _, a, b = array.(!index_ref) in
    (a, b)
  else raise Not_found

(* ヒープの !index_ref 番目の値を更新したヒープを返す *)
(* ヒープは（破壊的に）書き変わる *)
(* set : ('a, 'b) t -> index_t -> 'a -> 'b -> ('a, 'b) t *)
let set (num_ref, array) index_ref v info =
  let _, v', _ = array.(!index_ref) in
  array.(!index_ref) <- (index_ref, v, info);
  if v < v' then adjust_parent array !index_ref
  else adjust_child !num_ref array !index_ref;
  (num_ref, array)

(* 最小の値を持つものとそれを取り除いたヒープの組を返す *)
(* 最小の値を持つものの index は無効な値になる *)
(* ヒープが空のときは Empty を raise する *)
(* ヒープは（破壊的に）書き変わる *)
(* split_top : ('a, 'b) t -> ('a * 'b) * ('a, 'b) t *)
let split_top (num_ref, array) =
  if !num_ref = 0 then raise Empty
  else
    let index_ref, v, info = array.(0) in
    num_ref := !num_ref - 1;
    (* 要素数をひとつ減らす *)
    array.(0) <- array.(!num_ref);
    adjust_child !num_ref array 0;
    index_ref := -1;
    (* 取り出した先頭の要素の index_ref は無効にする *)
    ((v, info), (num_ref, array))

(* ヒープ中のデータの数を返す *)
let length (num_ref, _) = !num_ref
