(* 19.1 *)

(* 座標を表すモジュール *)
module Zahyou = struct
  let x = 3.0 (* x 座標 *)
  let y = 4.0 (* y 座標 *)

  (* 目的：受け取った点 (a, b) から点 (x, y) までの距離を求める *)
  (* kyori : float * float -> float *)
  let kyori (a, b) = sqrt (((x -. a) *. (x -. a)) +. ((y -. b) *. (y -. b)))
end

(* 19.2 *)

(* 2 分探索木を現すモジュール *)
module Tree = struct
  (* 2 分探索木を表す型 *)
  type ('a, 'b) t = Empty | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t

  (* 空の木 *)
  let empty = Empty

  (* 目的：tree にキーが k で値が v を挿入した木を返す *)
  (* insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t *)
  let rec insert tree k v =
    match tree with
    | Empty -> Node (Empty, k, v, Empty)
    | Node (left, key, value, right) ->
        if k = key then Node (left, key, v, right)
        else if k < key then Node (insert left k v, key, value, right)
        else Node (left, key, value, insert right k v)

  (* 目的：tree の中のキー k に対応する値を探して返す *)
  (* みつからなければ例外 Not_found を起こす *)
  (* search : ('a, 'b) t -> 'a -> 'b *)
  let rec search tree k =
    match tree with
    | Empty -> raise Not_found
    | Node (left, key, value, right) ->
        if k = key then value
        else if k < key then search left k
        else search right k
end
;;

Tree.insert Tree.empty "a" 3

(* 19.3 *)

(* 2 分探索木を表すモジュールのシグネチャ *)
module type Tree_t = sig
  type ('a, 'b) t
  (* キーが 'a 型、値が 'b 型の木の型。型の中身は非公開 *)

  val empty : ('a, 'b) t
  (* 使い方：empty *)
  (* 空の木 *)

  val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  (* 使い方：insert tree key value *)
  (* 木 tree にキー key と値 value を挿入した木を返す *)
  (* キーがすでに存在していたら新しい値に置き換える *)

  val search : ('a, 'b) t -> 'a -> 'b
  (* 使い方：search tree key *)
  (* 木 tree の中からキー key に対応する値を探して返す *)
  (* みつからなければ Not_found を raise する *)
end

module NewTree : Tree_t = Tree;;

NewTree.insert NewTree.empty "a" 3

(* 19.4 *)

(* 2 分探索木を現すモジュール *)
module Tree : Tree_t = struct
  (* 2 分探索木を表す型 *)
  type ('a, 'b) t = Empty | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t

  (* 空の木 *)
  let empty = Empty

  (* 目的：tree にキーが k で値が v を挿入した木を返す *)
  (* insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t *)
  let rec insert tree k v =
    match tree with
    | Empty -> Node (Empty, k, v, Empty)
    | Node (left, key, value, right) ->
        if k = key then Node (left, key, v, right)
        else if k < key then Node (insert left k v, key, value, right)
        else Node (left, key, value, insert right k v)

  (* 目的：tree の中のキー k に対応する値を探して返す *)
  (* みつからなければ例外 Not_found を起こす *)
  (* search : ('a, 'b) t -> 'a -> 'b *)
  let rec search tree k =
    match tree with
    | Empty -> raise Not_found
    | Node (left, key, value, right) ->
        if k = key then value
        else if k < key then search left k
        else search right k
end

(* 2 分探索木を現すモジュール *)
module Tree : sig
  type ('a, 'b) t
  (* キーが 'a 型、値が 'b 型の木の型。型の中身は非公開 *)

  val empty : ('a, 'b) t
  (* 使い方：empty *)
  (* 空の木 *)

  val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  (* 使い方：insert tree key value *)
  (* 木 tree にキー key と値 value を挿入した木を返す *)
  (* キーがすでに存在していたら新しい値に置き換える *)

  val search : ('a, 'b) t -> 'a -> 'b
  (* 使い方：search tree key *)
  (* 木 tree の中からキー key に対応する値を探して返す *)
  (* みつからなければ Not_found を raise する *)
end = struct
  (* 2 分探索木を表す型 *)
  type ('a, 'b) t = Empty | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t

  (* 空の木 *)
  let empty = Empty

  (* 目的：tree にキーが k で値が v を挿入した木を返す *)
  (* insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t *)
  let rec insert tree k v =
    match tree with
    | Empty -> Node (Empty, k, v, Empty)
    | Node (left, key, value, right) ->
        if k = key then Node (left, key, v, right)
        else if k < key then Node (insert left k v, key, value, right)
        else Node (left, key, value, insert right k v)

  (* 目的：tree の中のキー k に対応する値を探して返す *)
  (* みつからなければ例外 Not_found を起こす *)
  (* search : ('a, 'b) t -> 'a -> 'b *)
  let rec search tree k =
    match tree with
    | Empty -> raise Not_found
    | Node (left, key, value, right) ->
        if k = key then value
        else if k < key then search left k
        else search right k
end

(* 19.6 *)

(* 19.7 *)
