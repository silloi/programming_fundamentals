(* 23.1 *)

(* 23.2 *)

(* 23.3 *)

(* 23.4 *)

(* 23.5 *)

module type Heap_t = sig
  type ('a, 'b) t
  (* 最小値を求める値が 'a 型で
     そのほかの付加情報が 'b 型であるヒープの型 *)

  val create : int -> 'a -> 'b -> ('a, 'b) t
  (* 使い方：create size key value *)
  (* ヒープのサイズと 'a 型と 'b 型のダミーの値を受け取ったら *)
  (* 空のヒープを返す *)

  type index_t
  (* ヒープの添字の型 *)

  val insert : ('a, 'b) t -> 'a -> 'b -> index_t * ('a, 'b) t
  (* 使い方：insert heap key value *)
  (* ヒープに新しい要素を追加する *)
  (* ヒープは（破壊的に）書き変わる *)

  val get : ('a, 'b) t -> index_t -> 'a * 'b
  (* 使い方：get heap index *)
  (* ヒープの index 番目の要素を返す *)

  val set : ('a, 'b) t -> index_t -> 'a -> 'b -> ('a, 'b) t
  (* 使い方：get heap index *)
  (* ヒープの index 番目の値を更新したヒープを返す *)
  (* ヒープは（破壊的に）書き変わる *)

  val split_top : ('a, 'b) t -> ('a, 'b) t -> ('a * 'b) * ('a, 'b) t
  (* 使い方：split_top heap *)
  (* 最小の値を持つものとそれを取り除いたヒープの組を返す *)
  (* ヒープは（破壊的に）書き変わる *)
end

(* 23.6 *)

module Heap = struct
  (* ヒープの添字の型 *)
  type index_t = int ref

  (* 最小値を求める値が 'a 型で
     そのほかの付加情報が 'b 型であるヒープの型 *)
  type ('a, 'b) t = int ref * (index_t * 'a * 'b) array
end

(* 23.7 *)
