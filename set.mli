(* 集合を表すシグネチャ *)

type 'a t
(* 要素の型が 'a の集合の型。型の中身は非公開 *)

val empty : 'a t
(* 使い方：empty *)
(* 空集合 *)

val singleton : 'a -> 'a t
(* 使い方：singleton element *)
(* 要素ひとつからなる集合 *)

val union : 'a t -> 'a t -> 'a t
(* 使い方：union set1 set2 *)
(* 和集合 *)

val inter : 'a t -> 'a t -> 'a t
(* 使い方：inter set1 set2 *)
(* 共通部分 *)

val diff : 'a t -> 'a t -> 'a t
(* 使い方：diff set1 set2 *)
(* 差集合 *)

val mem : 'a -> 'a t -> bool
(* 使い方：mem element set *)
(* 要素が集合に入っているか *)
