(* 集合を表すシグネチャ *)
module type Set_t = sig
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
end

(* 集合のモジュール *)
module Set = struct
  (* 要素の型が 'a の集合の型 *)
  type 'a t = 'a list

  (* 空集合 *)
  let empty = []

  (* 目的：要素が element ひとつからなる集合を返す *)
  let singleton element = [ element ]

  (* 目的：集合 set1 と set2 の和集合を返す *)
  let union set1 set2 = set1 @ set2

  (* 目的：集合 set1 と set2 の共通部分を返す *)
  let inter set1 set2 =
    List.fold_left
      (fun lst element -> if List.mem element set2 then element :: lst else lst)
      [] set1

  (* 目的：集合 set1 と set2 の差集合を返す *)
  let diff set1 set2 =
    List.fold_left
      (fun lst element -> if List.mem element set2 then lst else element :: lst)
      [] set1

  (* 目的：要素 element が集合 set に入っているかどうかを返す *)
  let mem element set = List.mem element set
end
