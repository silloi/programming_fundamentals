(* 駅と駅の接続情報 *)
type ekikan_t = {
  kiten : string; (* 起点の駅名 *)
  shuten : string; (* 終点の駅名 *)
  keiyu : string; (* 経由する路線名 *)
  kyori : float; (* 2駅間の距離（km、実数） *)
  jikan : int; (* 所要時間（分、整数） *)
}

(* 駅名、最短距離、手前の駅名のリスト *)
type eki_t = {
  namae : string; (* 駅名（漢字の文字列） *)
  saitan_kyori : float; (* 最短距離（実数） *)
  temae_list : string list; (* 駅名（漢字の文字列）のリスト *)
}

(* ekikan_tree は
     - Empty              空の木、あるいは
     - Node (t1, (ekimei, lst), t2)   左の木が t1、値が (ekimei, lst)、右の木が t2 であるような節
                          （t1 と t2 が自己参照のケース）
   という形 *)

(* メトロネットワーク中のすべての駅間からなるリスト *)
let global_ekikan_list =
  [
    { kiten = "池袋"; shuten = "新大塚"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 3 };
    { kiten = "新大塚"; shuten = "茗荷谷"; keiyu = "丸ノ内線"; kyori = 1.2; jikan = 2 };
    { kiten = "茗荷谷"; shuten = "後楽園"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 2 };
    { kiten = "後楽園"; shuten = "本郷三丁目"; keiyu = "丸ノ内線"; kyori = 0.8; jikan = 1 };
  ]

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

(* 目的：漢字の駅名 ekimei1 と ekimei2、駅間の 2 分探索木 ekikan_tree を受け取ったら 2 駅間の距離を返す *)
(* みつからなかったら例外 Not_found を起こす *)
(* get_ekikan_kyori : string -> string -> (string * (string * float) list) Tree.t -> float *)
let rec get_ekikan_kyori ekimei1 ekimei2 ekikan_tree =
  let lst = Tree.search ekikan_tree ekimei1 in
  List.assoc ekimei2 lst

(* 目的：受け取った kiten、shuten、kyori を ekikan_tree に挿入した木を返す *)
(* insert1 : (string * (string * float) list) Tree.t -> string -> string -> float -> (string * (string * float) list) Tree.t *)
let rec insert1 ekikan_tree kiten shuten kyori =
  let lst = try Tree.search ekikan_tree kiten with Not_found -> [] in
  Tree.insert ekikan_tree kiten ((shuten, kyori) :: lst)

(* 目的：木 ekikan_tree に駅間 ekikan を挿入した木を返す *)
(* insert_ekikan : (string * (string * float) list) Tree.t -> ekikan_t -> (string * (string * float) list) Tree.t *)
let rec insert_ekikan ekikan ekikan_tree =
  match ekikan with
  | { kiten = ki; shuten = s; keiyu = ke; kyori = ky; jikan = j } ->
      insert1 (insert1 ekikan_tree s ki ky) ki s ky

(* 目的：木 ekikan_tree に駅間のリスト ekikan_list に含まれる駅間をすべて挿入した木を返す *)
(* inserts_ekikan : (string * (string * float) list) Tree.t -> ekikan_t list -> (string * (string * float) list) Tree.t *)
let inserts_ekikan ekikan_tree ekikan_list =
  List.fold_right insert_ekikan ekikan_list ekikan_tree

let global_ekikan_tree = inserts_ekikan Tree.empty global_ekikan_list

(* テスト *)
let test4 = get_ekikan_kyori "茗荷谷" "後楽園" global_ekikan_tree = 1.8
