(* 駅と駅の接続情報 *)
type ekikan_t = {
  kiten : string; (* 起点の駅名 *)
  shuten : string; (* 終点の駅名 *)
  keiyu : string; (* 経由する路線名 *)
  kyori : float; (* 2駅間の距離（km、実数） *)
  jikan : int; (* 所要時間（分、整数） *)
}

(* 目的：漢字の駅名 ekimei1 と ekimei2、駅間リスト lst を受け取ったら 2 駅間の距離を返す *)
(* get_ekikan_kyori : string -> string -> ekikan_t list -> float *)
let rec get_ekikan_kyori ekimei1 ekimei2 lst1 =
  match lst1 with
  | [] -> raise Not_found
  | { kiten; shuten; keiyu; kyori; jikan } :: rest ->
      if
        (ekimei1 = kiten && ekimei2 = shuten)
        || (ekimei1 = shuten && ekimei2 = kiten)
      then kyori
      else get_ekikan_kyori ekimei1 ekimei2 rest

(* 木の節に（漢字の）駅名と「『その駅に直接つながっている駅名（漢字）』と『その駅までの距離』の組」のリストを持つ木を表す型 *)
type ekikan_tree_t =
  | Empty
  | Node of ekikan_tree_t * (string * (string * float) list) * ekikan_tree_t

(* ekikan_tree は
     - Empty              空の木、あるいは
     - Node (t1, (ekimei, lst), t2)   左の木が t1、値が (ekimei, lst)、右の木が t2 であるような節
                          （t1 と t2 が自己参照のケース）
   という形 *)

(* 駅と駅の接続情報 *)
type ekikan_t = {
  kiten : string; (* 起点の駅名 *)
  shuten : string; (* 終点の駅名 *)
  keiyu : string; (* 経由する路線名 *)
  kyori : float; (* 2駅間の距離（km、実数） *)
  jikan : int; (* 所要時間（分、整数） *)
}

(* 目的：駅名 ekimei0 と駅名と距離の組のリスト lst を受け取ると、その駅までの距離を返す *)
(* みつからないときには例外 Not_found を発生する *)
(* assoc : string -> (string * float) list -> float *)
let rec assoc ekimei0 lst =
  match lst with
  | [] -> raise Not_found
  | (ekimei, kyori) :: rest ->
      if ekimei0 = ekimei then kyori else assoc ekimei0 rest

(* 目的：漢字の駅名 ekimei1 と ekimei2、駅間の木 ekikan_tree を受け取ったら 2 駅間の距離を返す *)
(* get_ekikan_kyori : string -> string -> ekikan_tree_t -> float *)
let rec get_ekikan_kyori ekimei1 ekimei2 ekikan_tree =
  match ekikan_tree with
  | Empty -> raise Not_found
  | Node (t1, (ekimei0, lst), t2) ->
      if ekimei0 = ekimei1 then assoc ekimei2 lst
      else if ekimei0 = ekimei2 then assoc ekimei1 lst
      else if ekimei1 < ekimei0 then get_ekikan_kyori ekimei1 ekimei2 t1
      else get_ekikan_kyori ekimei1 ekimei2 t2
