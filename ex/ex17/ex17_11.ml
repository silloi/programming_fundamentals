(* 木の節に（漢字の）駅名と「『その駅に直接つながっている駅名（漢字）』と『その駅までの距離』の組」のリストを持つ木を表す型 *)
type ekikan_tree_t =
  | Empty
  | Node of ekikan_tree_t * (string * (string * float)) list * ekikan_tree_t

(* (string * float) list は
     - []              空リスト、あるいは
     - (ekimei, kyori) :: rest   最初の要素が (ekimei, kyori) で残りのリストが rest
                       （rest が自己参照のケース）
   という形 *)

(* 目的：駅名 ekimei0 と駅名と距離の組のリスト lst を受け取ると、その駅までの距離を返す *)
(* assoc : string -> (string * float) list -> float *)
let rec assoc ekimei0 lst =
  match lst with
  | [] -> infinity
  | (ekimei, kyori) :: rest ->
      if ekimei0 = ekimei then kyori else assoc ekimei0 rest

(* テスト *)
let test1 = assoc "後楽園" [ ("新大塚", 1.2); ("後楽園", 1.8) ] = 1.8
let test2 = assoc "池袋" [ ("新大塚", 1.2); ("後楽園", 1.8) ] = infinity
