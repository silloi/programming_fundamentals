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

let ekikan1 =
  { kiten = "新大塚"; shuten = "茗荷谷"; keiyu = "丸ノ内線"; kyori = 1.2; jikan = 2 }

(* 目的：木 ekikan_tree に駅間 ekikan を挿入した木を返す *)
(* insert_ekikan : ekikan_t -> ekikan_tree_t -> ekikan_tree_t *)
let rec insert_ekikan ekikan ekikan_tree =
  let rec insert_eki ekikan_tree kiten0 shuten0 kyori =
    match ekikan_tree with
    | Empty -> Node (Empty, (kiten0, [ (shuten0, kyori) ]), Empty)
    | Node (t1, (kiten, lst), t2) ->
        if kiten = kiten0 then Node (t1, (kiten, (shuten0, kyori) :: lst), t2)
        else if kiten < kiten0 then
          Node (t1, (kiten, lst), insert_eki t2 kiten0 shuten0 kyori)
        else Node (insert_eki t1 kiten0 shuten0 kyori, (kiten, lst), t2)
  in
  match ekikan with
  | { kiten = ki; shuten = s; keiyu = ke; kyori = ky; jikan = j } ->
      let ekikan_tree1 = insert_eki ekikan_tree ki s ky in
      let ekikan_tree2 = insert_eki ekikan_tree1 s ki ky in
      ekikan_tree2

(* テスト *)
let test1 =
  insert_ekikan ekikan1 Empty
  = Node
      ( Empty,
        ("新大塚", [ ("茗荷谷", 1.2) ]),
        Node (Empty, ("茗荷谷", [ ("新大塚", 1.2) ]), Empty) )

let test2 =
  insert_ekikan ekikan1 (Node (Empty, ("新大塚", []), Empty))
  = Node
      ( Empty,
        ("新大塚", [ ("茗荷谷", 1.2) ]),
        Node (Empty, ("茗荷谷", [ ("新大塚", 1.2) ]), Empty) )

let test3 =
  insert_ekikan ekikan1 (Node (Empty, ("茗荷谷", []), Empty))
  = Node
      ( Node (Empty, ("新大塚", [ ("茗荷谷", 1.2) ]), Empty),
        ("茗荷谷", [ ("新大塚", 1.2) ]),
        Empty )

let test4 =
  insert_ekikan ekikan1 (Node (Empty, ("池袋", []), Empty))
  = Node
      ( Node (Empty, ("新大塚", [ ("茗荷谷", 1.2) ]), Empty),
        ("池袋", []),
        Node (Empty, ("茗荷谷", [ ("新大塚", 1.2) ]), Empty) )
