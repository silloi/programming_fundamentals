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
(* assoc : string -> (string * float) list -> float *)
let rec assoc ekimei0 lst =
  match lst with
  | [] -> infinity
  | (ekimei, kyori) :: rest ->
      if ekimei0 = ekimei then kyori else assoc ekimei0 rest

(* 目的：漢字の駅名 ekimei1 と ekimei2、駅間の木 ekikan_tree を受け取ったら 2 駅間の距離を返す *)
(* get_ekikan_kyori : string -> string -> ekikan_tree_t -> float *)
let rec get_ekikan_kyori ekimei1 ekimei2 ekikan_tree =
  match ekikan_tree with
  | Empty -> infinity
  | Node (t1, (ekimei0, lst), t2) ->
      if ekimei0 = ekimei1 then assoc ekimei2 lst
      else if ekimei0 = ekimei2 then assoc ekimei1 lst
      else if ekimei1 < ekimei0 then get_ekikan_kyori ekimei1 ekimei2 t1
      else get_ekikan_kyori ekimei1 ekimei2 t2

(* 目的：木 ekikan_tree に駅間 ekikan を挿入した木を返す *)
(* insert_ekikan : ekikan_tree_t -> ekikan_t -> ekikan_tree_t *)
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

(* 目的：木 ekikan_tree に駅間のリスト ekikan_list に含まれる駅間をすべて挿入した木を返す *)
(* inserts_ekikan : ekikan_tree_t -> ekikan_t list -> ekikan_tree_t *)
let inserts_ekikan ekikan_tree ekikan_list =
  List.fold_right insert_ekikan ekikan_list ekikan_tree

(* テスト *)
let test1 = get_ekikan_kyori "" "" Empty = infinity

let test2 =
  get_ekikan_kyori "" "茗荷谷"
    (inserts_ekikan Empty
       [
         {
           kiten = "茗荷谷";
           shuten = "後楽園";
           keiyu = "丸ノ内線";
           kyori = 1.8;
           jikan = 2;
         };
         {
           kiten = "淡路町";
           shuten = "大手町";
           keiyu = "丸ノ内線";
           kyori = 0.9;
           jikan = 2;
         };
       ])
  = infinity

let test3 =
  get_ekikan_kyori "茗荷谷" ""
    (inserts_ekikan Empty
       [
         {
           kiten = "茗荷谷";
           shuten = "後楽園";
           keiyu = "丸ノ内線";
           kyori = 1.8;
           jikan = 2;
         };
         {
           kiten = "淡路町";
           shuten = "大手町";
           keiyu = "丸ノ内線";
           kyori = 0.9;
           jikan = 2;
         };
       ])
  = infinity

let test4 =
  get_ekikan_kyori "茗荷谷" "後楽園"
    (inserts_ekikan Empty
       [
         {
           kiten = "茗荷谷";
           shuten = "後楽園";
           keiyu = "丸ノ内線";
           kyori = 1.8;
           jikan = 2;
         };
         {
           kiten = "淡路町";
           shuten = "大手町";
           keiyu = "丸ノ内線";
           kyori = 0.9;
           jikan = 2;
         };
       ])
(* = 1.8 *)

let test5 =
  get_ekikan_kyori "後楽園" "茗荷谷"
    (inserts_ekikan Empty
       [
         {
           kiten = "茗荷谷";
           shuten = "後楽園";
           keiyu = "丸ノ内線";
           kyori = 1.8;
           jikan = 2;
         };
         {
           kiten = "淡路町";
           shuten = "大手町";
           keiyu = "丸ノ内線";
           kyori = 0.9;
           jikan = 2;
         };
       ])
  = 1.8
