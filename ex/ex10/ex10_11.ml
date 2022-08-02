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
  | [] -> infinity
  | { kiten; shuten; keiyu; kyori; jikan } :: rest ->
      if
        (ekimei1 = kiten && ekimei2 = shuten)
        || (ekimei1 = shuten && ekimei2 = kiten)
      then kyori
      else get_ekikan_kyori ekimei1 ekimei2 rest

(* テスト *)
let test1 = get_ekikan_kyori "" "" [] = infinity

let test2 =
  get_ekikan_kyori "" "茗荷谷"
    [
      { kiten = "茗荷谷"; shuten = "後楽園"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 2 };
      { kiten = "淡路町"; shuten = "大手町"; keiyu = "丸ノ内線"; kyori = 0.9; jikan = 2 };
    ]
  = infinity

let test3 =
  get_ekikan_kyori "茗荷谷" ""
    [
      { kiten = "茗荷谷"; shuten = "後楽園"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 2 };
      { kiten = "淡路町"; shuten = "大手町"; keiyu = "丸ノ内線"; kyori = 0.9; jikan = 2 };
    ]
  = infinity

let test4 =
  get_ekikan_kyori "茗荷谷" "後楽園"
    [
      { kiten = "茗荷谷"; shuten = "後楽園"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 2 };
      { kiten = "淡路町"; shuten = "大手町"; keiyu = "丸ノ内線"; kyori = 0.9; jikan = 2 };
    ]
  = 1.8

let test5 =
  get_ekikan_kyori "後楽園" "茗荷谷"
    [
      { kiten = "茗荷谷"; shuten = "後楽園"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 2 };
      { kiten = "淡路町"; shuten = "大手町"; keiyu = "丸ノ内線"; kyori = 0.9; jikan = 2 };
    ]
  = 1.8
