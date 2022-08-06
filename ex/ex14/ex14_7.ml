(* 駅と駅の接続情報 *)
type ekikan_t = {
  kiten : string; (* 起点の駅名 *)
  shuten : string; (* 終点の駅名 *)
  keiyu : string; (* 経由する路線名 *)
  kyori : float; (* 2駅間の距離（km、実数） *)
  jikan : int; (* 所要時間（分、整数） *)
}

(* メトロネットワーク中のすべての駅間からなるリスト *)
let global_ekikan_list =
  [
    { kiten = "茗荷谷"; shuten = "後楽園"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 2 };
    { kiten = "新大塚"; shuten = "茗荷谷"; keiyu = "丸ノ内線"; kyori = 1.2; jikan = 2 };
    { kiten = "池袋"; shuten = "新大塚"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 3 };
  ]

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

(* 駅名、最短距離、手前の駅名のリスト *)
type eki_t = {
  namae : string; (* 駅名（漢字の文字列） *)
  saitan_kyori : float; (* 最短距離（実数） *)
  temae_list : string list; (* 駅名（漢字の文字列）のリスト *)
}

(* 直前に確定した駅 p と未確定の駅のリスト v を受け取ったら、必要な更新処理を行った後の未確定の駅のリストを返す *)
(* koushin : eki_t -> eki_t list -> eki_t list *)
let koushin p v =
  (* 直前に確定した駅 p と未確定の駅 q が直接つながっていたら最短距離と手前リストを必要に応じて更新し、でなければ q をそのまま返す *)
  (* koushin1 : eki_t -> eki_t -> eki_t *)
  let koushin1 p q =
    match (p, q) with
    | ( { namae = p_n; saitan_kyori = p_s; temae_list = p_t },
        { namae = q_n; saitan_kyori = q_s; temae_list = q_t } ) ->
        let ekikan_kyori = get_ekikan_kyori p_n q_n global_ekikan_list in
        if ekikan_kyori = infinity then q
        else if p_s +. ekikan_kyori < q_s then
          {
            namae = q_n;
            saitan_kyori = p_s +. ekikan_kyori;
            temae_list = q_n :: p_t;
          }
        else q
  in
  List.map (koushin1 p) v
