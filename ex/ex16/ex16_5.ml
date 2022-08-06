(* 駅名の情報 *)
type ekimei_t = {
  kanji : string; (* 漢字の駅名 *)
  kana : string; (* ひらがなの駅名 *)
  romaji : string; (* ローマ字の駅名 *)
  shozoku : string; (* 所属する路線名 *)
}

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

let global_ekimei_list =
  [
    { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線" };
    { kanji = "新大塚"; kana = "しんおおつか"; romaji = "shinotsuka"; shozoku = "丸ノ内線" };
    { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
    { kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "丸ノ内線" };
    { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "有楽町線" };
    { kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "南北線" };
  ]

(* メトロネットワーク中のすべての駅間からなるリスト *)
let global_ekikan_list =
  [
    { kiten = "茗荷谷"; shuten = "後楽園"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 2 };
    { kiten = "茗荷谷"; shuten = "後楽園"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 2 };
    { kiten = "新大塚"; shuten = "茗荷谷"; keiyu = "丸ノ内線"; kyori = 1.2; jikan = 2 };
    { kiten = "池袋"; shuten = "新大塚"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 3 };
  ]

(* 目的：ひらがな順の駅名のリスト lst に 駅名 ekimei を挿入したリストを返す *)
(* insert ekimei_t list -> ekimei_t -> ekimei_t list *)
let rec ekimei_insert lst ekimei =
  match lst with
  | [] -> [ ekimei ]
  | ({
       kanji = first_kanji;
       kana = first_kana;
       romaji = first_r;
       shozoku = first_s;
     } as first)
    :: rest ->
      let {
        kanji = ekimei_kanji;
        kana = ekimei_kana;
        romaji = ekimei_r;
        shozoku = ekimei_s;
      } =
        ekimei
      in
      if first_kana < ekimei_kana then first :: ekimei_insert rest ekimei
      else ekimei :: first :: rest

(* 目的：駅名のリスト lst をひらがな順に整列して返す *)
(* ekimei_sort : ekimei_t list -> ekimei_t list *)
let rec ekimei_ins_sort lst =
  match lst with
  | [] -> []
  | first :: rest -> ekimei_insert (ekimei_ins_sort rest) first

(* ekimei_t のリストをひらがなの列に整列し、さらに駅の重複を取り除く *)
(* seiretsu : ekimei_t list -> ekimei_t list *)
let rec seiretsu lst =
  let lst_ins_sort = ekimei_ins_sort lst in
  match lst_ins_sort with
  | [] -> []
  | [ first ] -> [ first ]
  | ({
       kanji = first_kanji;
       kana = first_kana;
       romaji = first_r;
       shozoku = first_s;
     } as first)
    :: ({
          kanji = second_kanji;
          kana = second_kana;
          romaji = second_r;
          shozoku = second_s;
        } as second)
    :: rest ->
      if first_kana = second_kana then first :: seiretsu rest
      else first :: seiretsu (second :: rest)

(* 目的：ローマ字の駅名と駅名リストを受け取ったら駅の漢字表記を文字列で返す *)
(* romaji_to_kanji : string -> ekimei_t list -> string *)
let rec romaji_to_kanji ekimei lst1 =
  match lst1 with
  | [] -> ""
  | { kanji; kana; romaji = r; shozoku = s } :: rest ->
      if r = ekimei then kanji else romaji_to_kanji ekimei rest

(* 目的：ekimei_t 型のリスト lst から eki_t 型のリストを作る *)
(* make_eki_list : ekimei_t list -> eki_t list *)
let make_eki_list lst =
  List.map
    (fun ekimei ->
      match ekimei with
      | { kanji; kana; romaji = r; shozoku = s } ->
          { namae = kanji; saitan_kyori = infinity; temae_list = [] })
    lst

(* 目的：eki_t 型のリスト lst のうち始点 shiten のみ saitan_kyori が 0、temae_list は始点の駅名のみからなるリストにして初期化する *)
(* let shokika : eki_t list -> string -> eki_t list *)
let shokika lst shiten =
  List.map (fun eki ->
      match eki with
      | { namae = n; saitan_kyori = s; temae_list = t } ->
          if n = shiten then
            { namae = n; saitan_kyori = 0.0; temae_list = [ n ] }
          else eki)

(* make_eki_list と shokika を一度にやってしまう *)
(* make_initial_eki_list -> ekimei_t list -> string -> eki_t list *)
let make_initial_eki_list lst shiten =
  List.map
    (fun ekimei ->
      match ekimei with
      | { kanji; kana; romaji = r; shozoku = s } ->
          if kanji = shiten then
            { namae = kanji; saitan_kyori = 0.0; temae_list = [ kanji ] }
          else { namae = kanji; saitan_kyori = infinity; temae_list = [] })
    lst

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

(* 直前に確定した駅 p と未確定の駅のリスト v を受け取ったら、必要な更新処理を行った後の未確定の駅のリストを返す *)
(* koushin : eki_t -> eki_t list -> ekikan_t list -> eki_t list *)
let koushin p v ekikan_lst =
  List.map
    (fun q ->
      match (p, q) with
      | ( { namae = p_n; saitan_kyori = p_s; temae_list = p_t },
          { namae = q_n; saitan_kyori = q_s; temae_list = q_t } ) ->
          let ekikan_kyori = get_ekikan_kyori p_n q_n ekikan_lst in
          if ekikan_kyori = infinity then q
          else if p_s +. ekikan_kyori < q_s then
            {
              namae = q_n;
              saitan_kyori = p_s +. ekikan_kyori;
              temae_list = q_n :: p_t;
            }
          else q)
    v

(* 駅のリストを「最短距離最小の駅」と「最短距離最小の駅以外からなるリスト」に分離する *)
(* saitan_wo_bunri : eki_t list -> eki_t * eki_t list *)
let saitan_wo_bunri lst =
  match lst with
  | [] -> ({ namae = ""; saitan_kyori = infinity; temae_list = [ "" ] }, [])
  | first :: rest ->
      List.fold_right
        (fun first (p, v) ->
          match (first, p) with
          | ( { namae = first_n; saitan_kyori = first_s; temae_list = first_t },
              {
                namae = second_n;
                saitan_kyori = second_s;
                temae_list = second_t;
              } ) ->
              if first_s < second_s then (first, p :: v) else (p, first :: v))
        rest (first, [])

(* 未確定の駅のリスト eki_lst と駅間のリスト ekikan_lst からダイクストラのアルゴリズムにしたがって各駅について最短距離と最短経路が正しく入ったリストを返す *)
(* dijkstra_main : eki_t list -> ekikan_t list -> eki_t list *)
let rec dijkstra_main eki_lst ekikan_lst =
  match eki_lst with
  | [] -> []
  | first :: rest ->
      let saitan, saitan_igai_lst = saitan_wo_bunri (first :: rest) in
      let eki_lst2 = koushin saitan saitan_igai_lst ekikan_lst in
      saitan :: dijkstra_main eki_lst2 ekikan_lst

(* 始点の駅 shiten から終点の駅 shuten までの最短路を求める *)
(* dijkstra : string -> string -> eki_t *)
let dijkstra shiten shuten =
  let seiretsu_result = seiretsu global_ekimei_list in
  let shiten_kanji, shuten_kanji =
    ( romaji_to_kanji shiten global_ekimei_list,
      romaji_to_kanji shuten global_ekimei_list )
  in
  let initial_eki_list = make_initial_eki_list seiretsu_result shiten_kanji in
  let dijkstra_result = dijkstra_main initial_eki_list global_ekikan_list in
  (* 終点の駅名に一致する駅を見つける *)
  (* find_shuten : eki_t list -> string -> eki_t *)
  let rec find_shuten lst shuten =
    match lst with
    | [] -> { namae = ""; saitan_kyori = infinity; temae_list = [] }
    | ({ namae = n; saitan_kyori = s; temae_list = t } as first) :: rest ->
        if n = shuten_kanji then first else find_shuten rest shuten
  in
  find_shuten dijkstra_result shuten

(* テスト *)
let test1 =
  dijkstra "myogadani" "myogadani"
  = { namae = "茗荷谷"; saitan_kyori = 0.0; temae_list = [ "茗荷谷" ] }

let test2 =
  dijkstra "myogadani" "korakuen"
  = { namae = "後楽園"; saitan_kyori = 1.8; temae_list = [ "後楽園"; "茗荷谷" ] }

let test3 =
  dijkstra "myogadani" "shinotsuka"
  = { namae = "新大塚"; saitan_kyori = 1.2; temae_list = [ "新大塚"; "茗荷谷" ] }

let test4 =
  dijkstra "myogadani" "ikebukuro"
  = { namae = "池袋"; saitan_kyori = 3.0; temae_list = [ "池袋"; "新大塚"; "茗荷谷" ] }
