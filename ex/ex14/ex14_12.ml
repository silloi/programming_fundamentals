(* 駅名の情報 *)
type ekimei_t = {
  kanji : string; (* 漢字の駅名 *)
  kana : string; (* ひらがなの駅名 *)
  romaji : string; (* ローマ字の駅名 *)
  shozoku : string; (* 所属する路線名 *)
}

(* 駅名、最短距離、手前の駅名のリスト *)
type eki_t = {
  namae : string; (* 駅名（漢字の文字列） *)
  saitan_kyori : float; (* 最短距離（実数） *)
  temae_list : string list; (* 駅名（漢字の文字列）のリスト *)
}

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
  List.map
    (fun eki ->
      match eki with
      | { namae = n; saitan_kyori = s; temae_list = t } ->
          if n = shiten then
            { namae = n; saitan_kyori = 0.0; temae_list = [ n ] }
          else eki)
    lst

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
