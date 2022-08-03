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
let rec make_eki_list lst =
  match lst with
  | [] -> []
  | { kanji; kana; romaji = r; shozoku = s } :: rest ->
      { namae = kanji; saitan_kyori = infinity; temae_list = [] }
      :: make_eki_list rest

(* テスト *)
let test1 = make_eki_list [] = []

let test2 =
  make_eki_list
    [
      { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
    ]
  = [ { namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] } ]

let test3 =
  make_eki_list
    [
      { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
      { kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "丸ノ内線" };
    ]
  = [
      { namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] };
      { namae = "後楽園"; saitan_kyori = infinity; temae_list = [] };
    ]
