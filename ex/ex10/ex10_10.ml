(* 駅名の情報 *)
type ekimei_t = {
  kanji : string; (* 漢字の駅名 *)
  kana : string; (* ひらがなの駅名 *)
  romaji : string; (* ローマ字の駅名 *)
  shozoku : string; (* 所属する路線名 *)
}

(* 目的：ローマ字の駅名と駅名リストを受け取ったら駅の漢字表記を文字列で返す *)
(* romaji_to_kanji : string -> ekimei_t list -> string *)
let rec romaji_to_kanji ekimei lst1 =
  match lst1 with
  | [] -> ""
  | { kanji; kana; romaji = r; shozoku = s } :: rest ->
      if r = ekimei then kanji else romaji_to_kanji ekimei rest

(* テスト *)
let test1 = romaji_to_kanji "myogadani" [] = ""

let test2 =
  romaji_to_kanji "myogadani"
    [
      { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
    ]
  = "茗荷谷"

let test3 =
  romaji_to_kanji "korakuen"
    [
      { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
      { kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "丸ノ内線" };
    ]
  = "後楽園"
