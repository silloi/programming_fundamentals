(* 駅名の情報 *)
type ekimei_t = {
  kanji : string; (* 漢字の駅名 *)
  kana : string; (* ひらがなの駅名 *)
  romaji : string; (* ローマ字の駅名 *)
  shozoku : string; (* 所属する路線名 *)
}

(* 駅が存在しないことを示す例外 *)
exception No_such_station of string

(* 目的：ローマ字の駅名と駅名リストを受け取ったら駅の漢字表記を文字列で返す *)
(* みつからないときには No_such_station という例外を発生する *)
(* romaji_to_kanji : string -> ekimei_t list -> string *)
let rec romaji_to_kanji ekimei lst1 =
  match lst1 with
  | [] -> raise (No_such_station ekimei)
  | { kanji; kana; romaji = r; shozoku = s } :: rest ->
      if r = ekimei then kanji else romaji_to_kanji ekimei rest
