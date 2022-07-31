(* 駅名の情報 *)
type ekimei_t = {
  kanji : string; (* 漢字の駅名 *)
  kana : string; (* ひらがなの駅名 *)
  romaji : string; (* ローマ字の駅名 *)
  shozoku : string; (* 所属する路線名 *)
}

(* 目的：ekimei_t 型のデータを受け取ったら「路線名，駅名（かな）」の形式の文字列を返す *)
(* hyoji : ekimei_t -> string *)
let hyoji ekimei =
  match ekimei with
  | { kanji; kana; romaji = r; shozoku = s } ->
      s ^ "，" ^ kanji ^ "（" ^ kana ^ "）"

(* テスト *)
let test1 =
  hyoji
    { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" }
  = "丸ノ内線，茗荷谷（みょうがだに）"

let test2 =
  hyoji { kanji = "赤坂"; kana = "あかさか"; romaji = "akasaka"; shozoku = "千代田線" }
  = "千代田線，赤坂（あかさか）"

let test3 =
  hyoji
    { kanji = "神保町"; kana = "じんぼうちょう"; romaji = "jinbocho"; shozoku = "半蔵門線" }
  = "半蔵門線，神保町（じんぼうちょう）"
