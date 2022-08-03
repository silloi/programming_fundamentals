(* 駅名の情報 *)
type ekimei_t = {
  kanji : string; (* 漢字の駅名 *)
  kana : string; (* ひらがなの駅名 *)
  romaji : string; (* ローマ字の駅名 *)
  shozoku : string; (* 所属する路線名 *)
}

(* 目的：ひらがな順の駅名のリスト lst に 駅名 ekimei を挿入したリストを返す *)
(* insert ekimei_t list -> ekimei -> ekimei_t list *)
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

(* テスト *)
let test1 =
  seiretsu
    [
      { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
    ]
  = [
      { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
    ]

let test2 =
  seiretsu
    [
      { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線" };
      { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
    ]
  = [
      { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線" };
      { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
    ]

let test3 =
  seiretsu
    [
      { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
      { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線" };
    ]
  = [
      { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線" };
      { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
    ]

let test4 =
  seiretsu
    [
      { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
      { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線" };
      { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "有楽町線" };
    ]
  = [
      { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線" };
      { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
    ]
