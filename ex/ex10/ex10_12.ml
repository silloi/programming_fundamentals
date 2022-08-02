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

(* 目的：ローマ字の駅名と駅名リストを受け取ったら駅の漢字表記を文字列で返す *)
(* romaji_to_kanji : string -> ekimei_t list -> string *)
let rec romaji_to_kanji ekimei lst1 =
  match lst1 with
  | [] -> ""
  | { kanji; kana; romaji = r; shozoku = s } :: rest ->
      if r = ekimei then kanji else romaji_to_kanji ekimei rest

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

(* 目的：ローマ字の駅名 ekimei1 と ekimei2 の間の距離を表示する *)
(* kyori_wo_hyoji : string -> string -> ekimei_t list -> ekikan_t list -> string *)
let rec kyori_wo_hyoji ekimei1 ekimei2 lst1 lst2 =
  let kanji_ekimei1 = romaji_to_kanji ekimei1 lst1 in
  let kanji_ekimei2 = romaji_to_kanji ekimei2 lst1 in
  if kanji_ekimei1 = "" then ekimei1 ^ "という駅は存在しません"
  else if kanji_ekimei2 = "" then ekimei2 ^ "という駅は存在しません"
  else
    let kyori = get_ekikan_kyori kanji_ekimei1 kanji_ekimei2 lst2 in
    if kyori = infinity then
      kanji_ekimei1 ^ "駅と" ^ kanji_ekimei2 ^ "駅はつながっていません"
    else
      kanji_ekimei1 ^ "駅から" ^ kanji_ekimei2 ^ "駅までは " ^ string_of_float kyori
      ^ " km です"

(* テスト *)
let test1 = kyori_wo_hyoji "" "" [] [] = "という駅は存在しません"

let test2 =
  kyori_wo_hyoji "manseibashi" "myogadani"
    [
      { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
      { kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "丸ノ内線" };
      { kanji = "東京"; kana = "とうきょう"; romaji = "tokyo"; shozoku = "丸ノ内線" };
    ]
    [
      { kiten = "茗荷谷"; shuten = "後楽園"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 2 };
    ]
  = "manseibashiという駅は存在しません"

let test3 =
  kyori_wo_hyoji "myogadani" "manseibashi"
    [
      { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
      { kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "丸ノ内線" };
      { kanji = "東京"; kana = "とうきょう"; romaji = "tokyo"; shozoku = "丸ノ内線" };
    ]
    [
      { kiten = "茗荷谷"; shuten = "後楽園"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 2 };
    ]
  = "manseibashiという駅は存在しません"

let test4 =
  kyori_wo_hyoji "myogadani" "tokyo"
    [
      { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
      { kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "丸ノ内線" };
      { kanji = "東京"; kana = "とうきょう"; romaji = "tokyo"; shozoku = "丸ノ内線" };
    ]
    [
      { kiten = "茗荷谷"; shuten = "後楽園"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 2 };
    ]
  = "茗荷谷駅と東京駅はつながっていません"

let test5 =
  kyori_wo_hyoji "myogadani" "korakuen"
    [
      { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
      { kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "丸ノ内線" };
      { kanji = "東京"; kana = "とうきょう"; romaji = "tokyo"; shozoku = "丸ノ内線" };
    ]
    [
      { kiten = "茗荷谷"; shuten = "後楽園"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 2 };
    ]
  = "茗荷谷駅から後楽園駅までは 1.8 km です"
