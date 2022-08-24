(* 2 分探索木を現すモジュール *)
module RedBlack : sig
  type ('a, 'b) t
  (* キーが 'a 型、値が 'b 型の木の型。型の中身は非公開 *)

  val empty : ('a, 'b) t
  (* 使い方：empty *)
  (* 空の木 *)

  val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  (* 使い方：insert tree key value *)
  (* 木 tree にキー key と値 value を挿入した木を返す *)
  (* キーがすでに存在していたら新しい値に置き換える *)

  val search : ('a, 'b) t -> 'a -> 'b
  (* 使い方：search tree key *)
  (* 木 tree の中からキー key に対応する値を探して返す *)
  (* みつからなければ Not_found を raise する *)

  val traverse : ('a -> 'b -> 'c -> 'a) -> 'a -> ('b, 'c) t -> 'a
  (* 使い方：traverse f init tree *)
  (* 全てのノードを深さ優先で訪れる *)
  (* 初期値 init から始めて、各ノードで関数 f を順に適用する *)

  val length : ('a, 'b) t -> int
  (* 使い方：length tree *)
  (* 木の中にあるノードの数を求める *)
end = struct
  (* 赤黒木を現すモジュール *)

  (* 赤か黒かを示す型 *)
  type color_t = Red | Black

  (* 赤黒木を表す型 *)
  type ('a, 'b) t =
    | Empty (* 空の木 *)
    | Node of ('a, 'b) t * 'a * 'b * color_t * ('a, 'b) t

  (* 空の木 *)
  let empty = Empty

  (* 木 rb_tree を受け取ったら赤黒木になるようバランスする *)
  (* balance : ('a, 'b) RedBlack.t -> ('a, 'b) RedBlack.t *)
  let balance rb_tree =
    match rb_tree with
    | Empty -> Empty
    | Node (Node (Node (a, xk, xv, Red, b), yk, yv, Red, c), zk, zv, Black, d)
    | Node (Node (a, xk, xv, Red, Node (b, yk, yv, Red, c)), zk, zv, Black, d)
    | Node (a, xk, xv, Black, Node (Node (b, yk, yv, Red, c), zk, zv, Red, d))
    | Node (a, xk, xv, Black, Node (b, yk, yv, Red, Node (c, zk, zv, Red, d)))
      ->
        Node
          (Node (a, xk, xv, Black, b), yk, yv, Red, Node (c, zk, zv, Black, d))
    | _ -> rb_tree

  (* 赤黒木 tree にキー key と値 value を挿入した赤黒木を返す *)
  (* insert : ('a, 'b) rb_tree -> 'a -> 'b -> ('a, 'b) rb_tree *)
  let insert rb_tree key value =
    let rec ins rb_tree =
      match rb_tree with
      | Empty -> Node (Empty, key, value, Red, Empty)
      | Node (left, k, v, color, right) ->
          if key = k then Node (left, k, value, color, right)
          else if key < k then balance (Node (ins left, k, v, color, right))
          else balance (Node (left, k, v, color, ins right))
    in
    match ins rb_tree with
    | Empty -> assert false (* 絶対に空ではない *)
    | Node (left, k, v, color, right) -> Node (left, k, v, Black, right)

  (* 目的：rb_tree の中のキー k に対応する値を探して返す *)
  (* みつからなければ例外 Not_found を起こす *)
  (* search : ('a, 'b) RedBlack.t -> 'a -> 'b *)
  let rec search rb_tree k =
    match rb_tree with
    | Empty -> raise Not_found
    | Node (left, key, value, color, right) ->
        if k = key then value
        else if k < key then search left k
        else search right k

  (* 目的：全てのノードを深さ優先で訪れる *)
  (* 初期値 init から始めて、各ノードで関数 f を順に適用する *)
  (* ex23_3.ml で使用 *)
  (* traverse : ('a -> 'b -> 'c -> 'a) -> 'a -> ('b, 'c) t -> 'a *)
  let rec traverse f init tree =
    match tree with
    | Empty -> init
    | Node (left, key, value, _, right) ->
        let result1 = f init key value in
        let result2 = traverse f result1 left in
        let result3 = traverse f result2 right in
        result3

  (* 目的：木の中にあるノードの数を求める *)
  (* ex23_3.ml で使用 *)
  (* length : ('a, 'b) t -> int *)
  let rec length tree =
    match tree with
    | Empty -> 0
    | Node (left, key, value, _, right) -> length left + 1 + length right
end

open RedBlack

module Heap : sig
  type index_t
  (* ヒープの添字の型 *)

  type ('a, 'b) t
  (* 最小値を求める値が 'a 型で
     そのほかの付加情報が 'b 型であるヒープの型 *)

  val create : int -> 'a -> 'b -> ('a, 'b) t
  (* 使い方：create size key value *)
  (* ヒープのサイズと 'a 型と 'b 型のダミーの値を受け取ったら *)
  (* 空のヒープを返す *)

  val insert : ('a, 'b) t -> 'a -> 'b -> index_t * ('a, 'b) t
  (* 使い方：insert heap key value *)
  (* ヒープに新しい要素を追加する *)
  (* ヒープは（破壊的に）書き変わる *)

  val get : ('a, 'b) t -> index_t -> 'a * 'b
  (* 使い方：get heap index *)
  (* ヒープの index 番目の要素を返す *)

  val set : ('a, 'b) t -> index_t -> 'a -> 'b -> ('a, 'b) t
  (* 使い方：get heap index *)
  (* ヒープの index 番目の値を更新したヒープを返す *)
  (* ヒープは（破壊的に）書き変わる *)

  val split_top : ('a, 'b) t -> ('a * 'b) * ('a, 'b) t
  (* 使い方：split_top heap *)
  (* 最小の値を持つものとそれを取り除いたヒープのペアを返す *)
  (* ヒープが空のときは Empty を raise する *)
  (* 最小の値を持つものの index は無効な値になる *)
  (* ヒープは（破壊的に）書き変わる *)

  val length : ('a, 'b) t -> int
  (* 使い方：length heap *)
  (* ヒープ中のデータの数を返す *)
end = struct
  (* ヒープの添字の型 *)
  type index_t = int ref

  (* 最小値を求める値が 'a 型で
     そのほかの付加情報が 'b 型であるヒープの型 *)
  type ('a, 'b) t = int ref * (index_t * 'a * 'b) array

  (* insert したときにヒープが一杯だと raise される例外 *)
  exception Full

  (* split_top したときにヒープが空だと raise される例外 *)
  exception Empty

  (* index_t 型を持つダミーの値 *)
  let example_index = ref (-1)

  (* ヒープのサイズと 'a 型と 'b 型のダミーの値を受け取ったら空のヒープを返す *)
  (* create : int -> 'a -> 'b -> ('a, 'b) t *)
  let create size a b = (ref 0, Array.init size (fun _ -> (example_index, a, b)))

  (* current_index と parent_index の要素を入れ換える *)
  let swap array current_index parent_index =
    let ((index_ref_c, value_c, info_c) as entry_c) = array.(current_index) in
    let ((index_ref_p, value_p, info_p) as entry_p) = array.(parent_index) in
    array.(current_index) <- entry_p;
    array.(parent_index) <- entry_c;
    index_ref_c := parent_index;
    (* 入れ換えにともなって index も付け変える *)
    index_ref_p := current_index;
    ()

  (* 下方向に向かってヒープの条件を満たすように要素の入れ換えを行う *)
  let rec adjust_child num array current_index =
    if current_index >= num then ()
    else
      let _, v, _ = array.(current_index) in
      let child1_index = (2 * current_index) + 1 in
      let child2_index = child1_index + 1 in
      if child1_index >= num then ()
      else
        let _, v1, _ = array.(child1_index) in
        if child2_index >= num then
          if v <= v1 then ()
          else (
            swap array current_index child1_index;
            adjust_child num array child1_index)
        else
          let _, v2, _ = array.(child2_index) in
          if v <= v1 && v <= v2 then ()
          else if v1 < v2 then (
            swap array current_index child1_index;
            adjust_child num array child1_index)
          else (
            swap array current_index child2_index;
            adjust_child num array child2_index)

  (* 上方向に向かってヒープの条件を満たすように要素の入れ換えを行う *)
  let rec adjust_parent array current_index =
    if current_index = 0 then ()
    else
      let _, value_c, _ = array.(current_index) in
      let parent_index = (current_index - 1) / 2 in
      let _, value_p, _ = array.(parent_index) in
      if value_c < value_p then (
        swap array current_index parent_index;
        adjust_parent array parent_index)
      else ()

  (* ヒープに新しい要素を追加する *)
  (* これ以上、入らないときは Full を raise する *)
  (* ヒープは（破壊的に）書き変わる *)
  (* insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t *)
  let insert (num_ref, array) v info =
    if !num_ref >= Array.length array then raise Full
    else
      let index = ref !num_ref in
      array.(!num_ref) <- (index, v, info);
      adjust_parent array !num_ref;
      num_ref := !num_ref + 1;
      (index, (num_ref, array))

  (* ヒープの !index_ref 番目の要素を返す *)
  (* index が無効であれば Not_found を raise する *)
  (* get : ('a, 'b) t -> index_t -> 'a * 'b *)
  let get (num_ref, array) index_ref =
    if 0 <= !index_ref && !index_ref < !num_ref then
      let _, a, b = array.(!index_ref) in
      (a, b)
    else raise Not_found

  (* ヒープの !index_ref 番目の値を更新したヒープを返す *)
  (* ヒープは（破壊的に）書き変わる *)
  (* set : ('a, 'b) t -> index_t -> 'a -> 'b -> ('a, 'b) t *)
  let set (num_ref, array) index_ref v info =
    let _, v', _ = array.(!index_ref) in
    array.(!index_ref) <- (index_ref, v, info);
    if v < v' then adjust_parent array !index_ref
    else adjust_child !num_ref array !index_ref;
    (num_ref, array)

  (* 最小の値を持つものとそれを取り除いたヒープの組を返す *)
  (* 最小の値を持つものの index は無効な値になる *)
  (* ヒープが空のときは Empty を raise する *)
  (* ヒープは（破壊的に）書き変わる *)
  (* split_top : ('a, 'b) t -> ('a * 'b) * ('a, 'b) t *)
  let split_top (num_ref, array) =
    if !num_ref = 0 then raise Empty
    else
      let index_ref, v, info = array.(0) in
      num_ref := !num_ref - 1;
      (* 要素数をひとつ減らす *)
      array.(0) <- array.(!num_ref);
      adjust_child !num_ref array 0;
      index_ref := -1;
      (* 取り出した先頭の要素の index_ref は無効にする *)
      ((v, info), (num_ref, array))

  (* ヒープ中のデータの数を返す *)
  let length (num_ref, _) = !num_ref
end

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

(* ekimei_t list は
     - []              空リスト、あるいは
     - first :: rest   最初の要素が first で残りのリストが rest
                       （first は ekimei_t 型、
                         rest が自己参照のケース）
   という形 *)

(* メトロネットワーク中のすべての駅名からなるリスト *)
let global_ekimei_list =
  [
    {
      kanji = "代々木上原";
      kana = "よよぎうえはら";
      romaji = "yoyogiuehara";
      shozoku = "千代田線";
    };
    {
      kanji = "代々木公園";
      kana = "よよぎこうえん";
      romaji = "yoyogikouen";
      shozoku = "千代田線";
    };
    {
      kanji = "明治神宮前";
      kana = "めいじじんぐうまえ";
      romaji = "meijijinguumae";
      shozoku = "千代田線";
    };
    {
      kanji = "表参道";
      kana = "おもてさんどう";
      romaji = "omotesandou";
      shozoku = "千代田線";
    };
    { kanji = "乃木坂"; kana = "のぎざか"; romaji = "nogizaka"; shozoku = "千代田線" };
    { kanji = "赤坂"; kana = "あかさか"; romaji = "akasaka"; shozoku = "千代田線" };
    {
      kanji = "国会議事堂前";
      kana = "こっかいぎじどうまえ";
      romaji = "kokkaigijidoumae";
      shozoku = "千代田線";
    };
    {
      kanji = "霞ヶ関";
      kana = "かすみがせき";
      romaji = "kasumigaseki";
      shozoku = "千代田線";
    };
    { kanji = "日比谷"; kana = "ひびや"; romaji = "hibiya"; shozoku = "千代田線" };
    {
      kanji = "二重橋前";
      kana = "にじゅうばしまえ";
      romaji = "nijuubasimae";
      shozoku = "千代田線";
    };
    { kanji = "大手町"; kana = "おおてまち"; romaji = "otemachi"; shozoku = "千代田線" };
    {
      kanji = "新御茶ノ水";
      kana = "しんおちゃのみず";
      romaji = "shin-ochanomizu";
      shozoku = "千代田線";
    };
    { kanji = "湯島"; kana = "ゆしま"; romaji = "yushima"; shozoku = "千代田線" };
    { kanji = "根津"; kana = "ねづ"; romaji = "nedu"; shozoku = "千代田線" };
    { kanji = "千駄木"; kana = "せんだぎ"; romaji = "sendagi"; shozoku = "千代田線" };
    {
      kanji = "西日暮里";
      kana = "にしにっぽり";
      romaji = "nishinippori";
      shozoku = "千代田線";
    };
    { kanji = "町屋"; kana = "まちや"; romaji = "machiya"; shozoku = "千代田線" };
    { kanji = "北千住"; kana = "きたせんじゅ"; romaji = "kitasenjyu"; shozoku = "千代田線" };
    { kanji = "綾瀬"; kana = "あやせ"; romaji = "ayase"; shozoku = "千代田線" };
    { kanji = "北綾瀬"; kana = "きたあやせ"; romaji = "kitaayase"; shozoku = "千代田線" };
    { kanji = "浅草"; kana = "あさくさ"; romaji = "asakusa"; shozoku = "銀座線" };
    { kanji = "田原町"; kana = "たわらまち"; romaji = "tawaramachi"; shozoku = "銀座線" };
    { kanji = "稲荷町"; kana = "いなりちょう"; romaji = "inaricho"; shozoku = "銀座線" };
    { kanji = "上野"; kana = "うえの"; romaji = "ueno"; shozoku = "銀座線" };
    {
      kanji = "上野広小路";
      kana = "うえのひろこうじ";
      romaji = "uenohirokoji";
      shozoku = "銀座線";
    };
    { kanji = "末広町"; kana = "すえひろちょう"; romaji = "suehirocho"; shozoku = "銀座線" };
    { kanji = "神田"; kana = "かんだ"; romaji = "kanda"; shozoku = "銀座線" };
    {
      kanji = "三越前";
      kana = "みつこしまえ";
      romaji = "mitsukoshimae";
      shozoku = "銀座線";
    };
    { kanji = "日本橋"; kana = "にほんばし"; romaji = "nihonbashi"; shozoku = "銀座線" };
    { kanji = "京橋"; kana = "きょうばし"; romaji = "kyobashi"; shozoku = "銀座線" };
    { kanji = "銀座"; kana = "ぎんざ"; romaji = "ginza"; shozoku = "銀座線" };
    { kanji = "新橋"; kana = "しんばし"; romaji = "shinbashi"; shozoku = "銀座線" };
    { kanji = "虎ノ門"; kana = "とらのもん"; romaji = "toranomon"; shozoku = "銀座線" };
    {
      kanji = "溜池山王";
      kana = "ためいけさんのう";
      romaji = "tameikesannou";
      shozoku = "銀座線";
    };
    {
      kanji = "赤坂見附";
      kana = "あかさかみつけ";
      romaji = "akasakamitsuke";
      shozoku = "銀座線";
    };
    {
      kanji = "青山一丁目";
      kana = "あおやまいっちょうめ";
      romaji = "aoyamaicchome";
      shozoku = "銀座線";
    };
    { kanji = "外苑前"; kana = "がいえんまえ"; romaji = "gaienmae"; shozoku = "銀座線" };
    { kanji = "表参道"; kana = "おもてさんどう"; romaji = "omotesando"; shozoku = "銀座線" };
    { kanji = "渋谷"; kana = "しぶや"; romaji = "shibuya"; shozoku = "銀座線" };
    { kanji = "渋谷"; kana = "しぶや"; romaji = "shibuya"; shozoku = "半蔵門線" };
    {
      kanji = "表参道";
      kana = "おもてさんどう";
      romaji = "omotesandou";
      shozoku = "半蔵門線";
    };
    {
      kanji = "青山一丁目";
      kana = "あおやまいっちょうめ";
      romaji = "aoyama-itchome";
      shozoku = "半蔵門線";
    };
    { kanji = "永田町"; kana = "ながたちょう"; romaji = "nagatacho"; shozoku = "半蔵門線" };
    { kanji = "半蔵門"; kana = "はんぞうもん"; romaji = "hanzomon"; shozoku = "半蔵門線" };
    { kanji = "九段下"; kana = "くだんした"; romaji = "kudanshita"; shozoku = "半蔵門線" };
    { kanji = "神保町"; kana = "じんぼうちょう"; romaji = "jinbocho"; shozoku = "半蔵門線" };
    { kanji = "大手町"; kana = "おおてまち"; romaji = "otemachi"; shozoku = "半蔵門線" };
    {
      kanji = "三越前";
      kana = "みつこしまえ";
      romaji = "mitsukoshimae";
      shozoku = "半蔵門線";
    };
    {
      kanji = "水天宮前";
      kana = "すいてんぐうまえ";
      romaji = "suitengumae";
      shozoku = "半蔵門線";
    };
    {
      kanji = "清澄白河";
      kana = "きよすみしらかわ";
      romaji = "kiyosumi-shirakawa";
      shozoku = "半蔵門線";
    };
    { kanji = "住吉"; kana = "すみよし"; romaji = "sumiyoshi"; shozoku = "半蔵門線" };
    { kanji = "錦糸町"; kana = "きんしちょう"; romaji = "kinshicho"; shozoku = "半蔵門線" };
    { kanji = "押上"; kana = "おしあげ"; romaji = "oshiage"; shozoku = "半蔵門線" };
    { kanji = "中目黒"; kana = "なかめぐろ"; romaji = "nakameguro"; shozoku = "日比谷線" };
    { kanji = "恵比寿"; kana = "えびす"; romaji = "ebisu"; shozoku = "日比谷線" };
    { kanji = "広尾"; kana = "ひろお"; romaji = "hiro"; shozoku = "日比谷線" };
    { kanji = "六本木"; kana = "ろっぽんぎ"; romaji = "roppongi"; shozoku = "日比谷線" };
    { kanji = "神谷町"; kana = "かみやちょう"; romaji = "kamiyacho"; shozoku = "日比谷線" };
    {
      kanji = "霞ヶ関";
      kana = "かすみがせき";
      romaji = "kasumigaseki";
      shozoku = "日比谷線";
    };
    { kanji = "日比谷"; kana = "ひびや"; romaji = "hibiya"; shozoku = "日比谷線" };
    { kanji = "銀座"; kana = "ぎんざ"; romaji = "ginza"; shozoku = "日比谷線" };
    {
      kanji = "東銀座";
      kana = "ひがしぎんざ";
      romaji = "higashiginza";
      shozoku = "日比谷線";
    };
    { kanji = "築地"; kana = "つきじ"; romaji = "tsukiji"; shozoku = "日比谷線" };
    { kanji = "八丁堀"; kana = "はっちょうぼり"; romaji = "hacchobori"; shozoku = "日比谷線" };
    { kanji = "茅場町"; kana = "かやばちょう"; romaji = "kayabacho"; shozoku = "日比谷線" };
    {
      kanji = "人形町";
      kana = "にんぎょうちょう";
      romaji = "ningyomachi";
      shozoku = "日比谷線";
    };
    {
      kanji = "小伝馬町";
      kana = "こでんまちょう";
      romaji = "kodemmacho";
      shozoku = "日比谷線";
    };
    { kanji = "秋葉原"; kana = "あきはばら"; romaji = "akihabara"; shozoku = "日比谷線" };
    {
      kanji = "仲御徒町";
      kana = "なかおかちまち";
      romaji = "nakaokachimachi";
      shozoku = "日比谷線";
    };
    { kanji = "上野"; kana = "うえの"; romaji = "ueno"; shozoku = "日比谷線" };
    { kanji = "入谷"; kana = "いりや"; romaji = "iriya"; shozoku = "日比谷線" };
    { kanji = "三ノ輪"; kana = "みのわ"; romaji = "minowa"; shozoku = "日比谷線" };
    {
      kanji = "南千住";
      kana = "みなみせんじゅ";
      romaji = "minamisenju";
      shozoku = "日比谷線";
    };
    { kanji = "北千住"; kana = "きたせんじゅ"; romaji = "kitasenju"; shozoku = "日比谷線" };
    { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線" };
    { kanji = "新大塚"; kana = "しんおおつか"; romaji = "shinotsuka"; shozoku = "丸ノ内線" };
    { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" };
    { kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "丸ノ内線" };
    {
      kanji = "本郷三丁目";
      kana = "ほんごうさんちょうめ";
      romaji = "hongosanchome";
      shozoku = "丸ノ内線";
    };
    { kanji = "御茶ノ水"; kana = "おちゃのみず"; romaji = "ochanomizu"; shozoku = "丸ノ内線" };
    { kanji = "淡路町"; kana = "あわじちょう"; romaji = "awajicho"; shozoku = "丸ノ内線" };
    { kanji = "大手町"; kana = "おおてまち"; romaji = "otemachi"; shozoku = "丸ノ内線" };
    { kanji = "東京"; kana = "とうきょう"; romaji = "tokyo"; shozoku = "丸ノ内線" };
    { kanji = "銀座"; kana = "ぎんざ"; romaji = "ginza"; shozoku = "丸ノ内線" };
    {
      kanji = "霞ヶ関";
      kana = "かすみがせき";
      romaji = "kasumigaseki";
      shozoku = "丸ノ内線";
    };
    {
      kanji = "国会議事堂前";
      kana = "こっかいぎじどうまえ";
      romaji = "kokkaigijidomae";
      shozoku = "丸ノ内線";
    };
    {
      kanji = "赤坂見附";
      kana = "あかさかみつけ";
      romaji = "akasakamitsuke";
      shozoku = "丸ノ内線";
    };
    { kanji = "四ツ谷"; kana = "よつや"; romaji = "yotsuya"; shozoku = "丸ノ内線" };
    {
      kanji = "四谷三丁目";
      kana = "よつやさんちょうめ";
      romaji = "yotsuyasanchome";
      shozoku = "丸ノ内線";
    };
    {
      kanji = "新宿御苑前";
      kana = "しんじゅくぎょえんまえ";
      romaji = "shinjuku-gyoemmae";
      shozoku = "丸ノ内線";
    };
    {
      kanji = "新宿三丁目";
      kana = "しんじゅくさんちょうめ";
      romaji = "shinjuku-sanchome";
      shozoku = "丸ノ内線";
    };
    { kanji = "新宿"; kana = "しんじゅく"; romaji = "shinjuku"; shozoku = "丸ノ内線" };
    {
      kanji = "西新宿";
      kana = "にししんじゅく";
      romaji = "nishi-shinjuku";
      shozoku = "丸ノ内線";
    };
    {
      kanji = "中野坂上";
      kana = "なかのさかうえ";
      romaji = "nakano-sakaue";
      shozoku = "丸ノ内線";
    };
    { kanji = "新中野"; kana = "しんなかの"; romaji = "shin-nakano"; shozoku = "丸ノ内線" };
    {
      kanji = "東高円寺";
      kana = "ひがしこうえんじ";
      romaji = "higashi-koenji";
      shozoku = "丸ノ内線";
    };
    {
      kanji = "新高円寺";
      kana = "しんこうえんじ";
      romaji = "shin-koenji";
      shozoku = "丸ノ内線";
    };
    {
      kanji = "南阿佐ヶ谷";
      kana = "みなみあさがや";
      romaji = "minami-asagaya";
      shozoku = "丸ノ内線";
    };
    { kanji = "荻窪"; kana = "おぎくぼ"; romaji = "ogikubo"; shozoku = "丸ノ内線" };
    {
      kanji = "中野新橋";
      kana = "なかのしんばし";
      romaji = "nakano-shimbashi";
      shozoku = "丸ノ内線";
    };
    {
      kanji = "中野富士見町";
      kana = "なかのふじみちょう";
      romaji = "nakano-fujimicho";
      shozoku = "丸ノ内線";
    };
    { kanji = "方南町"; kana = "ほうなんちょう"; romaji = "honancho"; shozoku = "丸ノ内線" };
    { kanji = "四ツ谷"; kana = "よつや"; romaji = "yotsuya"; shozoku = "南北線" };
    { kanji = "永田町"; kana = "ながたちょう"; romaji = "nagatacho"; shozoku = "南北線" };
    {
      kanji = "溜池山王";
      kana = "ためいけさんのう";
      romaji = "tameikesanno";
      shozoku = "南北線";
    };
    {
      kanji = "六本木一丁目";
      kana = "ろっぽんぎいっちょうめ";
      romaji = "roppongiitchome";
      shozoku = "南北線";
    };
    {
      kanji = "麻布十番";
      kana = "あざぶじゅうばん";
      romaji = "azabujuban";
      shozoku = "南北線";
    };
    {
      kanji = "白金高輪";
      kana = "しろかねたかなわ";
      romaji = "shirokanetakanawa";
      shozoku = "南北線";
    };
    { kanji = "白金台"; kana = "しろかねだい"; romaji = "shirokanedai"; shozoku = "南北線" };
    { kanji = "目黒"; kana = "めぐろ"; romaji = "meguro"; shozoku = "南北線" };
    { kanji = "市ヶ谷"; kana = "いちがや"; romaji = "ichigaya"; shozoku = "南北線" };
    { kanji = "飯田橋"; kana = "いいだばし"; romaji = "idabashi"; shozoku = "南北線" };
    { kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "南北線" };
    { kanji = "東大前"; kana = "とうだいまえ"; romaji = "todaimae"; shozoku = "南北線" };
    { kanji = "本駒込"; kana = "ほんこまごめ"; romaji = "honkomagome"; shozoku = "南北線" };
    { kanji = "駒込"; kana = "こまごめ"; romaji = "komagome"; shozoku = "南北線" };
    { kanji = "西ヶ原"; kana = "にしがはら"; romaji = "nishigahara"; shozoku = "南北線" };
    { kanji = "王子"; kana = "おうじ"; romaji = "oji"; shozoku = "南北線" };
    { kanji = "王子神谷"; kana = "おうじかみや"; romaji = "ojikamiya"; shozoku = "南北線" };
    { kanji = "志茂"; kana = "しも"; romaji = "shimo"; shozoku = "南北線" };
    {
      kanji = "赤羽岩淵";
      kana = "あかばねいわぶち";
      romaji = "akabaneiwabuchi";
      shozoku = "南北線";
    };
    {
      kanji = "西船橋";
      kana = "にしふなばし";
      romaji = "nishi-funabashi";
      shozoku = "東西線";
    };
    {
      kanji = "原木中山";
      kana = "ばらきなかやま";
      romaji = "baraki-nakayama";
      shozoku = "東西線";
    };
    { kanji = "妙典"; kana = "みょうでん"; romaji = "myoden"; shozoku = "東西線" };
    { kanji = "行徳"; kana = "ぎょうとく"; romaji = "gyotoku"; shozoku = "東西線" };
    {
      kanji = "南行徳";
      kana = "みなみぎょうとく";
      romaji = "minami-gyotoku";
      shozoku = "東西線";
    };
    { kanji = "浦安"; kana = "うらやす"; romaji = "urayasu"; shozoku = "東西線" };
    { kanji = "葛西"; kana = "かさい"; romaji = "kasai"; shozoku = "東西線" };
    { kanji = "西葛西"; kana = "にしかさい"; romaji = "nishi-kasai"; shozoku = "東西線" };
    {
      kanji = "南砂町";
      kana = "みなみすなまち";
      romaji = "minami-sunamachi";
      shozoku = "東西線";
    };
    { kanji = "東陽町"; kana = "とうようちょう"; romaji = "touyoucho"; shozoku = "東西線" };
    { kanji = "木場"; kana = "きば"; romaji = "kiba"; shozoku = "東西線" };
    {
      kanji = "門前仲町";
      kana = "もんぜんなかちょう";
      romaji = "monzen-nakacho";
      shozoku = "東西線";
    };
    { kanji = "茅場町"; kana = "かやばちょう"; romaji = "kayabacho"; shozoku = "東西線" };
    { kanji = "日本橋"; kana = "にほんばし"; romaji = "nihonbashi"; shozoku = "東西線" };
    { kanji = "大手町"; kana = "おおてまち"; romaji = "otemachi"; shozoku = "東西線" };
    { kanji = "竹橋"; kana = "たけばし"; romaji = "takebashi"; shozoku = "東西線" };
    { kanji = "九段下"; kana = "くだんした"; romaji = "kudanshita"; shozoku = "東西線" };
    { kanji = "飯田橋"; kana = "いいだばし"; romaji = "iidabashi"; shozoku = "東西線" };
    { kanji = "神楽坂"; kana = "かぐらざか"; romaji = "kagurazaka"; shozoku = "東西線" };
    { kanji = "早稲田"; kana = "わせだ"; romaji = "waseda"; shozoku = "東西線" };
    {
      kanji = "高田馬場";
      kana = "たかだのばば";
      romaji = "takadanobaba";
      shozoku = "東西線";
    };
    { kanji = "落合"; kana = "おちあい"; romaji = "ochiai"; shozoku = "東西線" };
    { kanji = "中野"; kana = "なかの"; romaji = "nakano"; shozoku = "東西線" };
    { romaji = "shinkiba"; kana = "しんきば"; kanji = "新木場"; shozoku = "有楽町線" };
    { romaji = "tatsumi"; kana = "たつみ"; kanji = "辰巳"; shozoku = "有楽町線" };
    { romaji = "toyosu"; kana = "とよす"; kanji = "豊洲"; shozoku = "有楽町線" };
    { romaji = "tsukishima"; kana = "つきしま"; kanji = "月島"; shozoku = "有楽町線" };
    {
      romaji = "shintomityou";
      kana = "しんとみちょう";
      kanji = "新富町";
      shozoku = "有楽町線";
    };
    {
      romaji = "ginzaittyoume";
      kana = "ぎんざいっちょうめ";
      kanji = "銀座一丁目";
      shozoku = "有楽町線";
    };
    {
      romaji = "yuurakutyou";
      kana = "ゆうらくちょう";
      kanji = "有楽町";
      shozoku = "有楽町線";
    };
    { romaji = "sakuradamon"; kana = "さくらだもん"; kanji = "桜田門"; shozoku = "有楽町線" };
    { romaji = "nagatacho"; kana = "ながたちょう"; kanji = "永田町"; shozoku = "有楽町線" };
    { romaji = "koujimachi"; kana = "こうじまち"; kanji = "麹町"; shozoku = "有楽町線" };
    { romaji = "ichigaya"; kana = "いちがや"; kanji = "市ヶ谷"; shozoku = "有楽町線" };
    { romaji = "iidabashi"; kana = "いいだばし"; kanji = "飯田橋"; shozoku = "有楽町線" };
    {
      kanji = "江戸川橋";
      kana = "えどがわばし";
      romaji = "edogawabasi";
      shozoku = "有楽町線";
    };
    { kanji = "護国寺"; kana = "ごこくじ"; romaji = "gokokuji"; shozoku = "有楽町線" };
    {
      kanji = "東池袋";
      kana = "ひがしいけぶくろ";
      romaji = "higasiikebukuro";
      shozoku = "有楽町線";
    };
    { kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "有楽町線" };
    { kanji = "要町"; kana = "かなめちょう"; romaji = "kanametyou"; shozoku = "有楽町線" };
    { kanji = "千川"; kana = "せんかわ"; romaji = "senkawa"; shozoku = "有楽町線" };
    {
      kanji = "小竹向原";
      kana = "こたけむかいはら";
      romaji = "kotakemukaihara";
      shozoku = "有楽町線";
    };
    { kanji = "氷川台"; kana = "ひかわだい"; romaji = "hikawadai"; shozoku = "有楽町線" };
    { kanji = "平和台"; kana = "へいわだい"; romaji = "heiwadai"; shozoku = "有楽町線" };
    {
      kanji = "営団赤塚";
      kana = "えいだんあかつか";
      romaji = "eidanakakuka";
      shozoku = "有楽町線";
    };
    {
      kanji = "営団成増";
      kana = "えいだんなります";
      romaji = "eidannarimasu";
      shozoku = "有楽町線";
    };
    { kanji = "和光市"; kana = "わこうし"; romaji = "wakousi"; shozoku = "有楽町線" };
  ]

(* ekikan_t list は
     - []              空リスト、あるいは
     - first :: rest   最初の要素が first で残りのリストが rest
                       （first は ekikan_t 型、
                         rest が自己参照のケース）
   という形 *)

(* メトロネットワーク中のすべての駅間からなるリスト *)
let global_ekikan_list =
  [
    {
      kiten = "代々木上原";
      shuten = "代々木公園";
      keiyu = "千代田線";
      kyori = 1.0;
      jikan = 2;
    };
    {
      kiten = "代々木公園";
      shuten = "明治神宮前";
      keiyu = "千代田線";
      kyori = 1.2;
      jikan = 2;
    };
    { kiten = "明治神宮前"; shuten = "表参道"; keiyu = "千代田線"; kyori = 0.9; jikan = 2 };
    { kiten = "表参道"; shuten = "乃木坂"; keiyu = "千代田線"; kyori = 1.4; jikan = 3 };
    { kiten = "乃木坂"; shuten = "赤坂"; keiyu = "千代田線"; kyori = 1.1; jikan = 2 };
    { kiten = "赤坂"; shuten = "国会議事堂前"; keiyu = "千代田線"; kyori = 0.8; jikan = 1 };
    { kiten = "国会議事堂前"; shuten = "霞ヶ関"; keiyu = "千代田線"; kyori = 0.7; jikan = 1 };
    { kiten = "霞ヶ関"; shuten = "日比谷"; keiyu = "千代田線"; kyori = 1.2; jikan = 2 };
    { kiten = "日比谷"; shuten = "二重橋前"; keiyu = "千代田線"; kyori = 0.7; jikan = 1 };
    { kiten = "二重橋前"; shuten = "大手町"; keiyu = "千代田線"; kyori = 0.7; jikan = 1 };
    { kiten = "大手町"; shuten = "新御茶ノ水"; keiyu = "千代田線"; kyori = 1.3; jikan = 2 };
    { kiten = "新御茶ノ水"; shuten = "湯島"; keiyu = "千代田線"; kyori = 1.2; jikan = 2 };
    { kiten = "湯島"; shuten = "根津"; keiyu = "千代田線"; kyori = 1.2; jikan = 2 };
    { kiten = "根津"; shuten = "千駄木"; keiyu = "千代田線"; kyori = 1.0; jikan = 2 };
    { kiten = "千駄木"; shuten = "西日暮里"; keiyu = "千代田線"; kyori = 0.9; jikan = 1 };
    { kiten = "西日暮里"; shuten = "町屋"; keiyu = "千代田線"; kyori = 1.7; jikan = 2 };
    { kiten = "町屋"; shuten = "北千住"; keiyu = "千代田線"; kyori = 2.6; jikan = 3 };
    { kiten = "北千住"; shuten = "綾瀬"; keiyu = "千代田線"; kyori = 2.5; jikan = 3 };
    { kiten = "綾瀬"; shuten = "北綾瀬"; keiyu = "千代田線"; kyori = 2.1; jikan = 4 };
    { kiten = "浅草"; shuten = "田原町"; keiyu = "銀座線"; kyori = 0.8; jikan = 2 };
    { kiten = "田原町"; shuten = "稲荷町"; keiyu = "銀座線"; kyori = 0.7; jikan = 1 };
    { kiten = "稲荷町"; shuten = "上野"; keiyu = "銀座線"; kyori = 0.7; jikan = 2 };
    { kiten = "上野"; shuten = "上野広小路"; keiyu = "銀座線"; kyori = 0.5; jikan = 2 };
    { kiten = "上野広小路"; shuten = "末広町"; keiyu = "銀座線"; kyori = 0.6; jikan = 1 };
    { kiten = "末広町"; shuten = "神田"; keiyu = "銀座線"; kyori = 1.1; jikan = 2 };
    { kiten = "神田"; shuten = "三越前"; keiyu = "銀座線"; kyori = 0.7; jikan = 1 };
    { kiten = "三越前"; shuten = "日本橋"; keiyu = "銀座線"; kyori = 0.6; jikan = 2 };
    { kiten = "日本橋"; shuten = "京橋"; keiyu = "銀座線"; kyori = 0.7; jikan = 2 };
    { kiten = "京橋"; shuten = "銀座"; keiyu = "銀座線"; kyori = 0.7; jikan = 1 };
    { kiten = "銀座"; shuten = "新橋"; keiyu = "銀座線"; kyori = 0.9; jikan = 2 };
    { kiten = "新橋"; shuten = "虎ノ門"; keiyu = "銀座線"; kyori = 0.8; jikan = 2 };
    { kiten = "虎ノ門"; shuten = "溜池山王"; keiyu = "銀座線"; kyori = 0.6; jikan = 2 };
    { kiten = "溜池山王"; shuten = "赤坂見附"; keiyu = "銀座線"; kyori = 0.9; jikan = 2 };
    { kiten = "赤坂見附"; shuten = "青山一丁目"; keiyu = "銀座線"; kyori = 1.3; jikan = 2 };
    { kiten = "青山一丁目"; shuten = "外苑前"; keiyu = "銀座線"; kyori = 0.7; jikan = 2 };
    { kiten = "外苑前"; shuten = "表参道"; keiyu = "銀座線"; kyori = 0.7; jikan = 1 };
    { kiten = "表参道"; shuten = "渋谷"; keiyu = "銀座線"; kyori = 1.3; jikan = 1 };
    { kiten = "渋谷"; shuten = "表参道"; keiyu = "半蔵門線"; kyori = 1.3; jikan = 2 };
    { kiten = "表参道"; shuten = "青山一丁目"; keiyu = "半蔵門線"; kyori = 1.4; jikan = 2 };
    { kiten = "青山一丁目"; shuten = "永田町"; keiyu = "半蔵門線"; kyori = 1.3; jikan = 2 };
    { kiten = "永田町"; shuten = "半蔵門"; keiyu = "半蔵門線"; kyori = 1.0; jikan = 2 };
    { kiten = "半蔵門"; shuten = "九段下"; keiyu = "半蔵門線"; kyori = 1.6; jikan = 2 };
    { kiten = "九段下"; shuten = "神保町"; keiyu = "半蔵門線"; kyori = 0.4; jikan = 1 };
    { kiten = "神保町"; shuten = "大手町"; keiyu = "半蔵門線"; kyori = 1.7; jikan = 3 };
    { kiten = "大手町"; shuten = "三越前"; keiyu = "半蔵門線"; kyori = 0.7; jikan = 1 };
    { kiten = "三越前"; shuten = "水天宮前"; keiyu = "半蔵門線"; kyori = 1.3; jikan = 2 };
    { kiten = "水天宮前"; shuten = "清澄白河"; keiyu = "半蔵門線"; kyori = 1.7; jikan = 3 };
    { kiten = "清澄白河"; shuten = "住吉"; keiyu = "半蔵門線"; kyori = 1.9; jikan = 3 };
    { kiten = "住吉"; shuten = "錦糸町"; keiyu = "半蔵門線"; kyori = 1.; jikan = 2 };
    { kiten = "錦糸町"; shuten = "押上"; keiyu = "半蔵門線"; kyori = 1.4; jikan = 2 };
    { kiten = "中目黒"; shuten = "恵比寿"; keiyu = "日比谷線"; kyori = 1.; jikan = 2 };
    { kiten = "恵比寿"; shuten = "広尾"; keiyu = "日比谷線"; kyori = 1.5; jikan = 3 };
    { kiten = "広尾"; shuten = "六本木"; keiyu = "日比谷線"; kyori = 1.7; jikan = 3 };
    { kiten = "六本木"; shuten = "神谷町"; keiyu = "日比谷線"; kyori = 1.5; jikan = 3 };
    { kiten = "神谷町"; shuten = "霞ヶ関"; keiyu = "日比谷線"; kyori = 1.3; jikan = 2 };
    { kiten = "霞ヶ関"; shuten = "日比谷"; keiyu = "日比谷線"; kyori = 1.2; jikan = 2 };
    { kiten = "日比谷"; shuten = "銀座"; keiyu = "日比谷線"; kyori = 0.4; jikan = 1 };
    { kiten = "銀座"; shuten = "東銀座"; keiyu = "日比谷線"; kyori = 0.4; jikan = 1 };
    { kiten = "東銀座"; shuten = "築地"; keiyu = "日比谷線"; kyori = 0.6; jikan = 2 };
    { kiten = "築地"; shuten = "八丁堀"; keiyu = "日比谷線"; kyori = 1.; jikan = 2 };
    { kiten = "八丁堀"; shuten = "茅場町"; keiyu = "日比谷線"; kyori = 0.5; jikan = 1 };
    { kiten = "茅場町"; shuten = "人形町"; keiyu = "日比谷線"; kyori = 0.9; jikan = 2 };
    { kiten = "人形町"; shuten = "小伝馬町"; keiyu = "日比谷線"; kyori = 0.6; jikan = 1 };
    { kiten = "小伝馬町"; shuten = "秋葉原"; keiyu = "日比谷線"; kyori = 0.9; jikan = 2 };
    { kiten = "秋葉原"; shuten = "仲御徒町"; keiyu = "日比谷線"; kyori = 1.; jikan = 1 };
    { kiten = "仲御徒町"; shuten = "上野"; keiyu = "日比谷線"; kyori = 0.5; jikan = 1 };
    { kiten = "上野"; shuten = "入谷"; keiyu = "日比谷線"; kyori = 1.2; jikan = 2 };
    { kiten = "入谷"; shuten = "三ノ輪"; keiyu = "日比谷線"; kyori = 1.2; jikan = 2 };
    { kiten = "三ノ輪"; shuten = "南千住"; keiyu = "日比谷線"; kyori = 0.8; jikan = 2 };
    { kiten = "南千住"; shuten = "北千住"; keiyu = "日比谷線"; kyori = 1.8; jikan = 3 };
    { kiten = "池袋"; shuten = "新大塚"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 3 };
    { kiten = "新大塚"; shuten = "茗荷谷"; keiyu = "丸ノ内線"; kyori = 1.2; jikan = 2 };
    { kiten = "茗荷谷"; shuten = "後楽園"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 2 };
    { kiten = "後楽園"; shuten = "本郷三丁目"; keiyu = "丸ノ内線"; kyori = 0.8; jikan = 1 };
    { kiten = "本郷三丁目"; shuten = "御茶ノ水"; keiyu = "丸ノ内線"; kyori = 0.8; jikan = 1 };
    { kiten = "御茶ノ水"; shuten = "淡路町"; keiyu = "丸ノ内線"; kyori = 0.8; jikan = 1 };
    { kiten = "淡路町"; shuten = "大手町"; keiyu = "丸ノ内線"; kyori = 0.9; jikan = 2 };
    { kiten = "大手町"; shuten = "東京"; keiyu = "丸ノ内線"; kyori = 0.6; jikan = 1 };
    { kiten = "東京"; shuten = "銀座"; keiyu = "丸ノ内線"; kyori = 1.1; jikan = 2 };
    { kiten = "銀座"; shuten = "霞ヶ関"; keiyu = "丸ノ内線"; kyori = 1.0; jikan = 2 };
    { kiten = "霞ヶ関"; shuten = "国会議事堂前"; keiyu = "丸ノ内線"; kyori = 0.7; jikan = 1 };
    {
      kiten = "国会議事堂前";
      shuten = "赤坂見附";
      keiyu = "丸ノ内線";
      kyori = 0.9;
      jikan = 2;
    };
    { kiten = "赤坂見附"; shuten = "四ツ谷"; keiyu = "丸ノ内線"; kyori = 1.3; jikan = 2 };
    { kiten = "四ツ谷"; shuten = "四谷三丁目"; keiyu = "丸ノ内線"; kyori = 1.0; jikan = 2 };
    {
      kiten = "四谷三丁目";
      shuten = "新宿御苑前";
      keiyu = "丸ノ内線";
      kyori = 0.9;
      jikan = 1;
    };
    {
      kiten = "新宿御苑前";
      shuten = "新宿三丁目";
      keiyu = "丸ノ内線";
      kyori = 0.7;
      jikan = 1;
    };
    { kiten = "新宿三丁目"; shuten = "新宿"; keiyu = "丸ノ内線"; kyori = 0.3; jikan = 1 };
    { kiten = "新宿"; shuten = "西新宿"; keiyu = "丸ノ内線"; kyori = 0.8; jikan = 1 };
    { kiten = "西新宿"; shuten = "中野坂上"; keiyu = "丸ノ内線"; kyori = 1.1; jikan = 2 };
    { kiten = "中野坂上"; shuten = "新中野"; keiyu = "丸ノ内線"; kyori = 1.1; jikan = 2 };
    { kiten = "新中野"; shuten = "東高円寺"; keiyu = "丸ノ内線"; kyori = 1.0; jikan = 1 };
    { kiten = "東高円寺"; shuten = "新高円寺"; keiyu = "丸ノ内線"; kyori = 0.9; jikan = 1 };
    { kiten = "新高円寺"; shuten = "南阿佐ヶ谷"; keiyu = "丸ノ内線"; kyori = 1.2; jikan = 2 };
    { kiten = "南阿佐ヶ谷"; shuten = "荻窪"; keiyu = "丸ノ内線"; kyori = 1.5; jikan = 2 };
    { kiten = "中野坂上"; shuten = "中野新橋"; keiyu = "丸ノ内線"; kyori = 1.3; jikan = 2 };
    {
      kiten = "中野新橋";
      shuten = "中野富士見町";
      keiyu = "丸ノ内線";
      kyori = 0.6;
      jikan = 1;
    };
    { kiten = "中野富士見町"; shuten = "方南町"; keiyu = "丸ノ内線"; kyori = 1.3; jikan = 2 };
    { kiten = "市ヶ谷"; shuten = "四ツ谷"; keiyu = "南北線"; kyori = 1.0; jikan = 2 };
    { kiten = "四ツ谷"; shuten = "永田町"; keiyu = "南北線"; kyori = 1.3; jikan = 3 };
    { kiten = "永田町"; shuten = "溜池山王"; keiyu = "南北線"; kyori = 0.9; jikan = 1 };
    { kiten = "溜池山王"; shuten = "六本木一丁目"; keiyu = "南北線"; kyori = 0.9; jikan = 2 };
    { kiten = "六本木一丁目"; shuten = "麻布十番"; keiyu = "南北線"; kyori = 1.2; jikan = 2 };
    { kiten = "麻布十番"; shuten = "白金高輪"; keiyu = "南北線"; kyori = 1.3; jikan = 2 };
    { kiten = "白金高輪"; shuten = "白金台"; keiyu = "南北線"; kyori = 1.0; jikan = 2 };
    { kiten = "白金台"; shuten = "目黒"; keiyu = "南北線"; kyori = 1.3; jikan = 2 };
    { kiten = "市ヶ谷"; shuten = "飯田橋"; keiyu = "南北線"; kyori = 1.1; jikan = 2 };
    { kiten = "飯田橋"; shuten = "後楽園"; keiyu = "南北線"; kyori = 1.4; jikan = 2 };
    { kiten = "後楽園"; shuten = "東大前"; keiyu = "南北線"; kyori = 1.3; jikan = 3 };
    { kiten = "東大前"; shuten = "本駒込"; keiyu = "南北線"; kyori = 0.9; jikan = 2 };
    { kiten = "本駒込"; shuten = "駒込"; keiyu = "南北線"; kyori = 1.4; jikan = 2 };
    { kiten = "駒込"; shuten = "西ヶ原"; keiyu = "南北線"; kyori = 1.4; jikan = 2 };
    { kiten = "西ヶ原"; shuten = "王子"; keiyu = "南北線"; kyori = 1.0; jikan = 2 };
    { kiten = "王子"; shuten = "王子神谷"; keiyu = "南北線"; kyori = 1.2; jikan = 2 };
    { kiten = "王子神谷"; shuten = "志茂"; keiyu = "南北線"; kyori = 1.6; jikan = 3 };
    { kiten = "志茂"; shuten = "赤羽岩淵"; keiyu = "南北線"; kyori = 1.1; jikan = 2 };
    { kiten = "西船橋"; shuten = "原木中山"; keiyu = "東西線"; kyori = 1.9; jikan = 3 };
    { kiten = "原木中山"; shuten = "妙典"; keiyu = "東西線"; kyori = 2.1; jikan = 2 };
    { kiten = "妙典"; shuten = "行徳"; keiyu = "東西線"; kyori = 1.3; jikan = 2 };
    { kiten = "行徳"; shuten = "南行徳"; keiyu = "東西線"; kyori = 1.5; jikan = 2 };
    { kiten = "南行徳"; shuten = "浦安"; keiyu = "東西線"; kyori = 1.2; jikan = 2 };
    { kiten = "浦安"; shuten = "葛西"; keiyu = "東西線"; kyori = 1.9; jikan = 2 };
    { kiten = "葛西"; shuten = "西葛西"; keiyu = "東西線"; kyori = 1.2; jikan = 2 };
    { kiten = "西葛西"; shuten = "南砂町"; keiyu = "東西線"; kyori = 2.7; jikan = 2 };
    { kiten = "南砂町"; shuten = "東陽町"; keiyu = "東西線"; kyori = 1.2; jikan = 2 };
    { kiten = "東陽町"; shuten = "木場"; keiyu = "東西線"; kyori = 0.9; jikan = 1 };
    { kiten = "木場"; shuten = "門前仲町"; keiyu = "東西線"; kyori = 1.1; jikan = 1 };
    { kiten = "門前仲町"; shuten = "茅場町"; keiyu = "東西線"; kyori = 1.8; jikan = 2 };
    { kiten = "茅場町"; shuten = "日本橋"; keiyu = "東西線"; kyori = 0.5; jikan = 1 };
    { kiten = "日本橋"; shuten = "大手町"; keiyu = "東西線"; kyori = 0.8; jikan = 1 };
    { kiten = "大手町"; shuten = "竹橋"; keiyu = "東西線"; kyori = 1.0; jikan = 2 };
    { kiten = "竹橋"; shuten = "九段下"; keiyu = "東西線"; kyori = 1.0; jikan = 1 };
    { kiten = "九段下"; shuten = "飯田橋"; keiyu = "東西線"; kyori = 0.7; jikan = 1 };
    { kiten = "飯田橋"; shuten = "神楽坂"; keiyu = "東西線"; kyori = 1.2; jikan = 2 };
    { kiten = "神楽坂"; shuten = "早稲田"; keiyu = "東西線"; kyori = 1.2; jikan = 2 };
    { kiten = "早稲田"; shuten = "高田馬場"; keiyu = "東西線"; kyori = 1.7; jikan = 3 };
    { kiten = "高田馬場"; shuten = "落合"; keiyu = "東西線"; kyori = 1.9; jikan = 3 };
    { kiten = "落合"; shuten = "中野"; keiyu = "東西線"; kyori = 2.0; jikan = 3 };
    { kiten = "新木場"; shuten = "辰巳"; keiyu = "有楽町線"; kyori = 1.5; jikan = 2 };
    { kiten = "辰巳"; shuten = "豊洲"; keiyu = "有楽町線"; kyori = 1.7; jikan = 2 };
    { kiten = "豊洲"; shuten = "月島"; keiyu = "有楽町線"; kyori = 1.4; jikan = 2 };
    { kiten = "月島"; shuten = "新富町"; keiyu = "有楽町線"; kyori = 1.3; jikan = 2 };
    { kiten = "新富町"; shuten = "銀座一丁目"; keiyu = "有楽町線"; kyori = 0.7; jikan = 1 };
    { kiten = "銀座一丁目"; shuten = "有楽町"; keiyu = "有楽町線"; kyori = 0.5; jikan = 1 };
    { kiten = "有楽町"; shuten = "桜田門"; keiyu = "有楽町線"; kyori = 1.0; jikan = 1 };
    { kiten = "桜田門"; shuten = "永田町"; keiyu = "有楽町線"; kyori = 0.9; jikan = 2 };
    { kiten = "永田町"; shuten = "麹町"; keiyu = "有楽町線"; kyori = 0.9; jikan = 1 };
    { kiten = "麹町"; shuten = "市ヶ谷"; keiyu = "有楽町線"; kyori = 0.9; jikan = 1 };
    { kiten = "市ヶ谷"; shuten = "飯田橋"; keiyu = "有楽町線"; kyori = 1.1; jikan = 2 };
    { kiten = "飯田橋"; shuten = "江戸川橋"; keiyu = "有楽町線"; kyori = 1.6; jikan = 3 };
    { kiten = "江戸川橋"; shuten = "護国寺"; keiyu = "有楽町線"; kyori = 1.3; jikan = 2 };
    { kiten = "護国寺"; shuten = "東池袋"; keiyu = "有楽町線"; kyori = 1.1; jikan = 2 };
    { kiten = "東池袋"; shuten = "池袋"; keiyu = "有楽町線"; kyori = 2.0; jikan = 2 };
    { kiten = "池袋"; shuten = "要町"; keiyu = "有楽町線"; kyori = 1.2; jikan = 2 };
    { kiten = "要町"; shuten = "千川"; keiyu = "有楽町線"; kyori = 1.0; jikan = 1 };
    { kiten = "千川"; shuten = "小竹向原"; keiyu = "有楽町線"; kyori = 1.0; jikan = 2 };
    { kiten = "小竹向原"; shuten = "氷川台"; keiyu = "有楽町線"; kyori = 1.5; jikan = 2 };
    { kiten = "氷川台"; shuten = "平和台"; keiyu = "有楽町線"; kyori = 1.4; jikan = 2 };
    { kiten = "平和台"; shuten = "営団赤塚"; keiyu = "有楽町線"; kyori = 1.8; jikan = 2 };
    { kiten = "営団赤塚"; shuten = "営団成増"; keiyu = "有楽町線"; kyori = 1.5; jikan = 2 };
    { kiten = "営団成増"; shuten = "和光市"; keiyu = "有楽町線"; kyori = 2.1; jikan = 3 };
  ]

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

(* テスト *)
(* let test1 = romaji_to_kanji "myogadani" global_ekimei_list = "" *)
let test2 = romaji_to_kanji "myogadani" global_ekimei_list = "茗荷谷"
let test2 = romaji_to_kanji "korakuen" global_ekimei_list = "後楽園"

exception Not_found_ekikan of string * string

(* 目的：漢字の駅名 ekimei1 と ekimei2、駅間の 2 分探索木 ekikan_tree を受け取ったら 2 駅間の距離を返す *)
(* みつからなかったら infinity を返す *)
(* get_ekikan_kyori : string -> string -> (string * (string * float) list) Tree.t -> float *)
let rec get_ekikan_kyori ekimei1 ekimei2 ekikan_tree =
  try
    let lst = search ekikan_tree ekimei1 in
    List.assoc ekimei2 lst
  with Not_found -> infinity

(* 目的：ローマ字の駅名 ekimei1 と ekimei2 の間の距離を表示する *)
(* kyori_wo_hyoji : string -> string -> ekimei_t list -> ekikan_t list -> string *)
let rec kyori_wo_hyoji ekimei1 ekimei2 lst1 lst2 =
  try
    let kanji_ekimei1 = romaji_to_kanji ekimei1 lst1 in
    let kanji_ekimei2 = romaji_to_kanji ekimei2 lst1 in
    let kyori = get_ekikan_kyori kanji_ekimei1 kanji_ekimei2 lst2 in
    kanji_ekimei1 ^ "駅から" ^ kanji_ekimei2 ^ "駅までは " ^ string_of_float kyori
    ^ " km です"
  with
  | No_such_station ekimei0 -> ekimei0 ^ "という駅は存在しません"
  | Not_found_ekikan (ekimei1, ekimei2) ->
      ekimei1 ^ "駅と" ^ ekimei2 ^ "駅はつながっていません"

(* 駅名、最短距離、手前の駅名のリスト *)
type eki_t = {
  namae : string; (* 駅名（漢字の文字列） *)
  saitan_kyori : float; (* 最短距離（実数） *)
  temae_list : string list; (* 駅名（漢字の文字列）のリスト *)
}

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

(* 目的：重複を取り除き、各点の接続情報を示す木 ekikan_tree を作る。*)
(* 駅間情報のみから構築し、駅情報は使わない。*)
(* make_ekikan_tree : ekikan_t list -> ekikan_tree_t *)
let make_ekikan_tree ekikan_list =
  let insert ekikan_tree kiten shuten kyori =
    let lst = try search ekikan_tree kiten with Not_found -> [] in
    insert ekikan_tree kiten ((shuten, kyori) :: lst)
  in
  let insert_ekikan ekikan_tree
      { kiten = k; shuten = s; keiyu = y; kyori = r; jikan = j } =
    insert (insert ekikan_tree k s r) s k r
  in
  List.fold_left insert_ekikan empty ekikan_list

(* 未確定駅の情報を表すヒープの型。以下のコメントでは heap_t と記述 *)
(* type heap_t = (float,			最短距離
    string *			駅名
    string list *			手前リスト
   ) Heap.t *)

(* 各駅のヒープ中の位置を表す木の型。以下のコメントでは index_tree_t と記述 *)
(* type index_tree_t = (string, index_t) Tree.t *)

(* 目的：ekikan_tree から eki_heap と index_tree を作り、kiten を初期化する *)
(* make_eki_heap_and_index_tree :
   string -> ekikan_tree_t -> heap_t * index_tree_t *)
let make_eki_heap_and_index_tree kiten ekikan_tree =
  traverse
    (fun (eki_heap, index_tree) k lst ->
      let index, heap =
        Heap.insert eki_heap
          (if k = kiten then 0. else infinity)
          (k, if k = kiten then [ k ] else [])
      in
      let index_tree' = insert index_tree k index in
      (heap, index_tree'))
    (Heap.create (length ekikan_tree) 0. ("駅名", []), empty)
    ekikan_tree

(* 目的：確定した駅に接続している駅の最短距離、手前リストを更新する *)
(* koushin : string -> float -> string list ->
      heap_t -> ekikan_tree_t -> index_tree_t -> heap_t *)
let koushin pn ps pt eki_heap ekikan_tree index_tree =
  let lst = search ekikan_tree pn in
  List.fold_left
    (fun eki_heap (shuten, kyori) ->
      try
        let shuten_index = search index_tree shuten in
        let saitan_kyori, (n, _) = Heap.get eki_heap shuten_index in
        let new_saitan_kyori = ps +. kyori in
        if new_saitan_kyori <= saitan_kyori then
          Heap.set eki_heap shuten_index new_saitan_kyori (n, n :: pt)
        else eki_heap
      with Not_found -> eki_heap)
    eki_heap lst

(* 未確定の駅のリスト eki_lst と駅間のリスト ekikan_lst からダイクストラのアルゴリズムにしたがって各駅について最短距離と最短経路が正しく入ったリストを返す *)
(* dijkstra_main : heap_t -> ekikan_tree_t -> index_tree_t -> eki_t list *)
let rec dijkstra_main eki_heap ekikan_tree index_tree =
  if Heap.length eki_heap = 0 then []
  else
    let (ps, (pn, pt)), rest_heap = Heap.split_top eki_heap in
    let eki_heap2 = koushin pn ps pt rest_heap ekikan_tree index_tree in
    { namae = pn; saitan_kyori = ps; temae_list = pt }
    :: dijkstra_main eki_heap2 ekikan_tree index_tree

(* 目的：eki_t 型のレコードをきれいに表示する *)
(* print_eki : eki_t -> unit *)
let print_eki eki =
  match eki with
  | { namae = n; saitan_kyori = s; temae_list = lst } -> (
      match lst with
      | [] -> assert false (* この場合は起こりえない *)
      | [ a ] ->
          print_string (a ^ "（" ^ string_of_float s ^ "km）");
          print_newline ()
      | a :: rest ->
          List.fold_right (fun b () -> print_string (b ^ "、")) rest ();
          print_string (a ^ "（" ^ string_of_float s ^ "km）");
          print_newline ())

(* 目的：受け取った eki_list から shuten のレコードを探し出す *)
(* find : string -> eki_t list -> eki_t *)
let rec find shuten eki_list =
  match eki_list with
  | [] -> { namae = ""; saitan_kyori = infinity; temae_list = [] }
  | ({ namae = n; saitan_kyori = s; temae_list = t } as first) :: rest ->
      if n = shuten then first else find shuten rest

(* 始点の駅 shiten から終点の駅 shuten までの最短路を求める *)
(* dijkstra : string -> string -> eki_t *)
let dijkstra kiten shuten =
  let kiten_kanji, shuten_kanji =
    ( romaji_to_kanji kiten global_ekimei_list,
      romaji_to_kanji shuten global_ekimei_list )
  in
  let ekikan_tree = make_ekikan_tree global_ekikan_list in
  let eki_heap, index_tree =
    make_eki_heap_and_index_tree kiten_kanji ekikan_tree
  in
  let eki_list = dijkstra_main eki_heap ekikan_tree index_tree in
  find shuten_kanji eki_list

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
