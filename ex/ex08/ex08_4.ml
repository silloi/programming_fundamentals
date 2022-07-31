(* 人の名前、身長（m）、体重（kg）、誕生日（月と日）、血液型 *)
type person_t = {
  namae : string; (* 名前 *)
  shincho : float; (* 身長（m） *)
  taiju : float; (* 体重（kg） *)
  tanjoubi_tsuki : int; (* 誕生日（月） *)
  tanjoubi_hi : int; (* 誕生日（日） *)
  ketsueki : string; (* 血液型 *)
}

(* 目的：person_t 型のデータを受け取ったら「◯◯さんの血液型は△△型です」という形の文字列を返す *)
(* ketsueki_hyouji : person_t -> string *)
let ketsueki_hyouji person =
  match person with
  | {
   namae = n;
   shincho = s;
   taiju = t;
   tanjoubi_tsuki = tsuki;
   tanjoubi_hi = hi;
   ketsueki = k;
  } ->
      n ^ "さんの血液型は " ^ k ^ " 型です"

(* テスト *)
let person1 =
  {
    namae = "春夫";
    shincho = 168.0;
    taiju = 60.0;
    tanjoubi_tsuki = 4;
    tanjoubi_hi = 16;
    ketsueki = "A";
  }

let person2 =
  {
    namae = "夏樹";
    shincho = 154.0;
    taiju = 48.0;
    tanjoubi_tsuki = 7;
    tanjoubi_hi = 8;
    ketsueki = "B";
  }

let person3 =
  {
    namae = "秋子";
    shincho = 160.0;
    taiju = 46.0;
    tanjoubi_tsuki = 11;
    tanjoubi_hi = 8;
    ketsueki = "O";
  }

let person4 =
  {
    namae = "冬美";
    shincho = 152.0;
    taiju = 42.0;
    tanjoubi_tsuki = 2;
    tanjoubi_hi = 10;
    ketsueki = "AB";
  }

let test1 = ketsueki_hyouji person1 = "春夫さんの血液型は A 型です"
let test2 = ketsueki_hyouji person2 = "夏樹さんの血液型は B 型です"
let test3 = ketsueki_hyouji person3 = "秋子さんの血液型は O 型です"
let test4 = ketsueki_hyouji person4 = "冬美さんの血液型は AB 型です"