(* 人ひとり分のデータ（名前、身長、体重、誕生日（月、日）、血液型）を表す型 *)
type person_t = {
  namae : string; (* 名前 *)
  shincho : float; (* 身長 *)
  taiju : float; (* 体重 *)
  tanjoubi_tsuki : int; (* 誕生日（月） *)
  tanjoubi_hi : int; (* 誕生日（日） *)
  ketsueki : string; (* 血液型 *)
}

(* person_t list は
     - []              空リスト、あるいは
     - first :: rest   最初の要素が first で残りのリストが rest
                       （first は person_t 型、
                         rest が自己参照のケース）
   という形 *)

(* person_t list 型のデータの例 *)
let lst1 = []

let lst2 =
  [
    {
      namae = "春夫";
      shincho = 168.0;
      taiju = 60.0;
      tanjoubi_tsuki = 4;
      tanjoubi_hi = 16;
      ketsueki = "A";
    };
  ]

let lst3 =
  [
    {
      namae = "春夫";
      shincho = 168.0;
      taiju = 60.0;
      tanjoubi_tsuki = 4;
      tanjoubi_hi = 16;
      ketsueki = "A";
    };
    {
      namae = "夏樹";
      shincho = 154.0;
      taiju = 48.0;
      tanjoubi_tsuki = 7;
      tanjoubi_hi = 8;
      ketsueki = "B";
    };
  ]

let lst4 =
  [
    {
      namae = "秋子";
      shincho = 160.0;
      taiju = 46.0;
      tanjoubi_tsuki = 11;
      tanjoubi_hi = 8;
      ketsueki = "O";
    };
    {
      namae = "春夫";
      shincho = 168.0;
      taiju = 60.0;
      tanjoubi_tsuki = 4;
      tanjoubi_hi = 16;
      ketsueki = "A";
    };
    {
      namae = "夏樹";
      shincho = 154.0;
      taiju = 48.0;
      tanjoubi_tsuki = 7;
      tanjoubi_hi = 8;
      ketsueki = "B";
    };
  ]

let lst5 =
  [
    {
      namae = "秋子";
      shincho = 160.0;
      taiju = 46.0;
      tanjoubi_tsuki = 11;
      tanjoubi_hi = 8;
      ketsueki = "O";
    };
    {
      namae = "春夫";
      shincho = 168.0;
      taiju = 60.0;
      tanjoubi_tsuki = 4;
      tanjoubi_hi = 16;
      ketsueki = "A";
    };
    {
      namae = "夏樹";
      shincho = 154.0;
      taiju = 48.0;
      tanjoubi_tsuki = 7;
      tanjoubi_hi = 8;
      ketsueki = "B";
    };
    {
      namae = "太郎";
      shincho = 172.0;
      taiju = 68.0;
      tanjoubi_tsuki = 1;
      tanjoubi_hi = 1;
      ketsueki = "A";
    };
  ]

(* 目的：人のリスト lst のうち血液型が A 型の人の数を返す *)
(* count_ketsueki_A : person_t list -> person_t list *)
let rec count_ketsueki_A lst =
  match lst with
  | [] -> 0
  | {
      namae = n;
      shincho = s;
      taiju = t;
      tanjoubi_tsuki = tsuki;
      tanjoubi_hi = hi;
      ketsueki = k;
    }
    :: rest ->
      if k = "A" then 1 + count_ketsueki_A rest else count_ketsueki_A rest

(* テスト *)
let test1 = count_ketsueki_A lst1 = 0
let test2 = count_ketsueki_A lst2 = 1
let test3 = count_ketsueki_A lst3 = 1
let test4 = count_ketsueki_A lst4 = 1
let test5 = count_ketsueki_A lst5 = 2