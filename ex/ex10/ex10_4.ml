(* 人ひとり分のデータ（名前、身長、体重、誕生日（月、日）、血液型）を表す型 *)
type person_t = {
  namae : string; (* 名前 *)
  shincho : float; (* 身長 *)
  taiju : float; (* 体重 *)
  tanjoubi_tsuki : int; (* 誕生日（月） *)
  tanjoubi_hi : int; (* 誕生日（日） *)
  ketsueki : string; (* 血液型 *)
}

(* 目的：namae フィールドについて昇順の人のリスト lst に person を挿入したリストを返す *)
(* insert : person_t list -> person_t -> person_t list *)
let rec person_insert lst person =
  match lst with
  | [] -> [ person ]
  | ({
       namae = first_n;
       shincho = first_s;
       taiju = first_t;
       tanjoubi_tsuki = first_tsuki;
       tanjoubi_hi = first_hi;
       ketsueki = first_k;
     } as first)
    :: rest -> (
      match person with
      | {
       namae = person_n;
       shincho = person_s;
       taiju = person_t;
       tanjoubi_tsuki = person_tsuki;
       tanjoubi_hi = person_hi;
       ketsueki = person_k;
      } ->
          if first_n < person_n then first :: person_insert rest person
          else person :: first :: rest)

(* テスト *)
let test1 =
  person_insert []
    {
      namae = "春夫";
      shincho = 168.0;
      taiju = 60.0;
      tanjoubi_tsuki = 4;
      tanjoubi_hi = 16;
      ketsueki = "A";
    }
  = [
      {
        namae = "春夫";
        shincho = 168.0;
        taiju = 60.0;
        tanjoubi_tsuki = 4;
        tanjoubi_hi = 16;
        ketsueki = "A";
      };
    ]

let test2 =
  person_insert
    [
      {
        namae = "夏樹";
        shincho = 154.0;
        taiju = 48.0;
        tanjoubi_tsuki = 7;
        tanjoubi_hi = 8;
        ketsueki = "B";
      };
    ]
    {
      namae = "春夫";
      shincho = 168.0;
      taiju = 60.0;
      tanjoubi_tsuki = 4;
      tanjoubi_hi = 16;
      ketsueki = "A";
    }
  = [
      {
        namae = "夏樹";
        shincho = 154.0;
        taiju = 48.0;
        tanjoubi_tsuki = 7;
        tanjoubi_hi = 8;
        ketsueki = "B";
      };
      {
        namae = "春夫";
        shincho = 168.0;
        taiju = 60.0;
        tanjoubi_tsuki = 4;
        tanjoubi_hi = 16;
        ketsueki = "A";
      };
    ]

let test3 =
  person_insert
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
    {
      namae = "夏樹";
      shincho = 154.0;
      taiju = 48.0;
      tanjoubi_tsuki = 7;
      tanjoubi_hi = 8;
      ketsueki = "B";
    }
  = [
      {
        namae = "夏樹";
        shincho = 154.0;
        taiju = 48.0;
        tanjoubi_tsuki = 7;
        tanjoubi_hi = 8;
        ketsueki = "B";
      };
      {
        namae = "春夫";
        shincho = 168.0;
        taiju = 60.0;
        tanjoubi_tsuki = 4;
        tanjoubi_hi = 16;
        ketsueki = "A";
      };
    ]

let test4 =
  person_insert
    [
      {
        namae = "夏樹";
        shincho = 154.0;
        taiju = 48.0;
        tanjoubi_tsuki = 7;
        tanjoubi_hi = 8;
        ketsueki = "B";
      };
      {
        namae = "秋子";
        shincho = 160.0;
        taiju = 46.0;
        tanjoubi_tsuki = 11;
        tanjoubi_hi = 8;
        ketsueki = "O";
      };
    ]
    {
      namae = "春夫";
      shincho = 168.0;
      taiju = 60.0;
      tanjoubi_tsuki = 4;
      tanjoubi_hi = 16;
      ketsueki = "A";
    }
  = [
      {
        namae = "夏樹";
        shincho = 154.0;
        taiju = 48.0;
        tanjoubi_tsuki = 7;
        tanjoubi_hi = 8;
        ketsueki = "B";
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
        namae = "秋子";
        shincho = 160.0;
        taiju = 46.0;
        tanjoubi_tsuki = 11;
        tanjoubi_hi = 8;
        ketsueki = "O";
      };
    ]

(* 目的：人のデータのリスト lst を受け取ったら名前の順に整列して返す *)
(* person_sort : person_t list -> person_t list *)
let rec person_sort lst =
  match lst with
  | [] -> []
  | first :: rest -> person_insert (person_sort rest) first

(* テスト *)
let test5 =
  person_sort
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
  = [
      {
        namae = "春夫";
        shincho = 168.0;
        taiju = 60.0;
        tanjoubi_tsuki = 4;
        tanjoubi_hi = 16;
        ketsueki = "A";
      };
    ]

let test6 =
  person_sort
    [
      {
        namae = "夏樹";
        shincho = 154.0;
        taiju = 48.0;
        tanjoubi_tsuki = 7;
        tanjoubi_hi = 8;
        ketsueki = "B";
      };
      {
        namae = "春夫";
        shincho = 168.0;
        taiju = 60.0;
        tanjoubi_tsuki = 4;
        tanjoubi_hi = 16;
        ketsueki = "A";
      };
    ]
  = [
      {
        namae = "夏樹";
        shincho = 154.0;
        taiju = 48.0;
        tanjoubi_tsuki = 7;
        tanjoubi_hi = 8;
        ketsueki = "B";
      };
      {
        namae = "春夫";
        shincho = 168.0;
        taiju = 60.0;
        tanjoubi_tsuki = 4;
        tanjoubi_hi = 16;
        ketsueki = "A";
      };
    ]

let test6 =
  person_sort
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
  = [
      {
        namae = "夏樹";
        shincho = 154.0;
        taiju = 48.0;
        tanjoubi_tsuki = 7;
        tanjoubi_hi = 8;
        ketsueki = "B";
      };
      {
        namae = "春夫";
        shincho = 168.0;
        taiju = 60.0;
        tanjoubi_tsuki = 4;
        tanjoubi_hi = 16;
        ketsueki = "A";
      };
    ]

let test8 =
  person_sort
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
  = [
      {
        namae = "夏樹";
        shincho = 154.0;
        taiju = 48.0;
        tanjoubi_tsuki = 7;
        tanjoubi_hi = 8;
        ketsueki = "B";
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
        namae = "秋子";
        shincho = 160.0;
        taiju = 46.0;
        tanjoubi_tsuki = 11;
        tanjoubi_hi = 8;
        ketsueki = "O";
      };
    ]
