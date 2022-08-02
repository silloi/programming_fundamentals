(* 人ひとり分のデータ（名前、身長、体重、誕生日（月、日）、血液型）を表す型 *)
type person_t = {
  namae : string; (* 名前 *)
  shincho : float; (* 身長 *)
  taiju : float; (* 体重 *)
  tanjoubi_tsuki : int; (* 誕生日（月） *)
  tanjoubi_hi : int; (* 誕生日（日） *)
  ketsueki : string; (* 血液型 *)
}

(* 目的：学生リスト lst のうち各血液型の人数を集計する *)
(* shukei_ketsueki : person_t list -> int * int * int * int *)
let rec shukei_ketsueki lst =
  match lst with
  | [] -> (0, 0, 0, 0)
  | {
      namae = n;
      shincho = s;
      taiju = t;
      tanjoubi_tsuki = tsuki;
      tanjoubi_hi = hi;
      ketsueki = k;
    }
    :: rest ->
      let a, b, o, ab = shukei_ketsueki rest in
      if k = "A" then (a + 1, b, o, ab)
      else if k = "B" then (a, b + 1, o, ab)
      else if k = "O" then (a, b, o + 1, ab)
      else (a, b, o, ab + 1)

(* 目的：学生リスト lst のうち最も人数の多い血液型を返す *)
(* saita_ketsueki : person_t list -> string *)
let saita_ketsueki lst =
  let a, b, o, ab = shukei_ketsueki lst in
  if a >= b && a >= o && a >= ab then "A"
  else if b >= 0 && b >= ab then "B"
  else if o >= ab then "O"
  else "AB"

(* テスト *)
let test1 =
  saita_ketsueki
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
  = "A"

let test2 =
  saita_ketsueki
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
  = "A"

let test3 =
  saita_ketsueki
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
      {
        namae = "夏彦";
        shincho = 160.0;
        taiju = 46.0;
        tanjoubi_tsuki = 11;
        tanjoubi_hi = 8;
        ketsueki = "B";
      };
    ]
  = "B"

let test3 =
  saita_ketsueki
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
      {
        namae = "夏彦";
        shincho = 160.0;
        taiju = 46.0;
        tanjoubi_tsuki = 11;
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
      {
        namae = "冬美";
        shincho = 152.0;
        taiju = 42.0;
        tanjoubi_tsuki = 2;
        tanjoubi_hi = 10;
        ketsueki = "AB";
      };
    ]
  = "B"
