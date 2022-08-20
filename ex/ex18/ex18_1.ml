(* 人の名前、身長（m）、体重（kg）、誕生日（月と日）、血液型 *)
type person_t = {
  namae : string; (* 名前 *)
  shincho : float; (* 身長（m） *)
  taiju : float; (* 体重（kg） *)
  tanjoubi_tsuki : int; (* 誕生日（月） *)
  tanjoubi_hi : int; (* 誕生日（日） *)
  ketsueki : string; (* 血液型 *)
}

(* 目的：person_t 型のリスト person_list を受け取ったら、最初の A 型の人のレコードをオプション型で返す *)
(* first_A : person_t list -> person_t option *)
let rec first_A person_list =
  match person_list with
  | [] -> None
  | ({
       namae = n;
       shincho = s;
       taiju = t;
       tanjoubi_tsuki = tsuki;
       tanjoubi_hi = hi;
       ketsueki = k;
     } as first)
    :: rest ->
      if k = "A" then Some first else first_A rest

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

let test1 = first_A [ person1 ] = Some person1
let test2 = first_A [ person1; person2 ] = Some person1
let test3 = first_A [ person2; person3 ] = None
let test4 = first_A [ person4; person1 ] = Some person1
