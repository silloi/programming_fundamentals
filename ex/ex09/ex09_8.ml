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
      namae = "八三一郎";
      shincho = 183.1;
      taiju = 83.1;
      tanjoubi_tsuki = 8;
      tanjoubi_hi = 31;
      ketsueki = "A";
    };
  ]

let lst3 =
  [
    {
      namae = "八二一郎";
      shincho = 182.1;
      taiju = 82.1;
      tanjoubi_tsuki = 8;
      tanjoubi_hi = 21;
      ketsueki = "B";
    };
    {
      namae = "八三一郎";
      shincho = 183.1;
      taiju = 83.1;
      tanjoubi_tsuki = 8;
      tanjoubi_hi = 31;
      ketsueki = "A";
    };
  ]

let lst4 =
  [
    {
      namae = "八二一郎";
      shincho = 182.1;
      taiju = 82.1;
      tanjoubi_tsuki = 8;
      tanjoubi_hi = 21;
      ketsueki = "B";
    };
    {
      namae = "八三一郎";
      shincho = 183.1;
      taiju = 83.1;
      tanjoubi_tsuki = 8;
      tanjoubi_hi = 31;
      ketsueki = "A";
    };
    {
      namae = "九十郎";
      shincho = 191.0;
      taiju = 91.0;
      tanjoubi_tsuki = 9;
      tanjoubi_hi = 10;
      ketsueki = "O";
    };
  ]

(* 目的：誕生日の月 tsuki と日 hi を受け取ったら星座を返す *)
(* seiza : int -> int -> string *)
let seiza tsuki hi =
  if tsuki = 3 then
    if 1 <= hi && hi <= 20 then "魚座"
    else if 21 <= hi && hi <= 31 then "牡羊座"
    else ""
  else if tsuki = 4 then
    if 1 <= hi && hi <= 19 then "牡羊座"
    else if 20 <= hi && hi <= 30 then "牡牛座"
    else ""
  else if tsuki = 5 then
    if 1 <= hi && hi <= 20 then "牡牛座"
    else if 21 <= hi && hi <= 31 then "双子座"
    else ""
  else if tsuki = 6 then
    if 1 <= hi && hi <= 21 then "双子座"
    else if 22 <= hi && hi <= 30 then "蟹座"
    else ""
  else if tsuki = 7 then
    if 1 <= hi && hi <= 22 then "蟹座"
    else if 23 <= hi && hi <= 31 then "獅子座"
    else ""
  else if tsuki = 8 then
    if 1 <= hi && hi <= 22 then "獅子座"
    else if 23 <= hi && hi <= 31 then "乙女座"
    else ""
  else if tsuki = 9 then
    if 1 <= hi && hi <= 22 then "乙女座"
    else if 23 <= hi && hi <= 30 then "天秤座"
    else ""
  else if tsuki = 10 then
    if 1 <= hi && hi <= 23 then "天秤座"
    else if 24 <= hi && hi <= 31 then "蠍座"
    else ""
  else if tsuki = 11 then
    if 1 <= hi && hi <= 22 then "蠍座"
    else if 23 <= hi && hi <= 30 then "射手座"
    else ""
  else if tsuki = 12 then
    if 1 <= hi && hi <= 21 then "射手座"
    else if 22 <= hi && hi <= 31 then "山羊座"
    else ""
  else if tsuki = 1 then
    if 1 <= hi && hi <= 19 then "山羊座"
    else if 20 <= hi && hi <= 31 then "水瓶座"
    else ""
  else if tsuki = 2 then
    if 1 <= hi && hi <= 18 then "水瓶座"
    else if 19 <= hi && hi <= 30 then "魚座"
    else ""
  else if tsuki = 3 then
    if 1 <= hi && hi <= 20 then "魚座"
    else if 21 <= hi && hi <= 31 then "牡羊座"
    else ""
  else ""

(* 目的：人のリスト lst のうち乙女座の人の名前のみからなるリストを返す *)
(* otomeza : person_t list -> string list *)
let rec otomeza lst =
  match lst with
  | [] -> []
  | {
      namae = n;
      shincho = s;
      taiju = t;
      tanjoubi_tsuki = tsuki;
      tanjoubi_hi = hi;
      ketsueki = k;
    }
    :: rest ->
      if seiza tsuki hi = "乙女座" then n :: otomeza rest else otomeza rest

(* テスト *)
let test1 = otomeza lst1 = []
let test2 = otomeza lst2 = [ "八三一郎" ]
let test3 = otomeza lst3 = [ "八三一郎" ]
let test4 = otomeza lst4 = [ "八三一郎"; "九十郎" ]
