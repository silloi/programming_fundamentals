type person_t = {
  namae : string;
  shincho : float;
  taiju : float;
  tanjoubi_tsuki : int;
  tanjoubi_hi : int;
  ketsueki : string;
}

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

(* 目的：人のリスト lst 中の ketsueki0 型の人の数を返す *)
(* count_ketsueki : person_t list -> string -> int *)
let rec count_ketsueki lst ketsueki0 =
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
      if k = ketsueki0 then 1 + count_ketsueki rest ketsueki0
      else count_ketsueki rest ketsueki0

(* テスト *)
let test1 = count_ketsueki [ person1 ] "A" = 1
let test2 = count_ketsueki [ person2 ] "A" = 0
let test3 = count_ketsueki [ person1; person2 ] "B" = 1
let test4 = count_ketsueki [ person1; person2; person2 ] "B" = 2
