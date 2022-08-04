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

(* 目的：人のリスト lst 中に出てくる人の名前のリストを返す *)
(* person_namae : person_t list -> string list *)
let person_namae lst =
  let namae person =
    match person with
    | {
     namae = n;
     shincho = s;
     taiju = t;
     tanjoubi_tsuki = tsuki;
     tanjoubi_hi = hi;
     ketsueki = k;
    } ->
        n
  in
  List.map namae lst

(* テスト *)
let test1 = person_namae [ person1 ] = [ "春夫" ]
let test2 = person_namae [ person1; person2 ] = [ "春夫"; "夏樹" ]

let test3 = person_namae [ person1; person2; person3 ] = [ "春夫"; "夏樹"; "秋子" ]
