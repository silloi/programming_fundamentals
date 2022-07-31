(* 8.1 *)

let _ = ("asai", 70)

(* 8.2 *)
type gakusei_t = { namae : string; tensuu : int; seiseki : string }

let _ = { namae = "asai"; tensuu = 70; seiseki = "B" }
let _ = { tensuu = 70; namae = "asai"; seiseki = "B" }

(* let _ = {namae = "asai"} *)

(* 8.3 *)
let tsuuchi gakusei =
  match gakusei with
  | { namae = n; tensuu = t; seiseki = s } ->
      n ^ "さんは " ^ string_of_int t ^ " 点で、成績は " ^ s ^ " です。"

let _ = tsuuchi { namae = "asai"; tensuu = 70; seiseki = "B" }

let hyouka gakusei =
  match gakusei with
  | { namae = n; tensuu = t; seiseki = s } ->
      {
        namae = n;
        tensuu = t;
        seiseki =
          (if t >= 80 then "A"
          else if t >= 70 then "B"
          else if t >= 60 then "C"
          else "D");
      }

let _ = hyouka { namae = "asai"; tensuu = 70; seiseki = "B" }
let _ = tsuuchi (hyouka { namae = "asai"; tensuu = 70; seiseki = "" })

(* 8.4 *)

let asai = { namae = "asai"; tensuu = 70; seiseki = "B" }
let _ = asai.namae
let _ = asai.tensuu
let _ = asai.seiseki

(* 8.5 *)

type gakusei_t = { namae : string; tensuu : int; seiseki : string }

(* 8.6 *)

(* 学生ひとり分のデータ（名前、点数、成績）を表す型 *)
type gakusei_t = {
  namae : string; (* 名前 *)
  tensuu : int; (* 点数 *)
  seiseki : string; (* 成績 *)
}

(* 目的：学生のデータ gakusei を受け取り成績のついたデータを返す *)
(* hyoka : gakusei_t -> gakusei_t *)
let hyoka gakusei =
  match gakusei with
  | { namae = n; tensuu = t; seiseki = s } ->
      if 80 <= t then { namae = n; tensuu = t; seiseki = "A" }
      else if 70 <= t then { namae = n; tensuu = t; seiseki = "B" }
      else if 60 <= t then { namae = n; tensuu = t; seiseki = "C" }
      else { namae = n; tensuu = t; seiseki = "D" }

(* テスト *)
let test1 =
  hyoka { namae = "asai"; tensuu = 90; seiseki = "" }
  = { namae = "asai"; tensuu = 90; seiseki = "A" }

let test2 =
  hyoka { namae = "asai"; tensuu = 80; seiseki = "" }
  = { namae = "asai"; tensuu = 80; seiseki = "A" }

let test3 =
  hyoka { namae = "asai"; tensuu = 75; seiseki = "" }
  = { namae = "asai"; tensuu = 75; seiseki = "B" }

let test4 =
  hyoka { namae = "asai"; tensuu = 70; seiseki = "" }
  = { namae = "asai"; tensuu = 70; seiseki = "B" }

let test1 =
  hyoka { namae = "asai"; tensuu = 65; seiseki = "" }
  = { namae = "asai"; tensuu = 65; seiseki = "C" }

let test1 =
  hyoka { namae = "asai"; tensuu = 60; seiseki = "" }
  = { namae = "asai"; tensuu = 60; seiseki = "C" }

let test1 =
  hyoka { namae = "asai"; tensuu = 55; seiseki = "" }
  = { namae = "asai"; tensuu = 55; seiseki = "D" }
