(* 駅名、最短距離、手前の駅名のリスト *)
type eki_t = {
  namae : string; (* 駅名（漢字の文字列） *)
  saitan_kyori : float; (* 最短距離（実数） *)
  temae_list : string list; (* 駅名（漢字の文字列）のリスト *)
}

(* 目的：eki_t 型のリスト lst のうち始点 shiten のみ saitan_kyori が 0、temae_list は始点の駅名のみからなるリストにして初期化する *)
(* let shokika : eki_t list -> string -> eki_t list *)
let rec shokika lst shiten =
  match lst with
  | [] -> []
  | ({ namae = n; saitan_kyori = s; temae_list = t } as first) :: rest ->
      let shokika_rest = shokika rest shiten in
      if n = shiten then
        { namae = n; saitan_kyori = 0.0; temae_list = [ n ] } :: shokika_rest
      else first :: shokika_rest

(* テスト *)
let test1 = shokika [] "" = []
let test2 = shokika [] "茗荷谷" = []

let test3 =
  shokika [ { namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] } ] ""
  = [ { namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] } ]

let test4 =
  shokika [ { namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] } ] "茗荷谷"
  = [ { namae = "茗荷谷"; saitan_kyori = 0.0; temae_list = [ "茗荷谷" ] } ]

let test5 =
  shokika
    [
      { namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] };
      { namae = "後楽園"; saitan_kyori = infinity; temae_list = [] };
    ]
    "後楽園"
  = [
      { namae = "茗荷谷"; saitan_kyori = infinity; temae_list = [] };
      { namae = "後楽園"; saitan_kyori = 0.0; temae_list = [ "後楽園" ] };
    ]
