(* 駅名、最短距離、手前の駅名のリスト *)
type eki_t = {
  namae : string; (* 駅名（漢字の文字列） *)
  saitan_kyori : float; (* 最短距離（実数） *)
  temae_list : string list; (* 駅名（漢字の文字列）のリスト *)
}

(* 目的：最短距離が昇順の駅のリスト lst に 駅 eki を挿入したリストを返す *)
(* insert eki_t list -> eki_t -> eki_t list *)
let rec eki_insert lst eki =
  match lst with
  | [] -> [ eki ]
  | ({ namae = first_n; saitan_kyori = first_s; temae_list = first_t } as first)
    :: rest ->
      let { namae = eki_n; saitan_kyori = eki_s; temae_list = eki_t } = eki in
      if first_s < eki_s then first :: eki_insert rest eki
      else eki :: first :: rest

(* 目的：駅のリスト lst を最短距離の昇順に整列して返す *)
(* eki_sort : eki_t list -> eki_t list *)
let rec eki_ins_sort lst =
  match lst with
  | [] -> []
  | first :: rest -> eki_insert (eki_ins_sort rest) first

(* 駅のリストを「最短距離最小の駅」と「最短距離最小の駅以外からなるリスト」に分離する *)
(* saitan_wo_bunri : eki_t list -> eki_t * eki_t list *)
let rec saitan_wo_bunri lst =
  match lst with
  | [] -> ({ namae = ""; saitan_kyori = infinity; temae_list = [ "" ] }, [])
  | ({ namae = n; saitan_kyori = s; temae_list = t } as first) :: rest -> (
      if rest = [] then (first, [])
      else
        let eki_sort = eki_ins_sort lst in
        match eki_sort with
        | [] ->
            ({ namae = ""; saitan_kyori = infinity; temae_list = [ "" ] }, [])
        | first :: rest -> (first, rest))

let test1 =
  saitan_wo_bunri
    [ { namae = "茗荷谷"; saitan_kyori = 0.0; temae_list = [ "茗荷谷" ] } ]
  = ({ namae = "茗荷谷"; saitan_kyori = 0.0; temae_list = [ "茗荷谷" ] }, [])

let test2 =
  saitan_wo_bunri
    [
      { namae = "茗荷谷"; saitan_kyori = 0.0; temae_list = [ "茗荷谷" ] };
      { namae = "後楽園"; saitan_kyori = 1.8; temae_list = [ "後楽園"; "茗荷谷" ] };
    ]
  = ( { namae = "茗荷谷"; saitan_kyori = 0.0; temae_list = [ "茗荷谷" ] },
      [ { namae = "後楽園"; saitan_kyori = 1.8; temae_list = [ "後楽園"; "茗荷谷" ] } ]
    )

let test3 =
  saitan_wo_bunri
    [
      { namae = "後楽園"; saitan_kyori = 1.8; temae_list = [ "後楽園"; "茗荷谷" ] };
      { namae = "新大塚"; saitan_kyori = 1.2; temae_list = [ "新大塚"; "茗荷谷" ] };
    ]
  = ( { namae = "新大塚"; saitan_kyori = 1.2; temae_list = [ "新大塚"; "茗荷谷" ] },
      [ { namae = "後楽園"; saitan_kyori = 1.8; temae_list = [ "後楽園"; "茗荷谷" ] } ]
    )
