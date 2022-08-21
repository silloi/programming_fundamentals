(* 駅名、最短距離、手前の駅名のリスト *)
type eki_t = {
  namae : string; (* 駅名（漢字の文字列） *)
  saitan_kyori : float; (* 最短距離（実数） *)
  temae_list : string list; (* 駅名（漢字の文字列）のリスト *)
}

(* 駅のリストを「最短距離最小の駅」と「最短距離最小の駅以外からなるリスト」に分離する *)
(* saitan_wo_bunri : eki_t -> eki_t list -> eki_t * eki_t list *)
let rec saitan_wo_bunri eki eki_list =
  match eki_list with
  | [] -> (eki, [])
  | first :: rest -> (
      let p, v = saitan_wo_bunri first rest in
      match (eki, p) with
      | ( { namae = eki_n; saitan_kyori = eki_s; temae_list = eki_t },
          { namae = p_n; saitan_kyori = p_s; temae_list = p_t } ) ->
          if eki_s < p_s then (eki, p :: v) else (p, eki :: v))

let test1 =
  saitan_wo_bunri
    { namae = "茗荷谷"; saitan_kyori = 0.0; temae_list = [ "茗荷谷" ] }
    []
  = ({ namae = "茗荷谷"; saitan_kyori = 0.0; temae_list = [ "茗荷谷" ] }, [])

let test2 =
  saitan_wo_bunri
    { namae = "茗荷谷"; saitan_kyori = 0.0; temae_list = [ "茗荷谷" ] }
    [ { namae = "後楽園"; saitan_kyori = 1.8; temae_list = [ "後楽園"; "茗荷谷" ] } ]
  = ( { namae = "茗荷谷"; saitan_kyori = 0.0; temae_list = [ "茗荷谷" ] },
      [ { namae = "後楽園"; saitan_kyori = 1.8; temae_list = [ "後楽園"; "茗荷谷" ] } ]
    )

let test3 =
  saitan_wo_bunri
    { namae = "後楽園"; saitan_kyori = 1.8; temae_list = [ "後楽園"; "茗荷谷" ] }
    [ { namae = "新大塚"; saitan_kyori = 1.2; temae_list = [ "新大塚"; "茗荷谷" ] } ]
  = ( { namae = "新大塚"; saitan_kyori = 1.2; temae_list = [ "新大塚"; "茗荷谷" ] },
      [ { namae = "後楽園"; saitan_kyori = 1.8; temae_list = [ "後楽園"; "茗荷谷" ] } ]
    )
