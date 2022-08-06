(* 駅名、最短距離、手前の駅名のリスト *)
type eki_t = {
  namae : string; (* 駅名（漢字の文字列） *)
  saitan_kyori : float; (* 最短距離（実数） *)
  temae_list : string list; (* 駅名（漢字の文字列）のリスト *)
}

(* 駅のリストを「最短距離最小の駅」と「最短距離最小の駅以外からなるリスト」に分離する *)
(* saitan_wo_bunri : eki_t list -> eki_t * eki_t list *)
let saitan_wo_bunri lst =
  match lst with
  | [] -> ({ namae = ""; saitan_kyori = infinity; temae_list = [ "" ] }, [])
  | first :: rest ->
      List.fold_right
        (fun first (p, v) ->
          match (first, p) with
          | ( { namae = first_n; saitan_kyori = first_s; temae_list = first_t },
              {
                namae = second_n;
                saitan_kyori = second_s;
                temae_list = second_t;
              } ) ->
              if first_s < second_s then (first, p :: v) else (p, first :: v))
        rest (first, [])

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
