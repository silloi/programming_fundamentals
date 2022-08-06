(* 16.1 *)

(* 距離と距離の合計を持っている型 *)
type distance_t = { kyori : float; (* 距離 *) total : float (* 距離の合計 *) }

(* 目的：先頭からリスト中の各点までの距離の合計を計算する *)
(* total_distance : distance_t list -> distance_t list *)
let rec total_distance lst =
  match lst with
  | [] -> []
  | { kyori = k; total = t } :: rest -> [] (* total_distance rest *)

(* 16.2 *)

(* 目的：先頭からリスト中の各点までの距離の合計を計算する *)
(* total_distance : distance_t list -> distance_t list *)
let total_distance lst =
  (* 目的：先頭からリスト中の各点までの距離の合計を計算する *)
  (* ここで total0 はこれまでの距離の合計 *)
  (* hojo : distance_t list -> float -> distance_t list *)
  let rec hojo lst total0 =
    match lst with
    | [] -> []
    | { kyori = k; total = t } :: rest ->
        { kyori = k; total = total0 +. k } :: hojo rest (total0 +. k)
  in
  hojo lst 0.0

(* 16.3 *)

(* 目的：与えられたリストを逆順にして返す *)
(* reverse : 'a list -> 'a list *)
let rec reverse lst =
  match lst with
  | [] -> []
  | first :: rest ->
      (* 目的：（lst の逆順のリスト） @ result を返す *)
      (* ここで result はこれまでの要素を逆順にしたリストを示す *)
      let rec rev lst result =
        match lst with
        | [] -> result
        | first :: rest -> rev rest (first :: result)
      in
      rev lst []
