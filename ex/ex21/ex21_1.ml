(* 駅名、最短距離、手前の駅名のリスト *)
type eki_t = {
  namae : string; (* 駅名（漢字の文字列） *)
  saitan_kyori : float; (* 最短距離（実数） *)
  temae_list : string list; (* 駅名（漢字の文字列）のリスト *)
}

(* eki_t 型の結果 eki をきれいに出力する *)
(* print_eki : eki_t -> unit *)
let print_eki eki =
  match eki with
  | { namae = n; saitan_kyori = s; temae_list = t } ->
      print_string (n ^ "までの最短距離は ");
      print_float s;
      print_string "km です。経由駅：";
      List.iter
        (fun s ->
          print_newline ();
          print_string s)
        t
