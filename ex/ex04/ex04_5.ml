let jikyu = 850

let baito_kyuyo nensu jikan =
  (jikyu + (nensu * 100)) * jikan (* int -> int -> int *)

let jikoshokai namae = "私の名前は" ^ namae ^ "です。" (* string -> string *)
let hyojun_taiju shincho = (shincho ** 2.) *. 22. (* float -> float *)
let bmi shincho taiju = taiju /. (shincho ** 2.) (* float -> float -> float *)
