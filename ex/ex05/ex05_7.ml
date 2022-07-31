let bmi shincho taiju = taiju /. (shincho ** 2.) (* float -> float -> float *)

(* 目的：身長 shincho（m）と体重 taiju（kg）を受け取ったら BMI 指数を計算し、その数値によって体型を返す *)
(* taikei : float -> float -> string *)

let taikei shincho taiju =
  if bmi shincho taiju < 18.5 then "やせ"
  else if bmi shincho taiju < 25.0 then "標準"
  else if bmi shincho taiju < 30.0 then "肥満"
  else "高度肥満"

(* テスト *)
let test1 = taikei 1.5 40.0 = "やせ"
let test2 = taikei 1.5 41.625 = "標準"
let test3 = taikei 1.5 45.0 = "標準"
let test4 = taikei 1.5 56.25 = "肥満"
let test5 = taikei 1.5 65.0 = "肥満"
let test5 = taikei 1.5 67.5 = "高度肥満"
let test4 = taikei 1.5 70.0 = "高度肥満"
