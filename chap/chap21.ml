(* 21.1 *)

print_string "こんにちは";;
"新" ^ "横浜";;

(* 21.2 *)

();;
print_string;;
print_string "こんにちは";;
print_newline;;
print_newline ()

(* 21.3 *)

let saitan_kyori = 3.2;;

print_string "最短距離は ";
print_float saitan_kyori;
print_string "km です。";
print_newline ();
0
;;

1 + 2;
3 + 4

(* 21.4 *)

(* 目的：ふたつの自然数 m と n の最大公約数を求める *)
(* gcd : int -> int -> int *)
let rec gcd m n =
  print_string "m = ";
  print_int m;
  print_string ", n = ";
  print_int n;
  print_newline ();
  if n = 0 then m else gcd n (m mod n)

(* 21.5 *)

(* 21.6 *)
