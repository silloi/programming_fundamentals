(* 目的：配列 arr にフィボナッチ数を順に入れて返す *)
(* fib_array : int array -> int array *)
let fib_array arr =
  let length = Array.length arr in
  (* 目的：配列 arr の n 番目のフィボナッチ数を求める *)
  (* fib : int array -> int -> int *)
  let rec fib_arr arr n =
    if n < length then (
      if n < 2 then arr.(n) <- n else arr.(n) <- arr.(n - 1) + arr.(n - 2);
      fib_arr arr (n + 1))
    else ()
  in
  fib_arr arr 0;
  arr

let test1 =
  fib_array [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |]
  = [| 0; 1; 1; 2; 3; 5; 8; 13; 21; 34 |]
