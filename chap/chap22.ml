(* 22.1 *)

let a = [ 1; 2; 3 ]
let b = List.map (fun x -> x + 1) a;;

a

(* 22.2 *)

let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2);;

fib 8

(* 目的：フィボナッチ数を再帰回数とともに求める *)
(* fib : int -> int -> (int * int) *)
let rec fib n c =
  (* c はこれまでに呼ばれた回数 *)
  let c0 = c + 1 in
  (* カウンタに 1 を加える *)
  if n < 2 then (n, c0) (* カウンタを一緒に返す *)
  else
    let r1, c1 = fib (n - 1) c0 in
    (* c0 からはじめて fib (n - 1) 中での呼び出し回数を数える *)
    let r2, c2 = fib (n - 2) c1 in
    (* c1 からはじめて fib (n - 2) 中での呼び出し回数を数える *)
    (r1 + r2, c2)
(* c2 が全体の呼び出し回数*)
;;

fib 8 0;;

(* 22.3 *)

ref 0

let count = ref 0;;

!count

let count = ref 0;;

!count

let rec fib2 n =
  count := !count + 1;
  if n < 2 then n else fib2 (n - 1) + fib2 (n - 2)
;;

fib2 8;;
!count;;

(* 22.4 *)

fib2 8;;
!count

let c = [ ref 1; ref 2; ref 3 ];;

List.iter (fun x -> x := !x + 1) c;;
c

let d = ref 0 :: c;;

List.iter (fun x -> x := !x + 2) d;;
d;;
c

(* 22.5 *)

type eki_t = {
  namae : string;
  mutable saitan_kyori : float;
  mutable temae_list : string list;
}

let a = { namae = "A"; saitan_kyori = 3.2; temae_list = [] };;

a.saitan_kyori <- 4.1;;
a

(* 22.6 *)

let a = [| 3; 5; 2; 4 |];;

Array.get a 3;;
Array.get a 4;;
a.(3);;

(* 22.7 *)

a;;
Array.set a 1 7;;
a;;
a.(2) <- 6;;
a
