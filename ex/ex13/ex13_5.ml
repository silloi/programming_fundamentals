let twice f =
  let g x = f (f x) in
  g

(* ('a -> 'a) -> 'a -> 'a *)
let _ = twice twice
