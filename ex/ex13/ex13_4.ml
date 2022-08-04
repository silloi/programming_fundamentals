(* ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b *)
let compose f g x = f (g x)
