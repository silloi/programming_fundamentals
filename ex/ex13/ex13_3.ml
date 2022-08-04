(* 'a -> 'a *)
let id x = x

(* 'a -> 'b -> 'a *)
let aba a b = a

(* 'a -> 'b -> 'b *)
let abb a b = b

(* 'a -> ('a -> 'b) -> 'b *)
let aabb x y = y x

(* ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c *)
let abbcac x y z = y (x z)
