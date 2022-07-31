(* 目的：x 座標と y 座標の組で表された平面座標をふたつ受け取ったらその中点の座標を返す *)
(* chuten : float * float -> float * float -> float * float *)
let chuten zahyo1 zahyo2 =
  match zahyo1 with
  | x1, y1 -> (
      match zahyo2 with x2, y2 -> ((x1 +. x2) /. 2., (y1 +. y2) /. 2.))

(* テスト *)
let test1 = chuten (1.0, 2.0) (3.0, 4.0) = (2.0, 3.0)
let test2 = chuten (1.0, 2.0) (-3.0, -4.0) = (-1.0, -1.0)
let test1 = chuten (1.0, -2.0) (-1.0, -2.0) = (0.0, -2.0)
