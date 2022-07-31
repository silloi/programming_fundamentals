(* 目的：x 座標と y 座標の組で表された平面座標を受け取ったら x 軸について対象な点の座標を返す *)
(* taisho_x : float * float -> float * float *)
let taisho_x zahyo = match zahyo with x, y -> (x, -.y)

(* テスト *)
let test1 = taisho_x (0.0, 0.0) = (0.0, 0.0)
let test1 = taisho_x (1.0, 2.0) = (1.0, -2.0)
let test1 = taisho_x (-3.0, -4.0) = (-3.0, 4.0)
