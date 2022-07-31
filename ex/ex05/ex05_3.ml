(* 目的：誕生日の月 tsuki と日 hi を受け取ったら星座を返す *)
(* seiza : int -> int -> string *)
let seiza tsuki hi =
  if tsuki = 3 then
    if 1 <= hi && hi <= 20 then "魚座"
    else if 21 <= hi && hi <= 31 then "牡羊座"
    else ""
  else if tsuki = 4 then
    if 1 <= hi && hi <= 19 then "牡羊座"
    else if 20 <= hi && hi <= 30 then "牡牛座"
    else ""
  else if tsuki = 5 then
    if 1 <= hi && hi <= 20 then "牡牛座"
    else if 21 <= hi && hi <= 31 then "双子座"
    else ""
  else if tsuki = 6 then
    if 1 <= hi && hi <= 21 then "双子座"
    else if 22 <= hi && hi <= 30 then "蟹座"
    else ""
  else if tsuki = 7 then
    if 1 <= hi && hi <= 22 then "蟹座"
    else if 23 <= hi && hi <= 31 then "獅子座"
    else ""
  else if tsuki = 8 then
    if 1 <= hi && hi <= 22 then "獅子座"
    else if 23 <= hi && hi <= 31 then "乙女座"
    else ""
  else if tsuki = 9 then
    if 1 <= hi && hi <= 22 then "乙女座"
    else if 23 <= hi && hi <= 30 then "天秤座"
    else ""
  else if tsuki = 10 then
    if 1 <= hi && hi <= 23 then "天秤座"
    else if 24 <= hi && hi <= 31 then "蠍座"
    else ""
  else if tsuki = 11 then
    if 1 <= hi && hi <= 22 then "蠍座"
    else if 23 <= hi && hi <= 30 then "射手座"
    else ""
  else if tsuki = 12 then
    if 1 <= hi && hi <= 21 then "射手座"
    else if 22 <= hi && hi <= 31 then "山羊座"
    else ""
  else if tsuki = 1 then
    if 1 <= hi && hi <= 19 then "山羊座"
    else if 20 <= hi && hi <= 31 then "水瓶座"
    else ""
  else if tsuki = 2 then
    if 1 <= hi && hi <= 18 then "水瓶座"
    else if 19 <= hi && hi <= 30 then "魚座"
    else ""
  else if tsuki = 3 then
    if 1 <= hi && hi <= 20 then "魚座"
    else if 21 <= hi && hi <= 31 then "牡羊座"
    else ""
  else ""

(* テスト *)
let test1 = seiza 3 21 = "牡羊座"
let test2 = seiza 4 1 = "牡羊座"
let test3 = seiza 4 19 = "牡羊座"
let test4 = seiza 4 20 = "牡牛座"
let test5 = seiza 5 1 = "牡牛座"
let test6 = seiza 5 20 = "牡牛座"
let test7 = seiza 5 21 = "双子座"
let test8 = seiza 6 1 = "双子座"
let test9 = seiza 6 21 = "双子座"
let test10 = seiza 6 22 = "蟹座"
let test11 = seiza 7 1 = "蟹座"
let test12 = seiza 7 22 = "蟹座"
let test13 = seiza 7 23 = "獅子座"
let test14 = seiza 8 1 = "獅子座"
let test15 = seiza 8 22 = "獅子座"
let test16 = seiza 8 23 = "乙女座"
let test17 = seiza 9 1 = "乙女座"
let test18 = seiza 9 22 = "乙女座"
let test19 = seiza 9 23 = "天秤座"
let test20 = seiza 10 1 = "天秤座"
let test21 = seiza 10 23 = "天秤座"
let test22 = seiza 10 24 = "蠍座"
let test23 = seiza 11 1 = "蠍座"
let test24 = seiza 11 22 = "蠍座"
let test25 = seiza 11 23 = "射手座"
let test26 = seiza 12 1 = "射手座"
let test27 = seiza 12 21 = "射手座"
let test28 = seiza 12 23 = "山羊座"
let test29 = seiza 1 1 = "山羊座"
let test30 = seiza 1 19 = "山羊座"
let test31 = seiza 1 20 = "水瓶座"
let test32 = seiza 2 1 = "水瓶座"
let test33 = seiza 2 18 = "水瓶座"
let test34 = seiza 2 19 = "魚座"
let test35 = seiza 3 1 = "魚座"
let test36 = seiza 3 20 = "魚座"
