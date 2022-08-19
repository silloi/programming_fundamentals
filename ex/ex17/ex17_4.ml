(* 日を示す引数を取る、1 月から 12 月までを表す構成子を持つ型 *)
type year_t =
  | January of int (* 1月 *)
  | February of int (* 2月 *)
  | March of int (* 3月 *)
  | April of int (* 4月 *)
  | May of int (* 5月 *)
  | June of int (* 6月 *)
  | July of int (* 7月 *)
  | August of int (* 8月 *)
  | September of int (* 9月 *)
  | October of int (* 10月 *)
  | November of int (* 11月 *)
  | December of int (* 12月 *)

(* 12 星座を示す型 *)
type seiza_t =
  | Aries (* 牡羊座*)
  | Taurus (* 牡牛座 *)
  | Gemini (* 双子座 *)
  | Cancer (* 蟹座 *)
  | Leo (* 獅子座 *)
  | Virgo (* 乙女座 *)
  | Libra (* 天秤座 *)
  | Scorpio (* 蠍座 *)
  | Sagittarius (* 射手座 *)
  | Capricorn (* 山羊座 *)
  | Aquarius (* 水瓶座 *)
  | Pisces (* 魚座 *)

(* 目的：月日 year を受け取ったら星座を返す *)
(* seiza : year_t -> seiza_t *)
let seiza year =
  match year with
  | March n ->
      if 1 <= n && n <= 20 then Pisces
      else if 21 <= n && n <= 31 then Aries
      else Aries
  | April n ->
      if 1 <= n && n <= 19 then Aries
      else if 20 <= n && n <= 30 then Taurus
      else Taurus
  | May n ->
      if 1 <= n && n <= 20 then Taurus
      else if 21 <= n && n <= 31 then Gemini
      else Gemini
  | June n ->
      if 1 <= n && n <= 21 then Gemini
      else if 22 <= n && n <= 30 then Cancer
      else Cancer
  | July n ->
      if 1 <= n && n <= 22 then Cancer
      else if 23 <= n && n <= 31 then Leo
      else Leo
  | August n ->
      if 1 <= n && n <= 22 then Leo
      else if 23 <= n && n <= 31 then Virgo
      else Virgo
  | September n ->
      if 1 <= n && n <= 22 then Virgo
      else if 23 <= n && n <= 30 then Libra
      else Libra
  | October n ->
      if 1 <= n && n <= 23 then Libra
      else if 24 <= n && n <= 31 then Scorpio
      else Scorpio
  | November n ->
      if 1 <= n && n <= 22 then Scorpio
      else if 23 <= n && n <= 30 then Sagittarius
      else Sagittarius
  | December n ->
      if 1 <= n && n <= 21 then Sagittarius
      else if 22 <= n && n <= 31 then Capricorn
      else Capricorn
  | January n ->
      if 1 <= n && n <= 19 then Capricorn
      else if 20 <= n && n <= 31 then Aquarius
      else Aquarius
  | February n ->
      if 1 <= n && n <= 18 then Aquarius
      else if 19 <= n && n <= 30 then Pisces
      else Pisces

(* テスト *)
let test1 = seiza (March 21) = Aries
let test2 = seiza (April 1) = Aries
let test3 = seiza (April 19) = Aries
let test4 = seiza (April 20) = Taurus
let test5 = seiza (May 1) = Taurus
let test6 = seiza (May 20) = Taurus
let test7 = seiza (May 21) = Gemini
let test8 = seiza (June 1) = Gemini
let test9 = seiza (June 21) = Gemini
let test10 = seiza (June 22) = Cancer
let test11 = seiza (July 1) = Cancer
let test12 = seiza (July 22) = Cancer
let test13 = seiza (July 23) = Leo
let test14 = seiza (August 1) = Leo
let test15 = seiza (August 22) = Leo
let test16 = seiza (August 23) = Virgo
let test17 = seiza (September 1) = Virgo
let test18 = seiza (September 22) = Virgo
let test19 = seiza (September 23) = Libra
let test20 = seiza (October 1) = Libra
let test21 = seiza (October 23) = Libra
let test22 = seiza (October 24) = Scorpio
let test23 = seiza (November 1) = Scorpio
let test24 = seiza (November 22) = Scorpio
let test25 = seiza (November 23) = Sagittarius
let test26 = seiza (December 1) = Sagittarius
let test27 = seiza (December 21) = Sagittarius
let test28 = seiza (December 23) = Capricorn
let test29 = seiza (January 1) = Capricorn
let test30 = seiza (January 19) = Capricorn
let test31 = seiza (January 20) = Aquarius
let test32 = seiza (February 1) = Aquarius
let test33 = seiza (February 18) = Aquarius
let test34 = seiza (February 19) = Pisces
let test35 = seiza (March 1) = Pisces
let test36 = seiza (March 20) = Pisces
