type person_t = {
  namae : string;
  shincho : float;
  taiju : float;
  tanjoubi_tsuki : int;
  tanjoubi_hi : int;
  ketsueki : string;
}
;;

fun person ->
  match person with
  | {
   namae = n;
   shincho = s;
   taiju = t;
   tanjoubi_tsuki = tsuki;
   tanjoubi_hi = hi;
   ketsueki = k;
  } ->
      n
