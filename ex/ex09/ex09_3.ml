type person_t = {
  namae : string;
  shincho : float;
  taiju : float;
  tanjoubi_tsuki : int;
  tanjoubi_hi : int;
  ketsueki : string;
}

let _ = [ "春"; "夏"; "秋"; "冬" ]

let _ =
  [
    {
      namae = "春夫";
      shincho = 168.0;
      taiju = 60.0;
      tanjoubi_tsuki = 4;
      tanjoubi_hi = 16;
      ketsueki = "A";
    };
    {
      namae = "夏樹";
      shincho = 154.0;
      taiju = 48.0;
      tanjoubi_tsuki = 7;
      tanjoubi_hi = 8;
      ketsueki = "B";
    };
    {
      namae = "秋子";
      shincho = 160.0;
      taiju = 46.0;
      tanjoubi_tsuki = 11;
      tanjoubi_hi = 8;
      ketsueki = "O";
    };
  ]
