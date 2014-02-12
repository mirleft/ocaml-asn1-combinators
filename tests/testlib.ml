
open OUnit2
open Bigarray


type testcase =
  | TC : string * 'a Asn.t * ('a * int list) list -> testcase

let case
: type a. string -> a Asn.t -> (a * int list) list -> testcase
= fun name asn examples -> TC (name, asn, examples)


let assert_decode
: type a. a Asn.codec -> Asn.bytes -> a -> unit
= fun codec bytes a ->
  match Asn.decode codec bytes with
  | None -> assert_failure "decode failed"
  | Some (x, buf) ->
      if Array1.dim buf <> 0 then
        assert_failure "not all input consumed"
      else assert_equal a x

let test_decode (TC (_, asn, examples)) _=
  let codec = Asn.(codec ber_der asn) in
  examples |> List.iter @@ fun (a, bytes) ->
    let arr = Dumpkit.bytes_of_list bytes in
    assert_decode codec arr a

let test_loop_decode (TC (_, asn, _)) _ =
  let codec = Asn.(codec ber_der asn) in
  for i = 1 to 1000 do
    let a = Asn.random asn in
    assert_decode codec (Asn.encode codec a) a
  done


let cases = [

  case "bool" Asn.bool [
    false, [0x01; 0x01; 0x00] ;
    true , [0x01; 0x01; 0xff]
  ];

  case
    "singleton seq"
    Asn.(sequence (single @@ required bool))
    [ true, [ 0x30; 0x03; 0x01; 0x01; 0xff; ];
      true, [ 0x30; 0x80; 0x01; 0x01; 0xff; 0x00; 0x00; ] ;
    ];

  case
    "sequence with implicits"
    Asn.(sequence3
          (required int)
          (required @@ implicit 1 bool)
          (required bool))

    [ (`I 42, false, true),
      [ 0x30; 0x09;
          0x02; 0x01; 0x2a;
          0x81; 0x01; 0x00;
          0x01; 0x01; 0xff; ] ;

      (`I 42, false, true),
      [ 0x30; 0x80;
          0x02; 0x01; 0x2a;
          0x81; 0x01; 0x00;
          0x01; 0x01; 0xff;
          0x00; 0x00; ]
    ];

  case
    "sequence with optional fields"
    Asn.(sequence3
          (required @@ implicit 1 int)
          (optional @@ explicit 2 bool)
          (optional @@ implicit 3 bool))

    [ (`I 255, Some true, Some false),
      [ 0x30; 0x0c;
          0x81; 0x02; 0x00; 0xff;
          0xa2; 0x03;
            0x01; 0x01; 0xf0;
          0x83; 0x01; 0x00; ] ;

      (`I 255, Some true, Some false),
      [ 0x30; 0x80;
          0x81; 0x02; 0x00; 0xff;
          0xa2; 0x03;
            0x01; 0x01; 0xf0;
          0x83; 0x01; 0x00;
          0x00; 0x00; ] ;

      (`I 255, Some true, Some false),
      [ 0x30; 0x80;
          0x81; 0x02; 0x00; 0xff;
          0xa2; 0x80;
            0x01; 0x01; 0xf0;
            0x00; 0x00;
          0x83; 0x01; 0x00;
          0x00; 0x00; ] ;
    ]


]


let suite =
  "ASN.1" >::: [
    "BER decoding" >:::
      List.map
        (fun (TC (name, _, _) as tc) -> name >:: test_decode tc)
        cases ;
    "BER/DER encode->decode" >:::
      List.map
        (fun (TC (name, _, _) as tc) -> name >:: test_loop_decode tc)
        cases
  ]

