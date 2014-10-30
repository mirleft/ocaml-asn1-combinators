
(* interactive *)

let bit_dump x =
  let rec go = function
    | i when i < 0 -> []
    | i -> string_of_int ((x lsr i) land 1) :: go (i - 1) in
  String.concat "" @@ go 7

let cstruct_of_list list =
  let cs = Cstruct.create @@ List.length list in
  List.iteri Cstruct.(set_uint8 cs) list ;
  cs

let verify_decode = function
  | None        -> `fail
  | Some (a, b) ->
      if Cstruct.len b = 0 then `ok a else `leftover

let verify_decode_value x dec =
  match verify_decode dec with
  | `ok a when x = a -> `ok
  | `ok a     -> `mismatch (x, a)
  | `fail     -> `fail
  | `leftover -> `leftover

let loop_code codec x =
  let enc = Asn.encode codec x in
  let dec = Asn.decode codec enc in
  verify_decode_value x dec

let loop_code_random ?(coding=Asn.ber) asn =
  loop_code Asn.(codec coding asn) Asn.(random asn)

let rec fuzz ?(coding=Asn.ber) ?(n=1000) asn =
  if n < 0 then [] else
    let rest = fuzz ~coding ~n:(pred n) asn in
    match loop_code_random ~coding asn with
    | `ok  -> rest
    | fail -> fail :: rest


(* the other one *)

open OUnit2

type 'a cmp = 'a -> 'a -> bool

type test_case =
  | TC : string * 'a cmp option * 'a Asn.t * ('a * int list) list -> test_case

let case :
  type a. string -> ?cmp:(a cmp) -> a Asn.t -> (a * int list) list -> test_case
= fun name ?cmp asn examples -> TC (name, cmp, asn, examples)

type test_anticase =
  | ATC : string * 'a Asn.t * int list list -> test_anticase

let anticase :
  type a. string -> a Asn.t -> int list list -> test_anticase
= fun name asn examples -> ATC (name, asn, examples)


let assert_decode
: type a. ?example:a -> ?cmp:(a cmp) -> a Asn.codec -> Cstruct.t -> unit
= fun ?example ?cmp codec bytes ->
  match Asn.decode codec bytes with
  | None -> assert_failure "decode failed"
  | Some (x, buf) ->
      if Cstruct.len buf <> 0 then
        assert_failure "not all input consumed"
      else
        match example with
        | Some a -> assert_equal ?cmp a x
        | None   -> ()

let test_decode encoding (TC (_, cmp, asn, examples)) _ =
  let codec = Asn.(codec encoding asn) in
  examples |> List.iter @@ fun (a, bytes) ->
    let arr = cstruct_of_list bytes in
    assert_decode ~example:a ?cmp codec arr

let test_loop_decode ?(iter=10000) ?cmp encoding asn _ =
  let codec = Asn.(codec encoding asn) in
  for i = 1 to iter do
    let a = Asn.random asn in
    assert_decode ~example:a ?cmp codec (Asn.encode codec a)
  done

let test_no_decode encoding (ATC (_, asn, examples)) _ =
  let codec = Asn.(codec encoding asn) in
  examples |> List.iter @@ fun bytes ->
    match
      try
        (* XXX BER/Prim surface: not all exns caught *)
        Some (Asn.decode_exn codec (cstruct_of_list bytes))
      with _ -> None
    with
    | None   -> ()
    | Some _ -> assert_failure "zalgo he comes"


(* Prim tests *)

let random_time_tests ~n _ = (* round trip with result of Unix.gmtime  *)
  let about_200_years = Int64.(mul 200L (mul 365L 86_400L)) in
  let rtime span = Int64.(sub (Random.int64 (succ span)) (div span 2L)) in
  let test () =
    let ti = rtime about_200_years (* around the unix epoch *) in
    let t  = Int64.to_float ti in
    let tm = Unix.gmtime t in
    let t' = Asn.Time.date_to_posix_time
        ~y:(tm.Unix.tm_year + 1900)
        ~m:(tm.Unix.tm_mon + 1)
        ~d:(tm.Unix.tm_mday)
        ~hh:(tm.Unix.tm_hour)
        ~mm:(tm.Unix.tm_min)
        ~ss:(tm.Unix.tm_sec)
        ~ff:0.
        ~tz_mm:0
    in
    assert_equal t t'
  in
  for i = 1 to n do test () done


let cases = [

  case "bool" Asn.bool [
    false, [0x01; 0x01; 0x00] ;
    true , [0x01; 0x01; 0xff]
  ];

  case "integer" Asn.integer [
    Z.(~$    0),  [0x02; 0x01; 0x00] ;
    Z.(~$    1),  [0x02; 0x01; 0x01] ;
    Z.(~$ (-1)),  [0x02; 0x01; 0xff] ;
    Z.(~$  127),  [0x02; 0x01; 0x7F] ;
    Z.(~$  128),  [0x02; 0x02; 0x00; 0x80] ;
    Z.(~$  256),  [0x02; 0x02; 0x01; 0x00] ;
    Z.(~$(-128)), [0x02; 0x01; 0x80] ;
    Z.(~$(-129)), [0x02; 0x02; 0xFF; 0x7F];
  ];

  case "long integer" Asn.integer [
    Z.of_int64 8366779L,
    [0x02; 0x03; 0x7f; 0xaa; 0xbb];
    Z.of_int64 2141895628L,
    [0x02; 0x04; 0x7f; 0xaa; 0xbb; 0xcc];
    Z.of_int64 548325280989L,
    [0x02; 0x05; 0x7f; 0xaa; 0xbb; 0xcc; 0xdd];
    Z.of_int64 140371271933422L,
    [0x02; 0x06; 0x7f; 0xaa; 0xbb; 0xcc; 0xdd; 0xee];
    Z.of_int64 35935045614956287L,
    [0x02; 0x07; 0x7f; 0xaa; 0xbb; 0xcc; 0xdd; 0xee; 0xff];
    Z.of_int64 9199371677428809489L,
    [0x02; 0x08; 0x7f; 0xaa; 0xbb; 0xcc; 0xdd; 0xee; 0xff; 0x11];
    Z.(of_int64 9199371677428809489L lsl 8),
    [0x02; 0x09; 0x7f; 0xaa; 0xbb; 0xcc; 0xdd; 0xee; 0xff; 0x11; 0x00];
  ];

  case "null" Asn.null [
    (), [ 0x05; 0x00 ];
    (), [ 0x05; 0x81; 0x00 ]
  ];

  case
    "singleton seq"
    Asn.(sequence (single @@ required bool))
    [ true, [ 0x30; 0x03; 0x01; 0x01; 0xff; ];
      true, [ 0x30; 0x80; 0x01; 0x01; 0xff; 0x00; 0x00; ] ;
    ];

  case
    "rename stack"
    Asn.(implicit 1 @@ implicit 2 @@ explicit 3 @@ implicit 4 @@ int)
    [ 42, [ 0xa1; 0x80; 0x84; 0x01; 0x2a; 0x00; 0x00 ];
      42, [ 0xa1; 0x03; 0x84; 0x01; 0x2a ] ];

  case
    "sequence with implicits"
    Asn.(sequence3
          (required int)
          (required @@ implicit 1 bool)
          (required bool))

    [ (42, false, true),
      [ 0x30; 0x09;
          0x02; 0x01; 0x2a;
          0x81; 0x01; 0x00;
          0x01; 0x01; 0xff; ] ;

      (42, false, true),
      [ 0x30; 0x80;
          0x02; 0x01; 0x2a;
          0x81; 0x01; 0x00;
          0x01; 0x01; 0xff;
          0x00; 0x00; ]
    ];

  case
    "sequence with optional and explicit fields"
    Asn.(sequence3
          (required @@ implicit 1 int)
          (optional @@ explicit 2 bool)
          (optional @@ implicit 3 bool))

    [ (255, Some true, Some false),
      [ 0x30; 0x0c;
          0x81; 0x02; 0x00; 0xff;
          0xa2; 0x03;
            0x01; 0x01; 0xf0;
          0x83; 0x01; 0x00; ] ;

      (255, Some true, Some false),
      [ 0x30; 0x80;
          0x81; 0x02; 0x00; 0xff;
          0xa2; 0x03;
            0x01; 0x01; 0xf0;
          0x83; 0x01; 0x00;
          0x00; 0x00; ] ;

      (255, Some true, Some false),
      [ 0x30; 0x80;
          0x81; 0x02; 0x00; 0xff;
          0xa2; 0x80;
            0x01; 0x01; 0xf0;
            0x00; 0x00;
          0x83; 0x01; 0x00;
          0x00; 0x00; ] ;
    ];

  case
    "sequence with missing optional and choice fields"
    Asn.(sequence3
          (required @@ choice2 bool int)
          (optional @@ choice2 bool int)
          (optional @@ explicit 0
                    @@ choice2 int (implicit 1 int)))

    [ (`C1 true, None, None),
      [ 0x30; 0x03; 0x01; 0x01; 0xff ] ;

      (`C2 42, None, None),
      [ 0x30; 0x05; 0x02; 0x03; 0x00; 0x00; 0x2a ] ;

      (`C1 false, Some (`C2 42), None),
      [ 0x30; 0x06;
          0x01; 0x01; 0x00;
          0x02; 0x01; 0x2a ] ;

      (`C1 true, None, Some (`C1 42)),
      [ 0x30; 0x08;
          0x01; 0x01; 0xff;
          0xa0; 0x03;
            0x02; 0x01; 0x2a ] ;

      (`C2 (-2), Some (`C2 42), Some (`C2 42)),
      [ 0x30; 0x0b;
          0x02; 0x01; 0xfe;
          0x02; 0x01; 0x2a;
          0xa0; 0x03;
            0x81; 0x01; 0x2a ] ;

      (`C2 (-3), None, Some (`C2 42)),
      [ 0x30; 0x0a;
          0x02; 0x01; 0xfd;
          0xa0; 0x80;
            0x81; 0x01; 0x2a; 0x00; 0x00; ] ;

      (`C2 (-4), None, Some (`C1 42)),
      [ 0x30; 0x80;
          0x02; 0x01; 0xfc;
          0xa0; 0x80;
            0x02; 0x01; 0x2a; 0x00; 0x00;
          0x00; 0x00 ] ;
    ];

  case
    "sequence with sequence"
    Asn.(sequence2
          (required @@
            sequence2
              (optional @@ implicit 1 bool)
              (optional bool))
          (required bool))

    [ ((Some true, Some false), true),
      [ 0x30; 0x0b ;
          0x30; 0x06;
            0x81; 0x01; 0xff;
            0x01; 0x01; 0x00;
          0x01; 0x01; 0xff ] ;

      ((None, Some false), true),
      [ 0x30; 0x08 ;
          0x30; 0x03;
            0x01; 0x01; 0x00;
          0x01; 0x01; 0xff ] ;

      ((Some true, None), true),
      [ 0x30; 0x08 ;
          0x30; 0x03;
            0x81; 0x01; 0xff;
          0x01; 0x01; 0xff ] ;

      ((Some true, None), true),
      [ 0x30; 0x80 ;
          0x30; 0x80;
            0x81; 0x01; 0xff;
            0x00; 0x00;
          0x01; 0x01; 0xff;
          0x00; 0x00 ] ;

      ((None, None), true),
      [ 0x30; 0x07; 0x30; 0x80; 0x00; 0x00; 0x01; 0x01; 0xff ];

      ((None, None), true),
      [ 0x30; 0x05; 0x30; 0x00; 0x01; 0x01; 0xff ];
    ];

  case
    "sequence_of choice"
    Asn.(sequence2
          (required @@
            sequence_of
              (choice2 bool (implicit 0 bool)))
          (required @@ bool))

    [ ([`C2 true; `C2 false; `C1 true], true),
      [ 0x30; 0x0e;
          0x30; 0x09;
            0x80; 0x01; 0xff;
            0x80; 0x01; 0x00;
            0x01; 0x01; 0xff;
          0x01; 0x01; 0xff ] ;

      ([`C2 true; `C2 false; `C1 true], true),
      [ 0x30; 0x80;
          0x30; 0x80;
            0x80; 0x01; 0xff;
            0x80; 0x01; 0x00;
            0x01; 0x01; 0xff;
            0x00; 0x00;
          0x01; 0x01; 0xff;
          0x00; 0x00; ]
    ];

  case
    "sets"
    Asn.(set4 (required @@ implicit 1 bool)
              (required @@ implicit 2 bool)
              (required @@ implicit 3 int )
              (optional @@ implicit 4 int ))

    [ (true, false, 42, None),
      [ 0x31; 0x09;
          0x81; 0x01; 0xff;
          0x82; 0x01; 0x00;
          0x83; 0x01; 0x2a; ];

      (true, false, 42, Some (-1)),
      [ 0x31; 0x0c;
          0x82; 0x01; 0x00;
          0x84; 0x01; 0xff;
          0x81; 0x01; 0xff;
          0x83; 0x01; 0x2a; ];

      (true, false, 42, None),
      [ 0x31; 0x09;
          0x82; 0x01; 0x00;
          0x83; 0x01; 0x2a;
          0x81; 0x01; 0xff; ];

      (true, false, 42, Some 15),
      [ 0x31; 0x0c;
          0x83; 0x01; 0x2a;
          0x82; 0x01; 0x00;
          0x81; 0x01; 0xff;
          0x84; 0x01; 0x0f; ];

      (true, false, 42, None),
      [ 0x31; 0x80;
          0x82; 0x01; 0x00;
          0x83; 0x01; 0x2a;
          0x81; 0x01; 0xff;
          0x00; 0x00 ];

      (true, false, 42, Some 15),
      [ 0x31; 0x80;
          0x83; 0x01; 0x2a;
          0x82; 0x01; 0x00;
          0x81; 0x01; 0xff;
          0x84; 0x01; 0x0f;
          0x00; 0x00 ];
    ];

  case
    "set or seq"
    Asn.(choice2
          (set2 (optional int )
                (optional bool))
          (sequence2 (optional int )
                     (optional bool)))

    [ (`C1 (None, Some true)),
      [ 0x31; 0x03;
          0x01; 0x01; 0xff; ];

      (`C1 (Some 42, None)),
      [ 0x31; 0x03;
          0x02; 0x01; 0x2a; ];

      (`C1 (Some 42, Some true)),
      [ 0x31; 0x06;
          0x01; 0x01; 0xff;
          0x02; 0x01; 0x2a; ];

      (`C2 (None, Some true)),
      [ 0x30; 0x03;
          0x01; 0x01; 0xff; ];

      (`C2 (Some 42, None)),
      [ 0x30; 0x03;
          0x02; 0x01; 0x2a; ];

      (`C2 (Some 42, Some true)),
      [ 0x30; 0x06;
          0x02; 0x01; 0x2a;
          0x01; 0x01; 0xff; ];
    ];

  case
    "large tag"
    Asn.(implicit 6666666 bool)
    [ true , [ 0x9f; 0x83; 0x96; 0xf3; 0x2a; 0x01; 0xff; ];
      false, [ 0x9f; 0x83; 0x96; 0xf3; 0x2a; 0x01; 0x00; ];
    ];


  case
    "recursive encoding"
    Asn.(
      fix @@ fun list ->
        map (function `C1 () -> [] | `C2 (x, xs) -> x::xs)
            (function [] -> `C1 () | x::xs -> `C2 (x, xs))
        @@
        choice2
          null
          (sequence2
            (required bool)
            (required list)))

    [ [], [ 0x05; 0x00 ] ;

      [true],
      [ 0x30; 0x05;
              0x01; 0x01; 0xff;
              0x05; 0x00; ] ;

      [true; false; true],
      [ 0x30; 0x0f;
          0x01; 0x01; 0xff;
          0x30; 0x0a;
            0x01; 0x01; 0x00;
            0x30; 0x05;
              0x01; 0x01; 0xff;
              0x05; 0x00; ] ;

      [false; true; false],
      [ 0x30; 0x80;
          0x01; 0x01; 0x00;
          0x30; 0x80;
            0x01; 0x01; 0xff;
            0x30; 0x80;
              0x01; 0x01; 0x00;
              0x05; 0x00;
              0x00; 0x00;
            0x00; 0x00;
          0x00; 0x00; ] ;

      [false; true; false],
      [ 0x30; 0x80;
          0x01; 0x01; 0x00;
          0x30; 0x80;
            0x01; 0x01; 0xff;
            0x30; 0x80;
              0x01; 0x01; 0x00;
              0x05; 0x00;
              0x00; 0x00;
            0x00; 0x00;
          0x00; 0x00; ] ;
    ];

  case
    "ia5 string"
    Asn.ia5_string

    [ "abc", [ 0x16; 0x03; 0x61; 0x62; 0x63; ];

      "abcd",
      [ 0x36; 0x0a;
          0x16; 0x01; 0x61;
          0x16; 0x01; 0x62;
          0x16; 0x02; 0x63; 0x64; ];

      "abcd",
      [ 0x36; 0x80;
          0x16; 0x01; 0x61;
          0x16; 0x01; 0x62;
          0x16; 0x02; 0x63; 0x64;
          0x00; 0x00; ];

      "abcd",
      [ 0x36; 0x80;
          0x36; 0x06;
            0x16; 0x01; 0x61;
            0x16; 0x01; 0x62;
          0x16; 0x02; 0x63; 0x64;
          0x00; 0x00; ];

      "test1@rsa.com",
      [ 0x16; 0x0d; 0x74; 0x65; 0x73; 0x74; 0x31; 0x40; 0x72; 0x73;
        0x61; 0x2e; 0x63; 0x6f; 0x6d; ] ;

      "test1@rsa.com",
      [ 0x16; 0x81; 0x0d;
          0x74; 0x65; 0x73; 0x74; 0x31; 0x40; 0x72; 0x73; 0x61; 0x2e; 0x63; 0x6f; 0x6d ];

      "test1@rsa.com",
      [ 0x36; 0x13;
          0x16; 0x05; 0x74; 0x65; 0x73; 0x74; 0x31;
          0x16; 0x01; 0x40;
          0x16; 0x07; 0x72; 0x73; 0x61; 0x2e; 0x63; 0x6f; 0x6d; ]
    ];

  case
    "bit string"
    Asn.bit_string

    ( let example =
        [| false; true; true; false; true; true; true; false; false;
          true; false; true; true; true; false; true; true; true |] in

      [ example, [ 0x03; 0x04; 0x06; 0x6e; 0x5d; 0xc0 ];
        example, [ 0x03; 0x04; 0x06; 0x6e; 0x5d; 0xe0 ];
        example, [ 0x03; 0x81; 0x04; 0x06; 0x6e; 0x5d; 0xc0 ];
        example, [ 0x23; 0x09;
                      0x03; 0x03; 0x00; 0x6e; 0x5d;
                      0x03; 0x02; 0x06; 0xc0]
      ] );

  ( let open Asn.OID in

    let rsa = base 1 2 <| 840 <| 113549 in

    case "oid" Asn.oid [

      ( rsa ),
      [ 0x06; 0x06; 0x2a; 0x86; 0x48; 0x86; 0xf7; 0x0d ];

      ( rsa <| 1 <| 7 <| 2 ),
      [ 0x06; 0x09; 0x2A; 0x86; 0x48; 0x86; 0xF7; 0x0D; 0x01; 0x07; 0x02 ];

      ( rsa <| 1 <| 7 <| 1 ),
      [ 0x06; 0x09; 0x2A; 0x86; 0x48; 0x86; 0xF7; 0x0D; 0x01; 0x07; 0x01 ];

      ( base 1 3 <| 14 <| 3 <| 2 <| 26 ),
      [ 0x06; 0x05; 0x2B; 0x0E; 0x03; 0x02; 0x1A ];

      ( base 2 5 <| 4 <| 3 ),
      [ 0x06; 0x03; 0x55; 0x04; 0x03 ];

      ( base 2 5 <| 29 <| 15 ),
      [ 0x06; 0x03; 0x55; 0x1D; 0x0F ];

      ( base 1 2 <| 99999 ),
      [ 0x06; 0x04; 0x2a; 0x86; 0x8d; 0x1f ];
    ] );

  case "octets" Asn.octet_string
  ( let f = cstruct_of_list in [

    f [ 0x01; 0x23; 0x45; 0x67; 0x89; 0xab; 0xcd; 0xef ],
    [ 0x04; 0x08; 0x01; 0x23; 0x45; 0x67; 0x89; 0xab; 0xcd; 0xef ] ;

    f [ 0x01; 0x23; 0x45; 0x67; 0x89; 0xab; 0xcd; 0xef ],
    [ 0x04; 0x81; 0x08; 0x01; 0x23; 0x45; 0x67; 0x89; 0xab; 0xcd; 0xef; ];

    f [ 0x01; 0x23; 0x45; 0x67; 0x89; 0xab; 0xcd; 0xef ],
    [ 0x24; 0x0c;
        0x04; 0x04; 0x01; 0x23; 0x45; 0x67;
        0x04; 0x04; 0x89; 0xab; 0xcd; 0xef; ]
  ]);

  case "utc time" Asn.utc_time [

    ( Asn.Time.({ date = (1991, 5, 6); time = (23, 45, 40, 0.); tz = None }),
      [ 0x17; 0x0d; 0x39; 0x31; 0x30; 0x35; 0x30; 0x36;
        0x32; 0x33; 0x34; 0x35; 0x34; 0x30; 0x5a ] ) ;

    ( Asn.Time.({
        date = (1991, 5, 6)  ;
        time = (16, 45, 40, 0.);
        tz   = Some (7, 0, `W) }) ,
      [ 0x17; 0x11; 0x39; 0x31; 0x30; 0x35; 0x30; 0x36; 0x31; 0x36;
        0x34; 0x35; 0x34; 0x30; 0x2D; 0x30; 0x37; 0x30; 0x30 ] )

  ] ;

  case "generalized time" Asn.generalized_time [ (* XXX add examples *) ] ;

]

let anticases = [

  (* thx @alpha-60 *)
  anticase "tag overflow" Asn.bool [
    [ 0x1f;
      0xA0; 0x80;
      0x80; 0x80;
      0x80; 0x80;
      0x80; 0x80;
      0x80; 0x01;
      0x01; 0xff
    ]
  ] ;

  anticase "leading zero" Asn.(implicit 127 bool) [
    [ 0x9f; 0x80; 0x7F; 0x01; 0xff]
  ];

  anticase "length overflow" Asn.bool [
    [ 0x01;
      0x88;
      0x80;0x00;0x00;0x00;0x00;0x00;0x00;0x01;
      0xff
    ]
  ] ;

  anticase "oid overflow" Asn.oid [
    [ 0x06;
      0x0b;
      0x2a;
      0xbf; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0x7f ]
  ] ;
]

let suite =

  "ASN.1" >::: [

    "Time normalization" >:: random_time_tests ~n:100000 ;

    "BER decoding" >:::
      List.map
        (fun (TC (name, _, _, _) as tc) -> name >:: test_decode Asn.ber tc)
        cases ;

    "not @@ BER decoding" >:::
      List.map
        (fun (ATC (name, _, _) as atc) -> name >:: test_no_decode Asn.ber atc)
        anticases ;

    "BER random encode->decode" >:::
      List.map
        (fun (TC (name, cmp, asn, _)) -> name >:: test_loop_decode ?cmp Asn.ber asn)
        cases ;

    "DER random encode->decode" >:::
      List.map
        (fun (TC (name, cmp, asn, _)) -> name >:: test_loop_decode ?cmp Asn.der asn)
        cases ;

    "X509 decode" >:::
      List.mapi
        (fun i bytes ->
          ("certificate " ^ string_of_int i) >:: fun _ ->
            assert_decode X509.cert_der (cstruct_of_list bytes))
        X509.examples ;

    "X509 elements random encode->decode" >::: [
      "BER validity" >:: test_loop_decode Asn.ber X509.validity ;
      "DER validity" >:: test_loop_decode Asn.der X509.validity ;
      "BER cert" >:: test_loop_decode ~iter:1000 Asn.ber X509.certificate ;
(*       "DER cert" >:: test_loop_decode ~iter:100 Asn.der X509.certificate ; *)
    ] ;
  ]

