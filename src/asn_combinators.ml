(* Copyright (c) 2014-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Asn_core
module Prim = Asn_prim

module Int = struct
  type t = int
  let compare (a: t) b = compare a b
  let equal (a: t) b = a = b
end

type cls = [ `Universal | `Application | `Private ]

let fix f = Fix (f, Asn_cache.variant ())

let map ?random f g asn = Iso (f, g, random, asn)

let to_tag id = function
  | Some `Application -> Tag.Application id
  | Some `Private     -> Tag.Private id
  | Some `Universal   -> Tag.Universal id
  | None              -> Tag.Context_specific id

let explicit ?cls id asn = Explicit (to_tag id cls, asn)
let rec implicit : type a. ?cls:cls -> int -> a asn -> a asn =
  fun ?cls id -> function
    Fix (f, _) as asn -> implicit ?cls id (f asn)
  | Iso (f, g, r, asn) -> Iso (f, g, r, implicit ?cls id asn)
  | Choice (_, _) as asn -> explicit ?cls id asn
  | asn -> Implicit (to_tag id cls, asn)

let bool                = Prim Bool
and integer             = Prim Int
and octet_string        = Prim Octets
and null                = Prim Null
and oid                 = Prim OID
and character_string    = Prim CharString

let string tag = implicit ~cls:`Universal tag character_string

let utf8_string      = string 0x0c
let numeric_string   = string 0x12
and printable_string = string 0x13
and teletex_string   = string 0x14
and videotex_string  = string 0x15
and ia5_string       = string 0x16
and graphic_string   = string 0x19
and visible_string   = string 0x1a
and general_string   = string 0x1b
and universal_string = string 0x1c
and bmp_string       = string 0x1e

let (utc_time, generalized_time) =
  let open Asn_prim.Time in
  let time ~random tag (f, g) =
    map ~random f g @@
      implicit ~cls:`Universal tag character_string in
  time ~random:utc_random 0x17 (utc_time_of_string, of_utc_time),
  time ~random:gen_random 0x18 (gen_time_of_string, of_gen_time)

let int =
  let f str =
    match String.length str with
    | 0 -> 0
    | 1 -> String.get_int8 str 0
    | 2 -> String.get_int16_be str 0
    | 3 -> String.get_int16_be str 0 lsl 8 + String.get_uint8 str 2
    | 4 ->
      let v = String.get_int32_be str 0 in
      if Sys.word_size = 32 && (v > Int32.of_int max_int || v < Int32.of_int min_int) then
        parse_error "INTEGER: int overflow: %a" pp_octets str
      else
        Int32.to_int v
    | 5 ->
      if Sys.word_size = 32 then
        parse_error "INTEGER: int overflow: %a" pp_octets str
      else
        let v = Int32.to_int (String.get_int32_be str 0) in
        v lsl 8 + String.get_uint8 str 4
    | 6 ->
      if Sys.word_size = 32 then
        parse_error "INTEGER: int overflow: %a" pp_octets str
      else
        let v = Int32.to_int (String.get_int32_be str 0) in
        v lsl 16 + String.get_uint16_be str 4
    | 7 ->
      if Sys.word_size = 32 then
        parse_error "INTEGER: int overflow: %a" pp_octets str
      else
        let v = Int32.to_int (String.get_int32_be str 0) in
        v lsl 24 + (String.get_uint16_be str 4) lsl 8 + String.get_uint8 str 6
    | 8 ->
      let v = String.get_int64_be str 0 in
      if Sys.word_size = 32 || (v > Int64.of_int max_int || v < Int64.of_int min_int) then
        parse_error "INTEGER: int overflow: %a" pp_octets str
      else
        Int64.to_int v
    | _ -> parse_error "INTEGER: int overflow: %a" pp_octets str
  and g i =
    let i64 = Int64.of_int i in
    if i >= -0x80 && i <= 0x7F then
      let b = Bytes.create 1 in
      Bytes.set_int8 b 0 i;
      Bytes.unsafe_to_string b
    else if i >= -0x8000 && i <= 0x7FFF then
      let b = Bytes.create 2 in
      Bytes.set_int16_be b 0 i;
      Bytes.unsafe_to_string b
    else if i >= -0x80_0000 && i <= 0x7F_FFFF then
      let b = Bytes.create 3 in
      Bytes.set_int16_be b 0 (i lsr 8);
      Bytes.set_uint8 b 2 (i land 0xff);
      Bytes.unsafe_to_string b
    else if i64 >= -0x8000_0000L && i64 <= 0x7FFF_FFFFL then
      let b = Bytes.create 4 in
      Bytes.set_int32_be b 0 (Int32.of_int i);
      Bytes.unsafe_to_string b
    else if i64 >= -0x80_0000_0000L && i64 <= 0x7F_FFFF_FFFFL then
      let b = Bytes.create 5 in
      Bytes.set_int32_be b 0 (Int32.of_int (i lsr 8));
      Bytes.set_uint8 b 4 (i land 0xFF);
      Bytes.unsafe_to_string b
    else if i64 >= -0x8000_0000_0000L && i64 <= 0x7FFF_FFFF_FFFFL then
      let b = Bytes.create 6 in
      Bytes.set_int32_be b 0 (Int32.of_int (i lsr 16));
      Bytes.set_uint16_be b 4 (i land 0xFFFF);
      Bytes.unsafe_to_string b
    else if i64 >= -0x80_0000_0000_0000L && i64 <= 0x7F_FFFF_FFFF_FFFFL then
      let b = Bytes.create 7 in
      Bytes.set_int32_be b 0 (Int32.of_int (i lsr 24));
      Bytes.set_uint16_be b 4 ((i land 0xFFFF00) lsr 8);
      Bytes.set_uint8 b 6 (i land 0xFF);
      Bytes.unsafe_to_string b
    else
      let b = Bytes.create 8 in
      Bytes.set_int64_be b 0 i64;
      Bytes.unsafe_to_string b
  in
  let random () =
    let rec go () =
      let buf = Prim.Integer.random ~size:(Sys.word_size / 8) () in
      (* OCaml integer are only 31 / 63 bit *)
      try f buf with
      | Parse_error _ -> go ()
    in
    go ()
  in
  map ~random f g integer

let unsigned_integer =
  let f str =
    let l = String.length str in
    if l > 0 then
      let fst = String.get_uint8 str 0 in
      if fst > 0x7F then
        parse_error "unsigned integer < 0"
      else if fst = 0x00 then
        String.sub str 1 (l - 1)
      else
        str
    else
      str
  and g str =
    let l = String.length str in
    let rec strip0 off =
      if l - off >= 2 &&
         String.get_uint8 str off = 0x00 &&
         String.get_uint8 str (off + 1) < 0x80
      then
        strip0 (off + 1)
      else if off = 0 then
        str
      else
        String.sub str off (l - off)
    in
    let str' = strip0 0 in
    if String.length str' = 0 || String.get_uint8 str' 0 > 0x7F then
      "\x00" ^ str'
    else
      str'
  in
  let random () =
    let rec go () =
      let buf = Prim.Integer.random () in
      try f buf with
      | Parse_error _ -> go ()
    in
    go ()
  in
  map ~random f g integer

let enumerated f g = map f g @@ implicit ~cls:`Universal 0x0a int

let bit_string = Prim.Bits.(map to_array of_array (Prim Bits))
and bit_string_octets =
  let f = function
  | 0, buf -> buf
  | clip, buf ->
      let n = String.length buf in
      let last = String.get_uint8 buf (n - 1) in
      let buf' = Bytes.of_string buf
      and last = last land (lnot (1 lsl clip - 1)) in
      Bytes.set_uint8 buf' (n - 1) last;
      Bytes.unsafe_to_string buf'
  in
  map f (fun cs -> (0, cs)) (Prim Bits)

let bit_string_flags (type a) (xs : (int * a) list) =
  let cmp = compare in (* XXX yes... *)
  let module M1 = Map.Make (struct type t = a let compare = cmp end) in
  let module M2 = Map.Make (Int) in
  let aix, ixa =
    List.fold_left (fun (m1, m2) (i, x) -> M1.add x i m1, M2.add i x m2)
    (M1.empty, M2.empty) xs in
  let n = match M2.max_binding_opt ixa with Some (x, _) -> x + 1 | _ -> 0 in
  let f bits =
    let r = ref [] in
    bits |> Array.iteri (fun i -> function
    | false -> ()
    | true -> try r := M2.find i ixa :: !r with Not_found -> ());
    List.sort cmp !r
  and g es =
    let arr = Array.make n false in
    let register e = try arr.(M1.find e aix) <- true with Not_found -> () in
    List.iter register es;
    arr
  in
  map f g bit_string


let single a   = Last a
and ( @) a b   = Pair (a, b)
and (-@) a b   = Pair (a, Last b)
and optional ?label a = Optional (label, a)
and required ?label a = Required (label, a)

let product2 fn a b = fn @@ a @ single b

let product3 fn a b c =
  map (fun (a, (b, c)) -> (a, b, c))
      (fun (a, b, c) -> (a, (b, c)))
      (fn @@ a @ b @ single c)

let product4 fn a b c d =
  map (fun (a, (b, (c, d))) -> (a, b, c, d))
      (fun (a, b, c, d) -> (a, (b, (c, d))))
      (fn @@ a @ b @ c @ single d)

let product5 fn a b c d e =
  map (fun (a, (b, (c, (d, e)))) -> (a, b, c, d, e))
      (fun (a, b, c, d, e) -> (a, (b, (c, (d, e)))))
      (fn @@ a @ b @ c @ d @ single e)

let product6 fn a b c d e f =
  map (fun (a, (b, (c, (d, (e, f))))) -> (a, b, c, d, e, f))
      (fun (a, b, c, d, e, f) -> (a, (b, (c, (d, (e, f))))))
      (fn @@ a @ b @ c @ d @ e @ single f)


let sequence seq = Sequence seq

let sequence2 a b         = product2 sequence a b
and sequence3 a b c       = product3 sequence a b c
and sequence4 a b c d     = product4 sequence a b c d
and sequence5 a b c d e   = product5 sequence a b c d e
and sequence6 a b c d e f = product6 sequence a b c d e f

let sequence_of asn = Sequence_of asn

let set seq = Set seq

let set2 a b         = product2 set a b
and set3 a b c       = product3 set a b c
and set4 a b c d     = product4 set a b c d
and set5 a b c d e   = product5 set a b c d e
and set6 a b c d e f = product6 set a b c d e f

let set_of asn = Set_of asn

let choice a b = Choice (a, b)

let choice2 a b =
  map (function L a -> `C1 a | R b -> `C2 b)
      (function `C1 a -> L a | `C2 b -> R b)
      (choice a b)

let choice3 a b c =
  map (function L (L a) -> `C1 a | L (R b) -> `C2 b | R c -> `C3 c)
      (function `C1 a -> L (L a) | `C2 b -> L (R b) | `C3 c -> R c)
      (choice (choice a b) c)

let choice4 a b c d =
  map (function | L (L a) -> `C1 a | L (R b) -> `C2 b
                | R (L c) -> `C3 c | R (R d) -> `C4 d)
      (function | `C1 a -> L (L a) | `C2 b -> L (R b)
                | `C3 c -> R (L c) | `C4 d -> R (R d))
      (choice (choice a b) (choice c d))

let choice5 a b c d e =
  map (function | L (L (L a)) -> `C1 a | L (L (R b)) -> `C2 b
                | L (R c) -> `C3 c
                | R (L d) -> `C4 d | R (R e) -> `C5 e)
      (function | `C1 a -> L (L (L a)) | `C2 b -> L (L (R b))
                | `C3 c -> L (R c)
                | `C4 d -> R (L d) | `C5 e -> R (R e))
      (choice (choice (choice a b) c) (choice d e))

let choice6 a b c d e f =
  map (function | L (L (L a)) -> `C1 a | L (L (R b)) -> `C2 b
                | L (R c) -> `C3 c
                | R (L (L d)) -> `C4 d | R (L (R e)) -> `C5 e
                | R (R f) -> `C6 f)
      (function | `C1 a -> L (L (L a)) | `C2 b -> L (L (R b))
                | `C3 c -> L (R c)
                | `C4 d -> R (L (L d)) | `C5 e -> R (L (R e))
                | `C6 f -> R (R f))
      (choice (choice (choice a b) c) (choice (choice d e) f))
