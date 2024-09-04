(* Copyright (c) 2014-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

let lex_compare cs1 cs2 =
  let (s1, s2) = String.(length cs1, length cs2) in
  let rec go i lim =
    if i = lim then
      compare s1 s2
    else
      match compare (String.get_uint8 cs1 i) (String.get_uint8 cs2 i) with
      | 0 -> go (succ i) lim
      | n -> n in
  go 0 (min s1 s2)

type t = int * (int -> bytes -> unit)

let immediate n f = (n, f)

let len (n, _) = n

let empty = (0, (fun _ _ -> ()))

let (<+>) (l1, w1) (l2, w2) =
  let w off buf =
    ( w1 off buf ; w2 (off + l1) buf ) in
  (l1 + l2, w)

let append = (<+>)

let rec concat = function
  | []    -> empty
  | w::ws -> w <+> concat ws

let of_list lst =
  let open List in
  let w off buf =
    iteri (fun i -> Bytes.set_uint8 buf (off + i)) lst in
  (length lst, w)

let of_octets str =
  let n = String.length str in
  (n, fun off buf -> Bytes.blit_string str 0 buf off n)

let of_byte b = (1, fun off buf -> Bytes.set_uint8 buf off b)

let to_octets (n, w) =
  let buf = Bytes.create n in
  w 0 buf;
  Bytes.unsafe_to_string buf

let to_writer (n, w) = (n, fun buf -> w 0 buf)

