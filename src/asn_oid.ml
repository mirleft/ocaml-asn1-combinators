(* Copyright (c) 2014-2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(* XXX
 * OIDs being just ints means not being able to represent the full range.
 * Rarely used in practice, but maybe switch to bignums.
 *)
type t = Oid of int * int * int list

let assert_positive name x =
  if x < 0 then
    invalid_arg ("OID." ^ name ^ ": component out of range")

let (<|) (Oid (v1, v2, vs)) vn =
  assert_positive "<|" vn ;
  Oid (v1, v2, vs @ [vn])

let (<||) (Oid (v1, v2, vs)) vs' =
  List.iter (assert_positive "<||") vs' ;
  Oid (v1, v2, vs @ vs')

let to_list (Oid (v1, v2, vs)) = v1 :: v2 :: vs

let base v1 v2 =
  match v1 with
  | 0|1 when v2 >= 0 && v2 < 40 -> Oid (v1, v2, [])
  | 2   when v2 >= 0            -> Oid (v1, v2, [])
  | _ -> invalid_arg "OID.base: component out of range"

let to_string (Oid (v1, v2, vs)) =
  let b = Buffer.create 16 in
  let component x = Buffer.add_string b (string_of_int x)
  and dot () = Buffer.add_char b '.' in
  component v1;
  List.iter (fun x -> dot () ; component x) (v2 :: vs);
  Buffer.contents b

let of_string str =
  let rec components str =
    if String.length str = 0 then []
    else Scanf.sscanf str ".%d%s" (fun v rest -> v :: components rest) in
  try
    let (v1, v2, rest) =
      Scanf.sscanf str "%d.%d%s" (fun v1 v2 rest -> (v1, v2, rest)) in
    base v1 v2 <|| components rest
  with End_of_file -> invalid_arg "malformed oid"

let compare (Oid (v1, v2, vs)) (Oid (v1', v2', vs')) =
  let rec cmp (xs : int list) (ys : int list) =
    match (xs, ys) with
    | ([], []) ->  0
    | ([], _ ) -> -1
    | (_ , []) ->  1
    | (x::xs, y::ys) -> match compare x y with 0 -> cmp xs ys | r -> r in
  match compare v1 v1' with
  | 0 -> ( match compare v2 v2' with 0 -> cmp vs vs' | r -> r )
  | r -> r

let equal o1 o2 = compare o1 o2 = 0

let hash (Oid (v1, v2, vs)) =
  List.fold_left Hashtbl.seeded_hash (Hashtbl.seeded_hash v1 v2) vs

