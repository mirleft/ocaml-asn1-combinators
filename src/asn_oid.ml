(* Copyright (c) 2014-2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(* XXX
 * OIDs being just ints means not being able to represent the full range.
 * Rarely used in practice, but maybe switch to bignums.
 *)
type t = Oid of int * int * int list

let invalid_arg fmt = Format.ksprintf invalid_arg fmt

let assert_non_negative ~caller x =
  if x < 0 then invalid_arg "OID.%s: negative component: %d" caller x

let (<|) (Oid (v1, v2, vs)) vn =
  assert_non_negative ~caller:"<|" vn;
  Oid (v1, v2, vs @ [vn])

let (<||) (Oid (v1, v2, vs)) vs' =
  List.iter (assert_non_negative ~caller:"<||") vs';
  Oid (v1, v2, vs @ vs')

let to_list (Oid (v1, v2, vs)) = v1 :: v2 :: vs

let base v1 v2 =
  match v1 with
  | 0|1 when v2 >= 0 && v2 < 40 -> Oid (v1, v2, [])
  | 2   when v2 >= 0            -> Oid (v1, v2, [])
  | _ -> invalid_arg "OID.base: out of range: %d.%d" v1 v2

let pp ppf (Oid (v1, v2, vs)) =
  Format.fprintf ppf "%d.%d%a" v1 v2
  (fun ppf -> List.iter (Format.fprintf ppf ".%d")) vs

let to_string = Format.asprintf "%a" pp

let of_string s =
  let rec components s =
    if String.length s = 0 then []
    else Scanf.sscanf s ".%d%s" (fun v s -> v :: components s) in
  try
    let (v1, v2, rest) =
      Scanf.sscanf s "%d.%d%s" (fun v1 v2 rest -> (v1, v2, rest)) in
    base v1 v2 <|| components rest
  with End_of_file | Scanf.Scan_failure _ ->
    invalid_arg "OID.of_string: malformed string: %s" s

let compare (Oid (v1, v2, vs)) (Oid (v1', v2', vs')) =
  let rec cmp (xs: int list) ys = match (xs, ys) with
    | ([], []) ->  0
    | ([], _ ) -> -1
    | (_ , []) ->  1
    | (x::xs, y::ys) -> match compare x y with 0 -> cmp xs ys | r -> r in
  match compare v1 v1' with
  | 0 -> ( match compare v2 v2' with 0 -> cmp vs vs' | r -> r )
  | r -> r

let equal o1 o2 = compare o1 o2 = 0

let seeded_hash seed (Oid (v1, v2, vs)) =
  Hashtbl.(List.fold_left seeded_hash (seeded_hash (seeded_hash seed v1) v2) vs)

let hash o = seeded_hash 0 o
