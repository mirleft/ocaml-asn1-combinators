
type t = Oid of int * int * int list

let (<| ) (Oid (v1, v2, vs)) vn  = Oid (v1, v2, vs @ [vn])
let (<||) (Oid (v1, v2, vs)) vs' = Oid (v1, v2, vs @ vs')

let to_list (Oid (v1, v2, vs)) = v1 :: v2 :: vs

let base v1 v2 =
  if v1 < 0 || v1 > 2  then
    invalid_arg "OID.base: component 1 not 0..2" else
  if v2 < 0 || v2 > 39 then
    invalid_arg "OID.base: component 2 not 0..39"
  else Oid (v1, v2, [])

let to_string (Oid (v1, v2, vs)) =
  let b = Buffer.create 16 in
  let component x = Buffer.add_string b (string_of_int x)
  and dot () = Buffer.add_char b '.' in
  component v1;
  List.iter (fun x -> dot () ; component x) (v2 :: vs);
  Buffer.contents b

let of_string str =
  try
    let rec components str =
      if String.length str = 0 then []
      else Scanf.sscanf str ".%d%s" (fun v rest -> v :: components rest) in
    let (v1, v2, rest) =
      Scanf.sscanf str "%d.%d%s" (fun v1 v2 rest -> (v1, v2, rest)) in
    base v1 v2 <|| components rest
  with End_of_file -> invalid_arg "malformed oid"
