
let cs_compare = Cstruct.compare

type t = int * (int -> Cstruct.t -> unit)

let immediate n f = (n, f)

let len (n, _) = n

let empty = (0, (fun _ _ -> ()))

let (<>) (l1, w1) (l2, w2) =
  let w off buf =
    ( w1 off buf ; w2 (off + l1) buf ) in
  (l1 + l2, w)

let append = (<>)

let rec concat = function
  | []    -> empty
  | w::ws -> w <> concat ws

let of_list lst =
  let open List in
  let w off cs =
    iteri (fun i -> Cstruct.set_uint8 cs (off + i)) lst in
  (length lst, w)

let of_string str =
  let n = String.length str in
  (n, fun off cs -> Cstruct.blit_from_string str 0 cs off n)

let of_cstruct cs' =
  let n = Cstruct.len cs' in
  (n, fun off cs -> Cstruct.blit cs' 0 cs off n)

let of_byte b = (1, fun off cs -> Cstruct.set_uint8 cs off b)

let to_cstruct (n, w) =
  let cs = Cstruct.create n in ( w 0 cs ; cs )

let to_writer (n, w) = (n, fun cs -> w 0 cs)

