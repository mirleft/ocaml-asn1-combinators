
open Bigarray

type bytes = (int, int8_unsigned_elt, c_layout) Array1.t

let compare_b arr1 arr2 =
  let s1, s2 = Array1.(dim arr1, dim arr2) in
  let rec go i lim =
    if i = lim then
      compare s1 s2
    else
      match compare arr1.{i} arr2.{i} with
      | 0 -> go (succ i) lim
      | n -> n in
  go 0 (min s1 s2)


module Rd = struct

  let drop off buffer = Array1.(
    let length = dim buffer in sub buffer off (length - off)
  )

  let isolate count buff =
    Array1.(sub buff 0 count, sub buff count (dim buff - count))

  let eof buff = Array1.dim buff = 0

  let empty = Array1.create int8_unsigned c_layout 0
end

module Wr = struct

  type t = int * (int -> bytes -> unit)

  let size (n, _) = n

  let empty = (0, (fun _ _ -> ()))

  let (<>) (l1, w1) (l2, w2) =
    let w off bytes =
      ( w1 off bytes ; w2 (off + l1) bytes ) in
    (l1 + l2, w)

  let append = (<>)

  let rec concat = function
    | []    -> empty
    | w::ws -> w <> concat ws

  let list lst =
    let open List in
    let w off bytes =
      iteri (fun i b -> bytes.{off + i} <- b) lst in
    (length lst, w)

  let array arr =
    let open Array in
    let w off bytes =
      iteri (fun i b -> bytes.{off + i} <- b) arr in
    (length arr, w)

  let string ?f s =
    let n = String.length s in
    let w =
      match f with
      | None ->
          fun off buf ->
            for i = 0 to n - 1 do
              buf.{off + i} <- int_of_char s.[i]
            done
      | Some f' ->
          fun off buf ->
            for i = 0 to n - 1 do
              buf.{off + i} <- f' (int_of_char s.[i])
            done
    in
    (n, w)
    

  let bytes bytes1 =
    let open Array1 in
    let n = dim bytes1 in
    let w off bytes = blit bytes1 (sub bytes off n) in
    (n, w)

  let byte b = (1, (fun off bytes -> bytes.{off} <- b))

  let write ?(off=0) (n, w) bytes =
    if Array1.dim bytes < (n + off) then
      raise @@ Invalid_argument "write: Bigarray too short"
    else ( w off bytes ; n )

  let to_bytes (n, w) =
    let arr = Array1.create int8_unsigned c_layout n in
    ( w 0 arr ; arr )
end

