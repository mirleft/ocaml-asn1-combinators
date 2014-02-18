
open Bigarray

let byte_dump x =
  let rec go i =
    if i < 0 then [] else
      string_of_int ((x lsr i) land 1) :: go (i - 1) in
  String.concat "" @@ go 7

let rec split_at n = function
  | []               -> ([], [])
  | x::xs when n > 0 -> let (hd, tl) = split_at (n - 1) xs in (x::hd, tl)
  | xs               -> ([], xs)

let rec groups n list =
  match split_at n list with
  | ([], []) -> []
  | (xs, []) -> [xs]
  | (xs, ys) -> xs :: groups n ys

let list_of_a1 arr =
  let rec go a n =
    if n < 0 then a else go (arr.{n} :: a) (n - 1) in
  go [] (Array1.dim arr - 1)

let hex_string ?(prefix="") bytes =
  let open Printf in
  let rec decorate off = function
    | []      -> []
    | xs::xxs ->
        let line : string =
          String.concat " " @@
          sprintf "%s%03x: " prefix off :: List.map (sprintf "%02x") xs in
        line :: decorate (off + List.length xs) xxs in
  bytes |> list_of_a1 |> groups 16 |> decorate 0
        |> String.concat "\n"

let hex_dump ?prefix bytes =
  print_endline @@ hex_string ?prefix bytes

let write_list ?(off = 0) arr list =
  let rec go i = function
    | [] -> ()
    | x::xs -> ( arr.{i} <- x ; go (succ i) xs ) in
  go off list

let bytes_of_list list =
  let length = List.length list in
  let arr = Array1.create int8_unsigned c_layout length in
  ( write_list arr list ; arr )

let bytes_of_string string =
  let length = String.length string in
  let arr = Array1.create int8_unsigned c_layout length in
  for i = 0 to length - 1 do arr.{i} <- int_of_char string.[i] done;
  arr

