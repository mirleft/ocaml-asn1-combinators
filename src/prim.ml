
open Bytekit

module type Prim = sig
  type t
  val of_bytes : int -> bytes -> t
  val to_bytes : t -> Wr.t
  val random   : unit -> t
end

module type String_primitive = sig
  include Prim
  val random : ?size:int -> unit -> t
  val concat : t list -> t
  val length : t -> int
end


let rec replicate_l n f =
  if n < 1 then [] else f () :: replicate_l (pred n) f

let max_r_int = (1 lsl 30) - 1

let random_int () = Random.int max_r_int

let random_int_r a b = a + Random.int (b - a + 1)

let random_size = function
  | Some size -> size
  | None      -> Random.int 20


module Integer :
  Prim with type t = [ `B of Big_int.big_int | `I of int ] =
struct

  open Big_int

  type t = [ `B of big_int | `I of int ]

  let small_int_bytes = 7

  let zero, one, neg_one, two =
    let f = big_int_of_int in
    f 0, f 1, f (-1), f 2

  let big_of_bytes n buff =
    let rec loop acc i =
      if i = n then acc else 
        loop (add_int_big_int
                buff.{i}
                (mult_int_big_int 0x100 acc))
              (succ i) in
    let x = loop zero_big_int 0 in
    match buff.{0} land 0x80 with
    | 0 -> x
    | _ ->
        sub_big_int x
          (power_big_int_positive_int two (n * 8))

  let int_of_bytes n buff =
    let rec loop acc i =
      if i = n then acc else
        loop (buff.{i} + (acc lsl 8)) (succ i) in
    let x = loop 0 0 in
    match buff.{0} land 0x80 with
    | 0 -> x
    | _ -> x - 0x01 lsl (n * 8)

  let of_bytes n buff =
    if n > small_int_bytes then
      `B (big_of_bytes n buff)
    else `I (int_of_bytes n buff)

  let int_to_byte_list n =
    let rec loop acc n =
      match (n, acc) with
      | ( 0, []  ) -> [ 0x00 ]
      | ( 0, x::_) when x >= 0x80 -> 0x00 :: acc
      | ( 0, _   ) -> acc
      | (-1, []  ) -> [ 0xff ]
      | (-1, x::_) when x < 0x80  -> 0xff :: acc
      | (-1, _   ) -> acc
      | _ -> loop (n land 0xff :: acc) (n asr 8) in
    loop [] n

  let big_to_byte_list n =
    let rec loop acc n =
      if eq_big_int n zero then
        match acc with
        | [] -> [0x00]
        | x::_ when x >= 0x80 -> 0x00 :: acc
        | _  -> acc else
      if eq_big_int n neg_one then
        match acc with
        | [] -> [0xff]
        | x::_ when x < 0x80 -> 0xff :: acc
        | _  -> acc
      else
        let b = int_of_big_int (extract_big_int n 0 8) in
        loop (b :: acc) (shift_right_big_int n 8) in
    loop [] n

  let to_bytes = function
    | `I n -> Wr.list (int_to_byte_list n)
    | `B n -> Wr.list (big_to_byte_list n)

  let random =
    let big_odds = 10 in fun () ->
      let x = Random.int (big_odds + 1) in
      if x <> big_odds then `I (x - x / 2)
      else `I (Random.int max_r_int - max_r_int / 2)

end

module Gen_string :
  String_primitive with type t = string =
struct

  type t = string

  let of_bytes n buf =
    let string = String.create n in
    for i = 0 to n - 1 do string.[i] <- char_of_int buf.{i} done;
    string

  let to_bytes = Wr.string ~f:(fun b -> b land 0x7f)

  let random ?size () =
    let n = random_size size in
    let s = String.create n in
    for i = 0 to n - 1 do s.[i] <- Char.chr (random_int_r 32 126) done;
    s

  let (concat, length) = String.(concat "", length)
end

module Bits :
  String_primitive with type t = bool array =
struct

  type t = bool array

  let of_bytes n buf =
    let unused = buf.{0} in
    Array.init ((n - 1) * 8 - unused) @@ fun i ->
      let byte = buf.{i / 8 + 1} lsl (i mod 8) in
      byte land 0x80 = 0x80

  let (|<) n = function
    | true  -> (n lsl 1) lor 1
    | false -> (n lsl 1)

  let to_bytes arr =
    Wr.list @@
      match
        Array.fold_left
          (fun (n, acc, accs) x ->
            if n = 8 then (1, 0 |< x, acc::accs)
            else (n + 1, acc |< x, accs))
          (0, 0, [])
          arr
      with
      | (0, acc, xs) -> 0 :: []
      | (n, acc, xs) -> 8 - n :: List.rev ((acc lsl (8 - n))::xs)

  let random ?size () =
    Array.init (random_size size) (fun _ -> Random.bool ())

  let (concat, length) = Array.(concat, length)

end

module Octets :
  String_primitive with type t = bytes =
struct

  type t = bytes

  open Bigarray

  let make = Array1.create int8_unsigned c_layout

  let of_bytes n buf = Array1.sub buf 0 n

  let to_bytes = Wr.bytes

  let random ?size () =
    let n   = random_size size in
    let arr = make n in
    for i = 0 to n - 1 do arr.{i} <- Random.int 256 done;
    arr

  let concat arrs =
    let arr = make List.(fold_left (+) 0 @@ map Array1.dim arrs) in
    let _   =
      List.fold_left
        Array1.(fun i e ->
          let len = dim e in
          ( blit e @@ sub arr i len ; i + len ))
        0 arrs in
    arr

  let length = Array1.dim

end

(* module OID : Prim with type t = int list = struct *)
module OID : sig

  include Prim

  val (<|)    : t -> int -> t
  val (<||)   : t -> int list -> t
  val to_list : t -> int list
  val base    : int -> int -> t

end = struct

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

  let of_bytes n buf =

    let rec values i =
      if i = n then []
      else let (i', v) = component 0 i in v :: values i'

    and component acc i =
      let byte = buf.{i} in
      let acc' = acc lor (byte land 0x7f) in
      match byte land 0x80 with
      | 0 -> (succ i, acc')
      | _ -> component (acc' lsl 7) (succ i) in

    let b1 = buf.{0} in
    let v1, v2 = b1 / 40, b1 mod 40 in

    Oid (v1, v2, values 1)

  let to_bytes = fun (Oid (v1, v2, vs)) ->
    let cons x = function [] -> [x] | xs -> x lor 0x80 :: xs in
    let rec component xs x =
      if x < 0x80 then cons x xs
      else component (cons (x land 0x7f) xs) (x lsr 7)
    and values = function
      | []    -> Wr.empty
      | v::vs -> Wr.(list (component [] v) <> values vs) in
    Wr.(byte (v1 * 40 + v2) <> values vs)

  let random () =
    Random.( base (int 3) (int 40) <|| replicate_l (int 10) random_int )

end

