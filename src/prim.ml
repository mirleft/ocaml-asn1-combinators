
open Bytekit

module Integer = struct
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

end

module String = struct

  let ascii_of_bytes n buf =
    let string = String.create n in
    for i = 0 to n - 1 do
      string.[i] <- char_of_int (buf.{i} land 0x7f)
    done;
    string

  let ascii_to_bytes = Wr.string ~f:(fun b -> b land 0x7f)
end

module Bit_string = struct

  let bits_of_bytes n (buf : bytes) =
    let unused = buf.{0} in
    Array.init ((n - 1) * 8 - unused) @@ fun i ->
      let byte = buf.{i / 8 + 1} lsl (i mod 8) in
      byte land 0x80 = 0x80

(*     let bytes_of_bits arr = *)

end
