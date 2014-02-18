
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

module Time = struct

  type t = {
    date : (int * int * int) ;
    time : (int * int * int * float) ;
    tz   : (int * int * [ `W | `E ]) option ;
  }

  module C = struct
    let is c d = c = d
    let digit c = c >= '0' && c <= '9'
  end

  module S = struct
    open String
    let drop n str =
      let len = length str in
      if n >= len then "" else sub str n (len - n)
    let pred_at p str n = length str > n && p str.[n]
    let length = length
  end

  let pn2, pn3, pn4 =
    Printf.(sprintf "%02d", sprintf "%03d", sprintf "%04d")

  let rnn n str off = int_of_string (String.sub str off n)
  let rn1, rn2, rn3, rn4 = rnn 1, rnn 2, rnn 3, rnn 4

  let tz_to_string = function
    | None              -> "Z"
    | Some (0, 0, _)    -> "Z"
    | Some (hh, mm, `W) -> "-" ^ pn2 hh ^ pn2 mm
    | Some (hh, mm, `E) -> "+" ^ pn2 hh ^ pn2 mm

  let tz_of_string str =
    let pack = function
      | (0, 0, _) -> None
      | triple    -> Some triple in
    match str.[0] with
    | 'Z' | 'z' -> None
    | '+' -> pack (rn2 str 1, rn2 str 3, `E)
    | '-' -> pack (rn2 str 1, rn2 str 3, `W)
    | _   -> failwith ""

  let tz_of_string_optional str =
    if S.length str = 0 then None
    else tz_of_string str

  let try_ fn =
    try Some (fn ()) with
    | Failure _ | Invalid_argument _ -> None

  let utc_time_of_string str = try_ @@ fun () ->
    let yy = rn2 str 0
    and mm = rn2 str 2
    and dd = rn2 str 4
    and hh = rn2 str 6
    and mi = rn2 str 8 in
    let (ss, tz_off) =
      match S.pred_at C.digit str 10 with
      | false -> (0, 10)
      | true  -> (rn2 str 10, 12) in
    let tz = tz_of_string (S.drop tz_off str)
    in
    { date = (yy, mm, dd) ; time = (hh, mi, ss, 0.) ; tz }

  let utc_time_to_string { date = (yy, mm, dd); time = (hh, mi, ss, _); tz } =
    String.concat "" [
      pn2 yy ; pn2 mm ; pn2 dd ; pn2 hh ; pn2 mi ;
      ( if ss = 0 then "" else pn2 ss ) ;
      tz_to_string tz
    ]

  let gen_time_of_string str = try_ @@ fun () ->
    let yy = rn4 str 0
    and mm = rn2 str 4
    and dd = rn2 str 6
    and hh = rn2 str 8
    and mi = rn2 str 10 in
    let (ss, sf, tz_off) =
      match S.pred_at C.digit str 10 with
      | false -> (0, 0., 12)
      | true  ->
          let ss = rn2 str 12 in
          match S.pred_at C.(is '.') str 14 with
          | false -> (ss, 0., 14)
          | true  ->
              let rec scan_f acc e i =
                if S.pred_at C.digit str i then
                  let digit = float @@ rn1 str i in
                  scan_f (acc +. e *. digit) (e *. 0.1) (succ i)
                else (ss, acc, i) in
              scan_f 0. 0.1 15 in
    let tz = tz_of_string_optional (S.drop tz_off str)
    in
    { date = (yy, mm, dd) ; time = (hh, mi, ss, sf) ; tz }

  let gen_time_to_string ?(ber=false) t =

    let pf3 f =
      let rec go e =
        if e > 1000. then ""
        else string_of_int (int_of_float (f *. e) mod 10) ^ go (e *. 10.)
      in go 10.  in

    match (ber, t.tz) with
    | (false, _) | (true, None) | (true, Some (0, 0, _)) ->
        let (yy, mm, dd)     = t.date
        and (hh, mi, ss, sf) = t.time in
        String.concat "" [
          pn4 yy ; pn2 mm ; pn2 dd ; pn2 hh ; pn2 mi ;
          ( if ss = 0 && sf = 0. then "" else
            if sf = 0. then pn2 ss
            else pn2 ss ^ "." ^ pf3 sf ) ;
          tz_to_string t.tz
        ]
    | _ -> invalid_arg "GeneralizedTime: can't encode time in non-GMT zone"


  let random ?(fraction=false) () =
    let num n = Random.int n + 1 in
    let sec   = if Random.int 3 = 0 then 0 else num 59
    and sec_f = if fraction then Random.float 1. else 0.
    and tz    = match Random.int 3 with
      | 0 -> None
      | 1 -> Some (num 12, num 59, `E)
      | 2 -> Some (num 12, num 59, `W)
      | _ -> assert false
    in
    { date = (num 99, num 12, num 30) ;
      time = (num 23, num 59, sec, sec_f) ;
      tz   = tz }

  module Str = Gen_string

end
  

