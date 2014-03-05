
module type Prim = sig
  type t
  val of_cstruct : int -> Cstruct.t -> t
  val to_writer  : t -> Writer.t
  val random     : unit -> t
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

let cs_concat list =
  let cs =
    Cstruct.create @@
      List.fold_left (fun a e -> a + Cstruct.len e) 0 list in
  let _ =
    List.fold_left
      (fun i e ->
        let n = Cstruct.len e in
        ( Cstruct.blit e 0 cs i n ; n + i ))
      0 list in
  cs


module Integer : sig
  include Prim with type t = Num.num
  val cs_of_nat_string : string -> Cstruct.t
  val nat_string_of_cs : Cstruct.t -> string
end
  =
struct

  open Num

  type t = num

  let of_cstruct n buf =
    let rec loop acc = function
      | i when i = n -> acc
      | i ->
          let x = Cstruct.get_uint8 buf i in
          loop ((acc */ Int 0x100) +/ Int x) (succ i)
    in
    let x = loop (Int 0) 0 in
    match (Cstruct.get_uint8 buf 0) land 0x80 with
    | 0 -> x
    | _ -> x -/ power_num (Int 2) (Int (n * 8))

  let asr_8 = function
    | Big_int x -> Big_int (Big_int.shift_right_big_int x 8)
    | Int     x -> Int     (x asr 8)
    | Ratio   _ -> invalid_arg "Asn: Num.Rational"

  let last_8 = function
    | Big_int x -> Big_int.(int_of_big_int @@ extract_big_int x 0 8)
    | Int     x -> x land 0xff
    | Ratio   _ -> invalid_arg "Asn: Num.Rational"

  let to_writer n =

    let rec list_of_pos acc n =
      if eq_num n (Int 0) then
        match acc with
        | []                  -> [ 0x00 ]
        | x::_ when x >= 0x80 -> 0x00 :: acc
        | _                   -> acc
      else list_of_pos (last_8 n :: acc) (asr_8 n)

    and list_of_neg acc n =
      if eq_num n (Int (-1)) then
        match acc with
        | []                 -> [ 0xff ]
        | x::_ when x < 0x80 -> 0xff :: acc
        | _                  -> acc
      else list_of_neg (last_8 n :: acc) (asr_8 n) in

    let chars =
      if lt_num n (Int 0) then
        list_of_neg [] n
      else list_of_pos [] n in

    Writer.of_list chars

  let random () = num_of_int (Random.int max_r_int - max_r_int / 2)

  (* XXX not BER safe *)
  let nat_string_of_cs cs =
    match Cstruct.get_uint8 cs 0 with
    | 0x00            -> Cstruct.(to_string @@ shift cs 1)
    | n when n < 0x80 -> Cstruct.to_string cs
    | _ -> invalid_arg "nat_string_of_cs: negative integer; expecting nat"

  let cs_of_nat_string str =
    match int_of_char str.[0] >= 0x80 with
    | false -> Cstruct.of_string str
    | true  ->
        let n  = String.length str in
        let cs = Cstruct.create (n + 1) in
        Cstruct.set_uint8 cs 0 0;
        Cstruct.blit_from_string str 0 cs 1 n;
        cs

end

module Gen_string : String_primitive with type t = string = struct

  type t = string

  let of_cstruct n buf = Cstruct.(to_string @@ sub buf 0 n)

  let to_writer = Writer.of_string

  let random ?size () =
    let n = random_size size in
    let s = String.create n in
    for i = 0 to n - 1 do s.[i] <- Char.chr (random_int_r 32 126) done;
    s

  let (concat, length) = String.(concat "", length)
end

module Octets : String_primitive with type t = Cstruct.t = struct

  type t = Cstruct.t

  let of_cstruct n buf =
    let cs' = Cstruct.sub buf 0 n in
    (* mumbo jumbo to retain cs equality *)
    Cstruct.(of_bigarray @@
      Bigarray.Array1.sub cs'.buffer cs'.off cs'.len)

  let to_writer = Writer.of_cstruct

  let random ?size () =
    let n   = random_size size in
    let str = String.create n in
    for i = 0 to n - 1 do
      str.[i] <- char_of_int @@ Random.int 256
    done;
    Cstruct.of_string str

  let concat = cs_concat

  let length = Cstruct.len

end

module Bits : sig
  include String_primitive with type t = int * Cstruct.t
  val array_of_pair : t -> bool array
  val pair_of_array : bool array -> t
end
  =
struct

  type t = int * Cstruct.t

  let of_cstruct n buf =
    let unused = Cstruct.get_uint8 buf 0
    and octets = Octets.of_cstruct (n - 1) (Cstruct.shift buf 1) in
    (unused, octets)

  let to_writer (unused, cs) =
    let size = Cstruct.len cs in
    let write off buf =
      Cstruct.set_uint8 buf off unused;
      Cstruct.blit cs 0 buf (off + 1) size in
    Writer.immediate (size + 1) write


  let array_of_pair (unused, cs) =
    Array.init (Cstruct.len cs * 8 - unused) @@ fun i ->
      let byte = (Cstruct.get_uint8 cs (i / 8)) lsl (i mod 8) in
      byte land 0x80 = 0x80

  let (|<) n = function
    | true  -> (n lsl 1) lor 1
    | false -> (n lsl 1)

  let pair_of_array arr =
    let cs =
      Cstruct.create
        ( match Array.length arr with
          | 0 -> 0
          | n -> (n - 1) / 8 + 1 ) in
    match
      Array.fold_left
        (fun (n, acc, i) bit ->
          if n = 8 then
            ( Cstruct.set_uint8 cs i acc ; (1, 0 |< bit, i + 1) )
          else (n + 1, acc |< bit, i))
        (0, 0, 0)
        arr
    with
    | (0, acc, _) -> (0, cs)
    | (n, acc, i) ->
        Cstruct.set_uint8 cs i (acc lsl (8 - n));
        (8 - n, cs)

  let random ?size () = (0, Octets.random ?size ())

  let concat css =
    let (unused, css') =
      let rec go = function
        | []           -> (0, [])
        | [(u, cs)]    -> (u, [cs])
        | (_, cs)::ucs -> let (u, css') = go ucs in (u, cs::css') in
      go css in
    (unused, cs_concat css')

  and length (unused, cs) = Cstruct.len cs - unused

end

module OID : sig

  include Prim

  val (<|)      : t -> int -> t
  val (<||)     : t -> int list -> t
  val base      : int -> int -> t
  val to_list   : t -> int list
  val to_string : t -> string
  val of_string : string -> t

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

  let of_cstruct n buf =

    let rec values i =
      if i = n then []
      else let (i', v) = component 0 i in v :: values i'

    and component acc i =
      let byte = Cstruct.get_uint8 buf i in
      let acc' = acc lor (byte land 0x7f) in
      match byte land 0x80 with
      | 0 -> (succ i, acc')
      | _ -> component (acc' lsl 7) (succ i) in

    let b1 = Cstruct.get_uint8 buf 0 in
    let (v1, v2) = (b1 / 40, b1 mod 40) in

    Oid (v1, v2, values 1)

  let to_writer = fun (Oid (v1, v2, vs)) ->
    let cons x = function [] -> [x] | xs -> x lor 0x80 :: xs in
    let rec component xs x =
      if x < 0x80 then cons x xs
      else component (cons (x land 0x7f) xs) (x lsr 7)
    and values = function
      | []    -> Writer.empty
      | v::vs -> Writer.(of_list (component [] v) <> values vs) in
    Writer.(of_byte (v1 * 40 + v2) <> values vs)

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
      match S.pred_at C.digit str 12 with
      | false -> (0, 0., 12)
      | true  ->
          let ss = rn2 str 12 in
          match S.pred_at C.(is '.') str 14 with
          | false -> (ss, 0., 14)
          | true  ->
              let rec scan_f acc e i =
                if S.pred_at C.digit str i then
                  scan_f (acc * 10 + rn1 str i) (e * 10) (succ i)
                else (ss, float acc /. float e, i) in
              scan_f 0 1 15 in
    let tz = tz_of_string_optional (S.drop tz_off str)
    in
    { date = (yy, mm, dd) ; time = (hh, mi, ss, sf) ; tz }

  let string_of_list list =
    let b  = Buffer.create 16 in
    let () = List.iter (Buffer.add_char b) list in
    Buffer.contents b

  let rec take n = function
    | []    -> []
    | x::xs -> if n > 0 then x :: take (pred n) xs else []

  (* The most ridiculously convoluted way to print three decimal digits.
   * When in doubt, multiply 0.57 by 100.  *)

  let pf3 f =
    let str = Printf.sprintf "%.03f" f in
    let rec dump acc i =
      match (str.[i], acc) with
      | '0', [] -> dump acc (pred i)
      | '.', [] -> []
      | '.', xs -> '.' :: take 3 xs
      | c  , xs -> dump (c::acc) (pred i) in
    let digits = dump [] (String.length str - 1) in
    string_of_list digits

  let gen_time_to_string ?(ber=false) t =
    match (ber, t.tz) with
    | (false, _) | (true, None) | (true, Some (0, 0, _)) ->
        let (yy, mm, dd)     = t.date
        and (hh, mi, ss, sf) = t.time in
        String.concat "" [
          pn4 yy ; pn2 mm ; pn2 dd ; pn2 hh ; pn2 mi ;
          ( if ss = 0 && sf = 0. then "" else
            if sf = 0. then pn2 ss
            else pn2 ss ^ pf3 sf ) ;
          tz_to_string t.tz
        ]
    | _ -> invalid_arg "GeneralizedTime: can't encode time in non-GMT zone"


  let random ?(fraction=false) () =
    let num n = Random.int n + 1 in
    let sec   = if Random.int 3 = 0 then 0 else num 59
    and sec_f = if fraction then float (Random.int 1000) /. 1000. else 0.
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
  

