

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
  let cs = Cstruct.(create @@ lenv list) in
  let _  = List.fold_left
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
    | (0, _acc, _) -> (0, cs)
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

module OID = struct

  include Asn_oid

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

    base v1 v2 <|| values 1

  let to_writer = fun (Oid (v1, v2, vs)) ->
    let cons x = function [] -> [x] | xs -> x lor 0x80 :: xs in
    let rec component xs x =
      if x < 0x80 then cons x xs
      else component (cons (x land 0x7f) xs) (x lsr 7)
    and values = function
      | []    -> Writer.empty
      | v::vs -> Writer.(of_list (component [] v) <> values vs) in
    Writer.(of_byte (v1 * 40 + v2) <> values vs)

  let random () =
    Random.( base (int 3) (int 40) <|| replicate_l (int 10) random_int )

end

module Time = struct

  type t = Asn_time.t

  open Asn_time

  let catch fn = try Some (fn ()) with _ -> None

  let frac f = f -. floor f

  let round f =
    int_of_float @@ if frac f < 0.5 then floor f else ceil f

  let tz_of_string_exn = function
    | "Z"|"" -> None
    | str    ->
        Scanf.sscanf str "%1[+-]%02u%02u%!" @@
          fun sgn h m -> match sgn with
            | "+" -> Some (h, m, `E)
            | "-" -> Some (h, m, `W)
            | _   -> None

  let time_of_string_utc str = catch @@ fun () ->
    Scanf.sscanf str
    "%02u%02u%02u%02u%02u%s" @@
    fun y m d hh mm rest ->
      let (ss, tz) =
        try Scanf.sscanf rest "%02u%s" @@ fun ss rest' ->
          (ss, tz_of_string_exn rest')
        with _ ->
          (0, tz_of_string_exn rest)
      in
      let y = if y < 50 then 2000 + y else 1900 + y in
      { date = (y, m, d) ; time = (hh, mm, ss, 0.) ; tz }

  let time_of_string_gen str = catch @@ fun () ->
    Scanf.sscanf str "%04u%02u%02u%02u%02u%s" @@
    fun y m d hh mm rest ->
      let (ssff, tz) =
        try Scanf.sscanf rest "%f%s" @@ fun ssff rest' ->
          (ssff, tz_of_string_exn rest')
        with _ ->
          (0., tz_of_string_exn rest)
      in
      let ss = int_of_float ssff
      and ff = frac ssff in
      { date = (y, m, d) ; time = (hh, mm, ss, ff) ; tz }


  let tz_to_string = function
    | None             -> "Z"
    | Some (h, m, sgn) ->
        Printf.sprintf "%c%02d%02d"
          (match sgn with `E -> '+' | `W -> '-') h m

  let time_to_string_utc t =
    let (y, m, d)       = t.date
    and (hh, mm, ss, _) = t.time in
    Printf.sprintf "%02d%02d%02d%02d%02d%02d%s"
      (y mod 100) m d hh mm ss (tz_to_string t.tz)

  (* The most ridiculously convoluted way to print three decimal digits.
   * When in doubt, multiply 0.57 by 100. *)
  (* XXX Assumes x = a * 10^(-n) + epsilon for natural a, n. *)
  let string_of_frac n x =
    let i   = round (frac x *. 10. ** float n) in
    let str = string_of_int i in
    let rec rstrip_0 = function
      | 0 -> ""
      | i ->
          match str.[i - 1] with
          | '0' -> rstrip_0 (pred i)
          | _   -> "." ^ String.sub str 0 i in
    rstrip_0 String.(length str)

  (* XXX BER-times must be UTC-normalized. Not sure whether optional ss and ff
   * are allowed to be zero-only.  *)
  let time_to_string_gen t =
    let (y, m, d)        = t.date
    and (hh, mm, ss, ff) = t.time in
    Printf.sprintf "%04d%02d%02d%02d%02d%02d%s%s"
      y m d hh mm ss (string_of_frac 3 ff) (tz_to_string t.tz)


  let random ?(fraction=false) () =
    let num n = Random.int n + 1 in
    let sec   = if Random.int 3 = 0 then 0 else num 59
    and sec_f = if fraction then float (Random.int 1000) /. 1000. else 0.
    and tz    = match Random.int 3 with
      | 0 -> None
      | 1 -> Some (num 11, num 59, `E)
      | 2 -> Some (num 11, num 59, `W)
      | _ -> assert false
    in
    { date = (1950 + num 99, num 12, num 30) ;
      time = (num 23, num 59, sec, sec_f) ;
      tz   = tz }

  module Str = Gen_string

end
