
(* generalities... *)

type 'a endo = 'a -> 'a

let id x = x

let const x _ = x

let comp f g x = f (g x)


module Bytekit = struct

  open Bigarray

  type bytes = (int, int8_unsigned_elt, c_layout) Array1.t

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

end

module Prim = struct

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

end


module Core = struct

  open Prim

  type ('a, 'b) sum = L of 'a | R of 'b

  type tag =
    | Universal of int
    | Application of int
    | Context_specific of int
    | Private of int

  type tags = tag list

  exception Ambiguous_tags

  exception Invalid_encoding
  exception End_of_input

  type _ asn =

    | Iso : ('a -> 'b) * ('b -> 'a) * 'a asn -> 'b asn
    | Fix : ('a asn -> 'a asn) -> 'a asn

    | Sequence    : 'a sequence -> 'a asn
    | Sequence_of : 'a asn -> 'a list asn
    | Set         : 'a sequence -> 'a asn
    | Set_of      : 'a asn -> 'a list asn
    | Choice      : 'a asn * 'b asn -> ('a, 'b) sum asn

    | Implicit : tag * 'a asn -> 'a asn
    | Explicit : tag * 'a asn -> 'a asn

    | Prim : 'a prim -> 'a asn

  and _ element =

    | Required : 'a asn -> 'a element
    | Optional : 'a asn -> 'a option element

  and _ sequence =

    | Last : 'a element -> 'a sequence
    | Pair : 'a element * 'b sequence -> ('a * 'b) sequence

  and _ prim =

    | Bool      : bool prim
    | Int       : Integer.t prim
    | Null      : unit prim
    | IA5String : string prim


  let sequence_tag = Universal 0x10
  and set_tag      = Universal 0x11

  let tag_of_p : type a. a prim -> tag = function

    | Bool      -> Universal 0x01
    | Int       -> Universal 0x02
    | Null      -> Universal 0x05
    | IA5String -> Universal 0x16

  let rec tagset : type a. a asn -> tags = function

    | Iso (_, _, asn) -> tagset asn
    | Fix f as fix    -> tagset (f fix)

    | Sequence    _ -> [ sequence_tag ]
    | Sequence_of _ -> [ sequence_tag ]
    | Set _         -> [ set_tag ]
    | Set_of _      -> [ set_tag ]
    | Choice (asn1, asn2) -> tagset asn1 @ tagset asn2

    | Implicit (t, _) -> [ t ]
    | Explicit (t, _) -> [ t ]

    | Prim p -> [ tag_of_p p ]

  let rec tag : type a. a -> a asn -> tag
  = fun a -> function

    | Iso (_, g, asn) -> tag (g a) asn
    | Fix f as fix    -> tag a fix
    | Sequence _      -> sequence_tag
    | Sequence_of _   -> sequence_tag
    | Set _           -> set_tag
    | Set_of _        -> set_tag
    | Choice (a1, a2) -> (match a with L a' -> tag a' a1 | R b' -> tag b' a2)
    | Implicit (t, _) -> t
    | Explicit (t, _) -> t
    | Prim p          -> tag_of_p p

end

module Combinators = struct

  open Core

  (* Horrible, horrible hack.
   * Extend the Core.asn type to fix. *)
  module Fix_cache (T : sig type 'a t end) : sig
    val cached : ('a asn -> 'a asn) -> (unit -> 'a T.t) -> 'a T.t
    val add : ('a asn -> 'a asn) -> 'a T.t -> 'a T.t
    val find : ('a asn -> 'a asn) -> 'a T.t
  end
  =
  struct
    let cast = Obj.magic
    let cache : (unit, unit) Hashtbl.t = Hashtbl.create 100
    let cached key cons =
      let key' = cast key in
      try cast (Hashtbl.find cache key') with
      | Not_found ->
          let res = cons () in
          ( Hashtbl.add cache key' (cast res) ; res )
    let add k v = ( Hashtbl.add cache (cast k) (cast v) ; v )
    let find k  = cast (Hashtbl.find cache @@ cast k)
  end


  let fix f = Fix f

  let map f g asn = Iso (f, g, asn)

  let implicit, explicit =
    let tag = function
      | (None, n) -> Context_specific n
      | (Some `Application, n) -> Application n
      | (Some `Private, n) -> Private n in
    ( (fun ?cls id asn -> Implicit (tag (cls, id), asn))
    , (fun ?cls id asn -> Explicit (tag (cls, id), asn)) )

  let bool       = Prim Bool
  and int        = Prim Int
  and null       = Prim Null
  and ia5_string = Prim IA5String

  let last a   = Last a
  and (@)  a b = Pair (a, b)
  and (!!) a   = Last (Required a)
  and (!?) a   = Last (Optional a)
  and (@!) a b = Pair (Required a, b)
  and (@?) a b = Pair (Optional a, b)
  and optional a = Optional a
  and required a = Required a

  let product2 fn a b = fn @@ a @ last b

  let product3 fn a b c =
    map (fun (a, (b, c)) -> (a, b, c))
        (fun (a, b, c) -> (a, (b, c)))
        (fn @@ a @ b @ last c)

  let product4 fn a b c d =
    map (fun (a, (b, (c, d))) -> (a, b, c, d))
        (fun (a, b, c, d) -> (a, (b, (c, d))))
        (fn @@ a @ b @ c @ last d)

  let product5 fn a b c d e =
    map (fun (a, (b, (c, (d, e)))) -> (a, b, c, d, e))
        (fun (a, b, c, d, e) -> (a, (b, (c, (d, e)))))
        (fn @@ a @ b @ c @ d @ last e)

  let product6 fn a b c d e f =
    map (fun (a, (b, (c, (d, (e, f))))) -> (a, b, c, d, e, f))
        (fun (a, b, c, d, e, f) -> (a, (b, (c, (d, (e, f))))))
        (fn @@ a @ b @ c @ d @ e @ last f)


  let sequence seq = Sequence seq

  let sequence2 a b         = product2 sequence a b
  and sequence3 a b c       = product3 sequence a b c
  and sequence4 a b c d     = product4 sequence a b c d
  and sequence5 a b c d e   = product5 sequence a b c d e
  and sequence6 a b c d e f = product6 sequence a b c d e f

  let sequence_of asn = Sequence_of asn

  let set seq = Set seq

  let set2 a b         = product2 set a b
  and set3 a b c       = product3 set a b c
  and set4 a b c d     = product4 set a b c d
  and set5 a b c d e   = product5 set a b c d e
  and set6 a b c d e f = product6 set a b c d e f

  let choice a b = Choice (a, b)

  let choice2 a b =
    map (function L a -> `C1 a | R b -> `C2 b)
        (function `C1 a -> L a | `C2 b -> R b)
        (choice a b)

  let choice3 a b c =
    map (function L (L a) -> `C1 a | L (R b) -> `C2 b | R c -> `C3 c)
        (function `C1 a -> L (L a) | `C2 b -> L (R b) | `C3 c -> R c)
        (choice (choice a b) c)

  let choice4 a b c d =
    map (function | L (L a) -> `C1 a | L (R b) -> `C2 b
                  | R (L c) -> `C3 c | R (R d) -> `C4 d)
        (function | `C1 a -> L (L a) | `C2 b -> L (R b)
                  | `C3 c -> R (L c) | `C4 d -> R (R d))
        (choice (choice a b) (choice c d))

  let choice5 a b c d e =
    map (function | L (L (L a)) -> `C1 a | L (L (R b)) -> `C2 b
                  | L (R c) -> `C3 c
                  | R (L d) -> `C4 d | R (R e) -> `C5 e)
        (function | `C1 a -> L (L (L a)) | `C2 b -> L (L (R b))
                  | `C3 c -> L (R c)
                  | `C4 d -> R (L d) | `C5 e -> R (R e))
        (choice (choice (choice a b) c) (choice d e))

  let choice6 a b c d e f =
    map (function | L (L (L a)) -> `C1 a | L (L (R b)) -> `C2 b
                  | L (R c) -> `C3 c
                  | R (L (L d)) -> `C4 d | R (L (R e)) -> `C5 e
                  | R (R f) -> `C6 f)
        (function | `C1 a -> L (L (L a)) | `C2 b -> L (L (R b))
                  | `C3 c -> L (R c)
                  | `C4 d -> R (L (L d)) | `C5 e -> R (L (R e))
                  | `C6 f -> R (R f))
        (choice (choice (choice a b) c) (choice (choice d e) f))


  (*
   * Check tag ambiguity.
   * XXX Maybe add no-implicit-over-choice check.
   *)

  let validate asn =

    let module C  = Fix_cache (struct type 'a t = unit end) in

    let rec disjunct tss =
      let rec go = function
        | t::(u::_ as ts) ->
            if t <> u then go ts else raise Ambiguous_tags
        | [] | [_] -> () in
      go List.(sort compare @@ concat tss)

    and ck_seq : type a. tags list * a sequence -> unit = function
      | ts, Last (Optional x)     -> check x ; disjunct (tagset x :: ts)
      | [], Last (Required x)     -> check x
      | ts, Last (Required x)     -> check x ; disjunct (tagset x :: ts)
      | ts, Pair (Optional x, xs) -> check x ; ck_seq (tagset x :: ts, xs)
      | [], Pair (Required x, xs) -> check x ; ck_seq ([], xs)
      | ts, Pair (Required x, xs) ->
          check x ; disjunct (tagset x :: ts) ; ck_seq ([], xs)

    and ck_set : type a. a sequence -> tags list = function
      | Last (Required x)     -> check x ; [ tagset x ]
      | Last (Optional x)     -> check x ; [ tagset x ]
      | Pair (Required x, xs) -> check x ; tagset x :: ck_set xs
      | Pair (Optional x, xs) -> check x ; tagset x :: ck_set xs

    and check : type a. a asn -> unit = function

      | Iso (_, _, asn) -> check asn
      | Fix f as fix ->
        ( try C.find f with Not_found -> C.add f () ; check (f fix) )

      | Sequence asns   -> ck_seq ([], asns)
      | Set      asns   -> disjunct @@ ck_set asns
      | Sequence_of asn -> check asn
      | Set_of      asn -> check asn

      | Choice (asn1, asn2) ->
          disjunct [ tagset asn1; tagset asn2 ] ; check asn1 ; check asn2

      | Implicit (_, asn) -> check asn
      | Explicit (_, asn) -> check asn
      | Prim _ -> () in

    try check asn with Stack_overflow -> raise Ambiguous_tags

end


module BER = struct
  
  open Core
  open Bytekit

  module RichMap (M : Map.OrderedType) = struct
    module Impl = Map.Make (M)
    include Impl

    let of_list xs =
      List.fold_left (fun m (k, e) -> add k e m) empty xs

    let of_keys ks v =
      List.fold_left (fun m k -> add k v m) empty ks

    let union m1 m2 =
      let right _ a b =
        match (a, b) with
        | _, Some e  -> Some e
        | Some e, _  -> Some e
        | None, None -> None in
      merge right m1 m2

    let unions ms = List.fold_left union empty ms
  end

  module R = struct

    type coding =
      | Primitive of int
      | Constructed of int
      | Constructed_indefinite

    type header = { tag : tag ; coding : coding ; buf : bytes }

    type 'a parser = header -> 'a * bytes


    let (>|=) prs f = fun header ->
      let (a, buf') = prs header in (f a, buf')


    module Partial = struct
      module C = Core

      type _ element =
        | Required : [`Nada | `Found of 'a] -> 'a element
        | Optional : [`Nada | `Found of 'a] -> 'a option element

      type _ sequence =
        | Last : 'a element -> 'a sequence
        | Pair : 'a element * 'b sequence -> ('a * 'b) sequence

      let rec of_complete : type a. a C.sequence -> a sequence = function
        | C.Last (C.Required _   ) -> Last (Required `Nada)
        | C.Last (C.Optional _   ) -> Last (Optional `Nada)
        | C.Pair (C.Required _, t) -> Pair (Required `Nada, of_complete t)
        | C.Pair (C.Optional _, t) -> Pair (Optional `Nada, of_complete t)

      let to_complete_exn =
        let rec f1 : type a. a element -> a = function
          | Required  `Nada     -> invalid_arg "partial sequence: incomplete"
          | Required (`Found a) -> a
          | Optional  `Nada     -> None
          | Optional (`Found a) -> Some a
        and f2 : type a. a sequence -> a = function
          | Last  e      ->  f1 e
          | Pair (e, tl) -> (f1 e, f2 tl) in
        f2
    end


    let is_sequence_end buf =
      buf.{0} = 0x00 && buf.{1} = 0x00

    let drop_sequence_end buf = Rd.drop 2 buf


    let p_big_tag buf =
      let rec loop acc i =
        let byte = buf.{i} in
        let acc' = byte land 0x7f + acc in
        match byte land 0x80 with
        | 0 -> (acc', succ i)
        | _ -> loop (acc' * 0x80) (succ i) in
      loop 0 1


    let p_big_length buf off n =
      let last = off + n in
      let rec loop acc i =
        if i > last then (acc, i) else
          loop (acc * 0x100 + buf.{i}) (succ i) in
      loop 0 (succ off)


    let p_header buffer =

      let b0 = buffer.{0} in
      let t_class       = b0 land 0xc0
      and t_constructed = b0 land 0x20
      and t_tag         = b0 land 0x1f in
 
      let tagn, length_off =
        match t_tag with
        | 0x1f -> p_big_tag buffer
        | n    -> (n, 1) in

      let l0 = buffer.{length_off} in
      let t_ltype  = l0 land 0x80
      and t_length = l0 land 0x7f in

      let length, contents_off =
        match t_ltype with
        | 0 -> (t_length, succ length_off)
        | _ -> p_big_length buffer length_off t_length
      in

      let tag =
        match t_class with
        | 0x00 -> Universal tagn
        | 0x40 -> Application tagn
        | 0x80 -> Context_specific tagn
        | 0xc0 -> Private tagn
        | _    -> assert false

      and coding =
        match (t_constructed, l0) with
        | (0, _   ) -> Primitive length
        | (_, 0x80) -> Constructed_indefinite
        | _         -> Constructed length

      and rest = Rd.drop contents_off buffer in

      { tag = tag ; coding = coding ; buf = rest }


    let halt ?why () =
      ( match why with
        | Some str -> Printf.printf "HALT: %s\n%!" str
        | None     -> Printf.printf "HALT.\n%!"  );
      raise Invalid_encoding


    let accepts : type a. a asn * header -> bool = function
      | (asn, { tag }) -> List.mem tag (tagset asn)


    let constructed
    : type a.  ((bytes -> bool) -> bytes -> a * bytes) -> a parser
    = fun fn -> function

      | { coding = Constructed n ; buf } -> 
          let (b1, b2) = Rd.isolate n buf in
          let (a, b1') = fn Rd.eof b1 in
          if Rd.eof b1' then (a, b2) else halt ()

      | { coding = Constructed_indefinite ; buf } ->
          let (a, buf') = fn is_sequence_end buf in
          if is_sequence_end buf' then
            (a, drop_sequence_end buf')
          else halt ()

      | { coding = Primitive _ } -> halt ()


    let primitive : type a. (int -> bytes -> a) -> a parser
    = fun fn -> function
      | { coding = Primitive n ; buf } ->
          (fn n buf, Rd.drop n buf)
      | _ -> halt ()


    let primitive_n : type a. int -> (bytes -> a) -> a parser
    = fun len fn -> function
      | { coding = Primitive n ; buf } when n = len ->
          (fn buf, Rd.drop len buf)
      | _ -> halt ()


    let sequence_of_parser prs =
      constructed @@ fun eof buf0 ->
        let rec scan acc buf =
          if eof buf then (List.rev acc, buf)
          else
            let (a, buf') = prs (p_header buf) in
            scan (a :: acc) buf' in
        scan [] buf0


    let string_like combine atom =
      let rec prs = function
        | { coding = Primitive n ; buf } -> (atom n buf, Rd.drop n buf)
        | h -> (sequence_of_parser prs >|= combine) h in
      prs


    let parser_of_prim : type a. a prim -> a parser = function

      | Bool -> primitive_n 1 @@ fun buf -> buf.{0} <> 0x00

      | Int  -> primitive Prim.Integer.of_bytes

      | Null -> primitive_n 0 @@ fun _ -> ()

      | IA5String ->
          string_like String.(concat "") Prim.String.ascii_of_bytes


    module Cache = Combinators.Fix_cache (struct type 'a t = 'a parser end)

    let rec parser_of_asn : type a. a asn -> a parser = function

      | Iso (f, _, asn) -> parser_of_asn asn >|= f

      | Fix fasn as fix ->
        ( try Cache.find fasn with Not_found ->
          Cache.add fasn @@
            let lprs = lazy (parser_of_asn (fasn fix)) in
            fun header -> Lazy.force lprs header )


      | Sequence asns ->

          let module S = struct
            type 'a touch = Hit of 'a * bytes | Pass of 'a
          end in
          let open S in

          let rec elt : type a. a element -> header -> a S.touch
          = function
            | Required asn ->
                let prs = parser_of_asn asn in fun header ->
                  if accepts (asn, header) then
                    let (a, buf) = prs header in Hit (a, buf)
                  else halt ()
            | Optional asn ->
                let prs = parser_of_asn asn in fun header ->
                  if accepts (asn, header) then
                    let (a, buf) = prs header in Hit (Some a, buf)
                  else Pass None

          and seq : type a. a sequence -> (bytes -> bool) -> a parser
          = function
            | Last e ->
                let prs = elt e in fun _ header ->
                ( match prs header with
                  | Pass a        -> halt ()
                  | Hit (a, buf') -> (a, buf') )
            | Pair (e, tl) ->
                let (prs1, prs2) = elt e, seq tl in fun eof header ->
                ( match prs1 header with
                  | Hit (a, buf) when eof buf ->
                      ((a, default_or_halt tl), buf)
                  | Hit (a, buf) ->
                      let (b, buf') = prs2 eof (p_header buf) in ((a, b), buf')
                  | Pass a ->
                      let (b, buf') = prs2 eof header in ((a, b), buf') )

          and default_or_halt : type a. a sequence -> a = function
            | Last (Required _   ) -> halt ()
            | Pair (Required _, _) -> halt ()
            | Last (Optional _   ) -> None
            | Pair (Optional _, s) -> None, default_or_halt s
             
          in
          let prs = seq asns in
          constructed @@ fun eof buf ->
            if eof buf then (default_or_halt asns, buf)
            else prs eof (p_header buf)

      | Set asns ->

          let module P = Partial in
          let module TM = RichMap ( struct
            type t = tag let compare = compare
          end ) in

          let rec partial_e : type a. a element -> tags * a P.element parser
            = function
            | Required asn ->
                ( tagset asn, 
                  parser_of_asn asn >|= fun a -> P.Required (`Found a) )
            | Optional asn ->
                ( tagset asn, 
                  parser_of_asn asn >|= fun a -> P.Optional (`Found a) )

          and setters :
            type a b. ((a P.sequence endo) -> b P.sequence endo)
                   -> a sequence
                   -> (tags * (b P.sequence endo) parser) list
            = fun k -> function

            | Last e ->
                let (tags, prs) = partial_e e in
                [(tags, prs >|= fun e' -> k (fun _ -> P.Last e'))]

            | Pair (e, rest) ->
                let put r = function
                  | P.Pair (_, tl) -> P.Pair (r, tl)
                  | _               -> assert false
                and wrap f = k @@ function
                  | P.Pair (v, tl) -> P.Pair (v, f tl)
                  | _               -> assert false
                and (tags, prs1) = partial_e e in
                (tags, prs1 >|= comp k put) :: setters wrap rest 

          in
          let parsers =
            TM.unions @@ List.map (fun (tags, prs) -> TM.of_keys tags prs)
                                  (setters id asns)

          and zero = P.of_complete asns in 

          constructed @@ fun eof buf0 ->
            let rec scan partial buf =
              if eof buf then (P.to_complete_exn partial, buf)
              else
                let header = p_header buf in
                let (f, buf') = TM.find header.tag parsers header in
                scan (f partial) buf'
            in
            ( try scan zero buf0 with Not_found -> halt () )


      | Sequence_of asn -> sequence_of_parser @@ parser_of_asn asn 

      | Set_of asn -> sequence_of_parser @@ parser_of_asn asn


      | Choice (asn1, asn2) ->

          let (prs1, prs2) = (parser_of_asn asn1, parser_of_asn asn2) in
          fun header ->
            if accepts (asn1, header) then
              let (a, buf') = prs1 header in (L a, buf')
            else
              let (b, buf') = prs2 header in (R b, buf')

      | Implicit (_, asn) -> parser_of_asn asn

      | Explicit (_, asn) ->
          constructed @@ const (comp (parser_of_asn asn) p_header)

      | Prim p -> parser_of_prim p


    let parser : 'a asn -> bytes -> 'a * bytes
    = fun asn ->
      let prs = parser_of_asn asn in
      fun buf0 ->
        try
          let header = p_header buf0 in
          if accepts (asn, header) then prs header else halt ()
        with Invalid_argument _ -> raise End_of_input

  end

  module W = struct

    let (<>) = Wr.(<>)

    let e_big_tag tag =
      let rec loop acc n =
        if n = 0 then acc else
          let acc' =
            match acc with
            | [] -> [ n land 0x7f ]
            | _  -> ((n land 0x7f) lor 0x80) :: acc in
          loop acc' (n lsr 7) in
      loop [] tag

    let e_big_length length =
      let rec loop acc n =
        if n = 0 then acc else
          loop (n land 0xff :: acc) (n lsr 8) in
      loop [] length

    let e_header tag mode len =

      let (klass, tagn) =
        match tag with
        | Universal n        -> (0x00, n)
        | Application n      -> (0x40, n)
        | Context_specific n -> (0x80, n)
        | Private n          -> (0xc0, n) in

      let constructed =
        match mode with
        | `Primitive   -> 0x00
        | `Constructed -> 0x20 in

      ( if tagn < 0x1f then
          Wr.byte (klass lor constructed lor tagn)
        else
          Wr.byte (klass lor constructed lor 0x1f) <>
            Wr.list (e_big_tag tagn) )
      <>
      ( if len <= 0x7f then
          Wr.byte len
        else
          let body = Wr.list (e_big_length len) in
          Wr.byte (0x80 lor Wr.size body) <> body )


    let (@?) o def =
      match o with | None -> def | Some x -> x

    let e_constructed tag body =
      e_header tag `Constructed (Wr.size body) <> body

    let e_primitive prim mtag body =
      let tag =
        match mtag with
        | Some x -> x | None -> tag_of_p prim in
      e_header tag `Primitive (Wr.size body) <> body

    let rec encode : type a. tag option -> a -> a asn -> Wr.t
    = fun tag a -> function

      | Iso (_, g, asn) -> encode tag (g a) asn

      | Fix asn as fix -> encode tag a (asn fix)

      | Sequence asns ->
          e_constructed (tag @? Universal 0x10)
                        (e_seq a asns)

      | Sequence_of asn -> (* size/stack? *)
          e_constructed (tag @? Universal 0x10) @@
            List.fold_right
              (fun e r -> encode None e asn <> r)
              a Wr.empty

      | Set asns -> assert false (* DER sort *)
(*           e_constructed (tag @? Universal 0x11)
                        (e_seq a asns) *)

      | Set_of asns -> assert false (* DER sort *)

      | Choice (asn1, asn2) ->
        ( match a with
          | L a' -> encode tag a' asn1
          | R b' -> encode tag b' asn2 )

      | Implicit (t, asn) ->
          encode (Some (tag @? t)) a asn

      | Explicit (t, asn) ->
          e_constructed (tag @? t) (encode None a asn)

      | Prim p -> e_prim tag a p


    and e_prim : type a. tag option -> a -> a prim -> Wr.t
    = fun tag a -> function

      | Bool as prim ->
          e_primitive prim tag @@
            Wr.byte (if a then 0xff else 0x00)

      | Int as prim ->
          e_primitive prim tag @@ Prim.Integer.to_bytes a

      | Null as prim -> e_primitive prim tag Wr.empty

      | IA5String as prim ->
          e_primitive prim tag @@ Prim.String.ascii_to_bytes a


    and e_elt : type a. a -> a element -> Wr.t
    = fun a -> function
      | Required asn -> encode None a asn
      | Optional asn ->
        ( match a with
          | None    -> Wr.empty
          | Some a' -> encode None a' asn)

    and e_seq : type a. a -> a sequence -> Wr.t
    = fun a -> function
      | Last  elt      -> e_elt a elt
      | Pair (elt, tl) ->
          let (a1, a2) = a in
          e_elt a1 elt <> e_seq a2 tl

    let encode_to_bytes asn a =
      Wr.to_bytes @@ encode None a asn

  end

end


module ASN = struct

  type 'a t = 'a Core.asn

  include Combinators

  module B = Bytekit

  type encoding = {
    mk_decoder : 'a. 'a t -> B.bytes -> 'a * B.bytes;
    mk_encoder : 'a. 'a t -> 'a -> B.bytes
  }

  let ber_der = {
    mk_encoder = BER.W.encode_to_bytes ;
    mk_decoder = BER.R.parser
  }

  type 'a codec = Codec of (B.bytes -> ('a * B.bytes)) * ('a -> B.bytes)

  let codec { mk_encoder ; mk_decoder } asn =
    let () = validate asn in
    Codec (mk_decoder asn, mk_encoder asn)

  let encode (Codec (_, enc)) a = enc a

  and decode_exn (Codec (dec, _)) b = dec b

  and decode (Codec (dec, _)) b =
    try Some (dec b) with
    ( Core.End_of_input | Core.Invalid_encoding ) -> None

end



(* debug *)



module Dumpkit = struct

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

  let list_of_a1 bigarr =
    let rec go a n =
      if n < 0 then a else go (bigarr.{n} :: a) (n - 1) in
    go [] (Bigarray.Array1.dim bigarr - 1)

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

  let ba_of_list list =
    let length = List.length list in
    let arr    = Bigarray.(
      Array1.create int8_unsigned c_layout length) in
    ( write_list arr list ; arr )
end





module Test = struct

  let time_it fn =
    let t1 = Unix.gettimeofday () in
    let res = fn () in
    let t2 = Unix.gettimeofday () in
    Printf.printf "[time] %04f s\n%!" (t2 -. t1);
    res

  let time_it_n n fn =
    time_it @@ fun () ->
      let rec loop i =
        if i <= 1 then fn ()
        else ( ignore(fn ()) ; loop (pred i) ) in
      loop n

  let replicate n f =
    let rec loop acc n =
      if n <= 0 then acc else
        loop (f () :: acc) (pred n) in
    loop [] n

  let r_int_r a b = a + Random.int (b - a + 1)

  let rstr () =
    let n = Random.int 30 in
    let s = String.create n in
    for i = 0 to n - 1 do s.[i] <- Char.chr (r_int_r 32 126) done;
    s

  let random : type a. a Core.asn -> a =
    let open Core in

    let rec random' : type a. a Core.asn -> a = function
      | Iso (f, _, asn) -> f @@ random' asn
      | Fix asn as fix -> random' (asn fix)
      | Sequence asns -> random_seq asns
      | Sequence_of asn ->
          replicate (Random.int 10)
                    (fun () -> random' asn)
      | Set asns -> random_seq asns
      | Choice (asn1, asn2) ->
          if Random.bool () then
            L (random' asn1)
          else R (random' asn2)
      | Implicit (_, asn) -> random' asn
      | Explicit (_, asn) -> random' asn
      | Prim p ->
          match p with
          | Bool -> Random.bool ()
          | Int  ->
              let x = Random.int 11 in
              if x = 10 then
                `I (Random.int ((1 lsl 30) - 1))
              else `I x
          | Null -> ()
          | IA5String -> rstr ()

    and random_elt : type a. a element -> a = function
      | Required asn -> random' asn
      | Optional asn ->
          if Random.int 3 = 0 then None
          else Some (random' asn)

    and random_seq : type a. a sequence -> a = function
      | Last e      -> random_elt e
      | Pair (h, t) -> (random_elt h, random_seq t)

    in random'

  open ASN
  open Dumpkit
  module A1 = Bigarray.Array1

  let examples = List.map ba_of_list

  let oneshot asn list =
    ASN.(decode @@ codec ber_der asn) (ba_of_list list)

  let run_test asn =
    let c = codec ber_der asn in
    List.map @@ fun bytes ->
      match decode c bytes with
      | Some (a, buf') ->
          if A1.dim buf' = 0 then
            `Decode a
          else `Leftovers
      | None           -> `No_parse

  let rec run_self_test ?(n=10) asn =
    let d = ASN.codec ber_der asn in
    let rec loop n =
      if n > 1 then
        let x = random asn in
        match ASN.decode d (ASN.encode d x) with
        | Some (y, _) ->
            if x = y then loop (n - 1)
            else `Mismatch (x, y)
        | None -> `No_decode x
      else `Ok
    in loop n

  let gen_random asn =
    let c = ASN.(codec ber_der asn)
    and a = random asn in
    let e = ASN.encode c a in
    ( Dumpkit.hex_dump e; (a, ASN.decode c e) )

  let loop_code asn =
    let c = ASN.(codec ber_der asn) in
    fun a ->
      let e = ASN.encode c a in
      Dumpkit.hex_dump e;
      match ASN.decode c e with
      | Some (a', buf') ->
          if A1.dim buf' <> 0 then `Leftovers
          else if a' <> a then `Decoded_mismatch
          else `Ok
      | None -> `No_parse

  let t1 = sequence (last @@ required bool)

  let b1 = examples [

    [ 0x30; 0x03;
        0x01; 0x01; 0xff; ];

    [ 0x30; 0x80;
        0x01; 0x01; 0xff;
        0x00; 0x00; ] ;
  ]

  let t2 =
    sequence3
      (required int)
      (optional @@ implicit 1 bool)
      (required bool)

  let b2 = examples [

    [ 0x30; 0x09;
        0x02; 0x01; 0x2a;
        0x81; 0x01; 0x00;
        0x01; 0x01; 0xff; ] ;

    [ 0x30; 0x80;
        0x02; 0x01; 0x2a;
        0x81; 0x01; 0x00;
        0x01; 0x01; 0xff;
        0x00; 0x00; ]
  ]

  let t3 =
    sequence3
      (required @@ implicit 1 int)
      (optional @@ explicit 2 bool)
      (optional @@ implicit 3 bool)

  let b3 = examples [

    [ 0x30; 0x0c;
        0x81; 0x02; 0x00; 0xff;
        0xa2; 0x03;
          0x01; 0x01; 0xf0;
        0x83; 0x01; 0x00; ] ;

    [ 0x30; 0x80;
        0x81; 0x02; 0x00; 0xff;
        0xa2; 0x03;
          0x01; 0x01; 0xf0;
        0x83; 0x01; 0x00;
        0x00; 0x00; ] ;

    [ 0x30; 0x80;
        0x81; 0x02; 0x00; 0xff;
        0xa2; 0x80;
          0x01; 0x01; 0xf0;
          0x00; 0x00;
        0x83; 0x01; 0x00;
        0x00; 0x00; ] ;
  ]

  let t4 =
    sequence3
      (required @@ choice2 bool int)
      (optional @@ choice2 bool int)
      (optional @@ explicit 0
                @@ choice2 int (implicit 1 int))

  let b4 = examples [

    [ 0x30; 0x03; 0x01; 0x01; 0xff ] ;

    [ 0x30; 0x05; 0x02; 0x03; 0x00; 0x00; 0x2a ] ;

    [ 0x30; 0x06;
        0x01; 0x01; 0x00;
        0x02; 0x01; 0x2a ] ;

    [ 0x30; 0x08;
        0x01; 0x01; 0xff;
        0xa0; 0x03;
          0x02; 0x01; 0x2a ] ;

    [ 0x30; 0x0b;
        0x02; 0x01; 0xfe;
        0x02; 0x01; 0x2a;
        0xa0; 0x03;
          0x81; 0x01; 0x2a ] ;

    [ 0x30; 0x0a;
        0x02; 0x01; 0xfd;
        0xa0; 0x80;
          0x81; 0x01; 0x2a; 0x00; 0x00; ] ;

    [ 0x30; 0x80;
        0x02; 0x01; 0xfc;
        0xa0; 0x80;
          0x02; 0x01; 0x2a; 0x00; 0x00;
        0x00; 0x00 ] ;

  ]

  let t5 =
    sequence2
      (required @@
        sequence2
          (optional @@ implicit 1 bool)
          (optional bool))
      (required bool)

  let b5 = examples [

    [ 0x30; 0x0b ;
        0x30; 0x06;
          0x81; 0x01; 0xff;
          0x01; 0x01; 0x00;
        0x01; 0x01; 0xff ] ;

    [ 0x30; 0x08 ;
        0x30; 0x03;
          0x01; 0x01; 0x00;
        0x01; 0x01; 0xff ] ;

    [ 0x30; 0x08 ;
        0x30; 0x03;
          0x81; 0x01; 0xff;
        0x01; 0x01; 0xff ] ;

    [ 0x30; 0x80 ;
        0x30; 0x80;
          0x81; 0x01; 0xff;
          0x00; 0x00;
        0x01; 0x01; 0xff;
        0x00; 0x00 ] ;
  ]

  let t6 =
    sequence2
      (required @@
        sequence_of
          (choice2 bool (implicit 0 bool)))
      (required @@ bool)

  let b6 = examples [

    [ 0x30; 0x0e;
        0x30; 0x09;
          0x80; 0x01; 0xff;
          0x80; 0x01; 0x00;
          0x01; 0x01; 0xff;
        0x01; 0x01; 0xff ] ;

    [ 0x30; 0x80;
        0x30; 0x80;
          0x80; 0x01; 0xff;
          0x80; 0x01; 0x00;
          0x01; 0x01; 0xff;
          0x00; 0x00;
        0x01; 0x01; 0xff;
        0x00; 0x00; ]
  ]

  let t7 =
    set4 (required @@ implicit 1 bool)
         (required @@ implicit 2 bool)
         (required @@ implicit 3 int )
         (optional @@ implicit 4 int )

  let b7 = examples [
    [ 0x31; 0x09;
        0x81; 0x01; 0xff;
        0x82; 0x01; 0x00;
        0x83; 0x01; 0x2a; ];
    [ 0x31; 0x0c;
        0x82; 0x01; 0x00;
        0x84; 0x01; 0xff;
        0x81; 0x01; 0xff;
        0x83; 0x01; 0x2a; ];
    [ 0x31; 0x09;
        0x82; 0x01; 0x00;
        0x83; 0x01; 0x2a;
        0x81; 0x01; 0xff; ];
    [ 0x31; 0x0c;
        0x83; 0x01; 0x2a;
        0x82; 0x01; 0x00;
        0x81; 0x01; 0xff;
        0x84; 0x01; 0x0f; ];
    [ 0x31; 0x80;
        0x82; 0x01; 0x00;
        0x83; 0x01; 0x2a;
        0x81; 0x01; 0xff;
        0x00; 0x00 ];
    [ 0x31; 0x80;
        0x83; 0x01; 0x2a;
        0x82; 0x01; 0x00;
        0x81; 0x01; 0xff;
        0x84; 0x01; 0x0f;
        0x00; 0x00 ];
  ]

  let t8 =
    choice2
      (set2 (optional int )
            (optional bool))
      (sequence2 (optional int )
                 (optional bool))

  let b8 = examples [
    [ 0x31; 0x03;
        0x01; 0x01; 0xff; ];
    [ 0x31; 0x03;
        0x02; 0x01; 0x2a; ];
    [ 0x31; 0x06;
        0x01; 0x01; 0xff;
        0x02; 0x01; 0x2a; ];
    [ 0x30; 0x03;
        0x01; 0x01; 0xff; ];
    [ 0x30; 0x03;
        0x02; 0x01; 0x2a; ];
    [ 0x30; 0x06;
        0x02; 0x01; 0x2a;
        0x01; 0x01; 0xff; ];
  ]

  let t9 = implicit 6666666 bool

  let b9 = examples [
    [ 0x9f; 0x83; 0x96; 0xf3; 0x2a; 0x01; 0xff; ] ;
    [ 0x9f; 0x83; 0x96; 0xf3; 0x2a; 0x01; 0x00; ] ;
  ]


  let t10 =
    fix @@ fun list ->
      map (function `C1 () -> [] | `C2 (x, xs) -> x::xs)
          (function [] -> `C1 () | x::xs -> `C2 (x, xs))
      @@
      choice2
        null
        (sequence2
          (required bool)
          (required list))


  let b10 = examples [

    [ 0x05; 0x00 ] ;

    [ 0x30; 0x05;
            0x01; 0x01; 0xff;
            0x05; 0x00; ] ;

    [ 0x30; 0x0f;
        0x01; 0x01; 0xff;
        0x30; 0x0a;
          0x01; 0x01; 0x00;
          0x30; 0x05;
            0x01; 0x01; 0xff;
            0x05; 0x00; ] ;

    [ 0x30; 0x80;
        0x01; 0x01; 0x00;
        0x30; 0x80;
          0x01; 0x01; 0xff;
          0x30; 0x80;
            0x01; 0x01; 0x00;
            0x05; 0x00;
            0x00; 0x00;
          0x00; 0x00;
        0x00; 0x00; ] ;

    [ 0x30; 0x80;
        0x01; 0x01; 0x00;
        0x30; 0x80;
          0x01; 0x01; 0xff;
          0x30; 0x80;
            0x01; 0x01; 0x00;
            0x05; 0x00;
            0x00; 0x00;
          0x00; 0x00;
        0x00; 0x00; ] ;

  ]

  let t11 = ia5_string

  let b11 = examples [
    [ 0x16; 0x03; 0x61; 0x62; 0x63; ];
    [ 0x36; 0x0a;
        0x16; 0x01; 0x61;
        0x16; 0x01; 0x62;
        0x16; 0x02; 0x63; 0x64; ];
    [ 0x36; 0x80;
        0x16; 0x01; 0x61;
        0x16; 0x01; 0x62;
        0x16; 0x02; 0x63; 0x64;
        0x00; 0x00; ];
    [ 0x36; 0x80;
        0x36; 0x06;
          0x16; 0x01; 0x61;
          0x16; 0x01; 0x62;
        0x16; 0x02; 0x63; 0x64;
        0x00; 0x00; ];
    [ 0x16; 0x0d; 0x74; 0x65; 0x73; 0x74; 0x31; 0x40; 0x72; 0x73;
      0x61; 0x2e; 0x63; 0x6f; 0x6d; ] ;
    [ 0x16; 0x81; 0x0d;
        0x74; 0x65; 0x73; 0x74; 0x31; 0x40; 0x72; 0x73; 0x61; 0x2e; 0x63; 0x6f; 0x6d ];
    [ 0x36; 0x13;
        0x16; 0x05; 0x74; 0x65; 0x73; 0x74; 0x31;
        0x16; 0x01; 0x40;
        0x16; 0x07; 0x72; 0x73; 0x61; 0x2e; 0x63; 0x6f; 0x6d; ]
  ]

  exception Example_failure of string * int list

(*   let example desc asn cases () =
    let check x v =
      match x with
      | `v v' -> v = v'
      | `p p  -> p v in
    let dec = BER.R.parse asn
    in
    cases |> List.iter @@ fun (bytes, result) ->
      let (a, buf) =
        try dec @@ Dumpkit.ba_of_list bytes
        with _ -> raise @@
          Example_failure (desc ^ ": decode failed", bytes)
      in
      if Bigarray.Array1.dim buf <> 0 then
        raise @@ Example_failure (desc ^ ": leftovers", bytes)
      else if not (check result a) then
        raise @@ Example_failure (desc ^ ": result mismatch", bytes)
      else ()

  and eq_big i bi = Big_int.(eq_big_int (big_int_of_int i) bi)

  let examples = [

    example "int" int [
      [0x02; 0x01; 0x2a], `v (`I 42);
      [0x02; 0x01; 0xff], `v (`I (-1));
      [0x02; 0x02; 0x00; 0xff], `v (`I 255);
    ];

    example "bool" bool [
      [0x01; 0x01; 0xff], `v true  ;
      [0x01; 0x01; 0x00], `v false ;
    ];

    example "null" null [
      [0x05; 0x00], `v ()
    ];

  ] *)

end

