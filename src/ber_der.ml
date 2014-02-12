open Common
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


  let halt ?why () =
    ( match why with
      | Some str -> Printf.printf "HALT: %s\n%!" str
      | None     -> Printf.printf "HALT.\n%!"  );
    raise Invalid_encoding


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
        | Required  `Nada     -> halt ()
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


  let accepts : type a. a asn * header -> bool = function
    | (asn, { tag }) -> List.mem tag (tagset asn)


  let with_header = fun f1 f2 -> function

    | { coding = Primitive n ; buf } ->
        (f1 n buf, Rd.drop n buf)

    | { coding = Constructed n ; buf } -> 
        let (b1, b2) = Rd.isolate n buf in
        let (a, b1') = f2 Rd.eof b1 in
        if Rd.eof b1' then (a, b2) else halt ()

    | { coding = Constructed_indefinite ; buf } ->
        let (a, buf') = f2 is_sequence_end buf in
        if is_sequence_end buf' then
          (a, drop_sequence_end buf')
        else halt ()

  let constructed f = with_header (fun _ _ -> halt ()) f

  let primitive f = with_header f (fun _ _ -> halt ())

  let primitive_n n f = primitive @@ fun n' b ->
    if n = n' then f b else halt ()


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
      match mtag with Some x -> x | None -> tag_of_p prim in
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

