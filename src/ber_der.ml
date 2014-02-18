
open Core
open Bytekit

type 'a endo = 'a -> 'a
let id x       = x
let const x _  = x
let comp f g x = f (g x)


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

module Seq = struct

  type 'r f = { f : 'a. 'a -> 'a asn -> 'r -> 'r }

  let rec fold_with_value : type a. 'r f -> 'r -> a -> a sequence -> 'r
  = fun f r a -> function
    | Last (Required (_, asn)) -> f.f a asn r
    | Last (Optional (_, asn)) ->
      ( match a with None -> r | Some a' -> f.f a' asn r )
    | Pair (Required (_, asn), asns) ->
        let (a1, a2) = a in f.f a1 asn (fold_with_value f r a2 asns)
    | Pair (Optional (_, asn), asns) ->
        let (a1, a2) = a in
        match a1 with
        | None     -> fold_with_value f r a2 asns
        | Some a1' -> f.f a1' asn (fold_with_value f r a2 asns)
end


module R = struct

  type coding =
    | Primitive of int
    | Constructed of int
    | Constructed_indefinite

  type header = { tag : tag ; coding : coding ; buf : bytes }

  type 'a parser = header -> 'a * bytes

  let parse_error ?header reason =
    let message = match header with
      | None   -> reason
      | Some h ->
          Printf.sprintf "%s (header: %s)"
            reason (string_of_tag h.tag) in
    raise (Parse_error message)

  let i_wanted asn = "expected: " ^ string_of_tags (tag_set asn)

  let field = function
    | None       -> "(unknown)"
    | Some label -> "'" ^ label ^ "'"

  let eof_error () = raise End_of_input

  let describe context prs header =
    try prs header with
    | Parse_error message ->
        parse_error ("in " ^ context ^ ": " ^ message)

(*
  let describe context prs header =
    try
      Dbg.note @@ context ^ " parsing " ^ string_of_tag header.tag;
      Dbg.enter context prs header
    with Parse_error message ->
      parse_error ("in " ^ context ^ ": " ^ message) *)

  let (>|=) prs f header =
    let (a, buf') = prs header in (f a, buf')

  let (>?=) prs (f, err_desc) header =
    let (a, buf') = prs header in
    match f a with
    | None    -> parse_error err_desc
    | Some a' -> (a', buf')


  module Partial = struct
    module C = Core

    exception Missing of string option

    type _ element =
      | Required : [`Nada of string option | `Found of 'a] -> 'a element
      | Optional : [`Nada of string option | `Found of 'a] -> 'a option element

    type _ sequence =
      | Last : 'a element -> 'a sequence
      | Pair : 'a element * 'b sequence -> ('a * 'b) sequence

    let rec of_complete : type a. a C.sequence -> a sequence = function
      | C.Last (C.Required (label, _)   ) -> Last (Required (`Nada label))
      | C.Last (C.Optional (label, _)   ) -> Last (Optional (`Nada label))
      | C.Pair (C.Required (label, _), t) -> Pair (Required (`Nada label), of_complete t)
      | C.Pair (C.Optional (label, _), t) -> Pair (Optional (`Nada label), of_complete t)

    let to_complete_exn =
      let rec f1 : type a. a element -> a = function
        | Required (`Nada label) -> raise (Missing label)
        | Required (`Found a   ) -> a
        | Optional (`Nada _    ) -> None
        | Optional (`Found a   ) -> Some a
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
      if i > last then (acc, i)
      else loop (acc * 0x100 + buf.{i}) (succ i) in
    loop 0 (succ off)


  let p_header_unsafe buffer =

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

    let tag = match t_class with
      | 0x00 -> Universal tagn
      | 0x40 -> Application tagn
      | 0x80 -> Context_specific tagn
      | 0xc0 -> Private tagn
      | _    -> assert false

    and coding = match (t_constructed, l0) with
      | (0, _   ) -> Primitive length
      | (_, 0x80) -> Constructed_indefinite
      | _         -> Constructed length

    and rest = Rd.drop contents_off buffer in

    { tag = tag ; coding = coding ; buf = rest }

  let p_header buffer =
    try p_header_unsafe buffer with
    | Invalid_argument _ -> parse_error "malformed header"


  let accepts : type a. a asn * header -> bool = function
    | (asn, { tag }) -> List.mem tag (tag_set asn)


  let with_header = fun f1 f2 -> function

    | { coding = Primitive n ; buf } ->
        (f1 n buf, Rd.drop n buf)

    | { coding = Constructed n ; buf } -> 
        let (b1, b2) = Rd.isolate n buf in
        let (a, b1') = f2 Rd.eof b1 in
        if Rd.eof b1' then (a, b2)
        else parse_error "definite constructed: leftovers"

    | { coding = Constructed_indefinite ; buf } ->
        let (a, buf') = f2 is_sequence_end buf in
        if is_sequence_end buf' then
          (a, drop_sequence_end buf')
        else parse_error "indefinite constructed: leftovers"

  let constructed f =
    with_header (fun _ _ -> parse_error "expected constructed") f

  let primitive f =
    with_header f (fun _ _ -> parse_error "expected primitive")

  let primitive_n n f = primitive @@ fun n' b ->
    if n = n' then f b
    else parse_error "primitive: invalid length"

  let sequence_of_parser prs =
    constructed @@ fun eof buf0 ->
      let rec scan acc buf =
        if eof buf then (List.rev acc, buf)
        else let (a, buf') = prs (p_header buf) in scan (a :: acc) buf' in
      scan [] buf0

  let string_like (type a) ?sz impl =
    let module P = (val impl : Prim.String_primitive with type t = a) in

    let rec prs = function
      | { coding = Primitive n ; buf } -> (P.of_bytes n buf, Rd.drop n buf)
      | h -> ( sequence_of_parser prs >|= P.concat ) h in

    match sz with
    | None   -> prs
    | Some s ->
        let ck a =
          if P.length a = s then a else
            parse_error "bad size; constrained by grammar"
        in prs >|= ck


  let parser_of_prim : type a. a prim -> a parser = function

    | Bool      -> primitive_n 1 @@ fun buf -> buf.{0} <> 0x00

    | Int       -> primitive Prim.Integer.of_bytes

    | Bits      -> string_like (module Prim.Bits)

    | Octets sz -> string_like ?sz (module Prim.Octets)

    | Null      -> primitive_n 0 @@ fun _ -> ()

    | OID       -> primitive Prim.OID.of_bytes

    | UTCTime   ->
        Prim.Time.(string_like (module Str) >?=
                     (utc_time_of_string, "malformed UTCTime"))
    | GeneralizedTime ->
        Prim.Time.(string_like (module Str) >?=
                     (gen_time_of_string, "malformed GeneralizedTime"))

    | UTF8String      -> string_like (module Prim.Gen_string)
    | NumericString   -> string_like (module Prim.Gen_string)
    | PrintableString -> string_like (module Prim.Gen_string)
    | TeletexString   -> string_like (module Prim.Gen_string)
    | VideotexString  -> string_like (module Prim.Gen_string)
    | IA5String       -> string_like (module Prim.Gen_string)
    | GraphicString   -> string_like (module Prim.Gen_string)
    | VisibleString   -> string_like (module Prim.Gen_string)
    | GeneralString   -> string_like (module Prim.Gen_string)
    | UniversalString -> string_like (module Prim.Gen_string)
    | BMPString       -> string_like (module Prim.Gen_string)


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

        let rec elt : type a. a element -> header -> a S.touch = function

          | Required (label, asn) ->
              let prs = parser_of_asn asn in
              describe (field label) @@ fun header ->
                if accepts (asn, header) then
                  let (a, buf) = prs header in Hit (a, buf)
                else parse_error ~header (i_wanted asn)

          | Optional (label, asn) ->
              let prs = parser_of_asn asn in
              describe (field label) @@ fun header ->
                if accepts (asn, header) then
                  let (a, buf) = prs header in Hit (Some a, buf)
                else Pass None

        and seq : type a. a sequence -> (bytes -> bool) -> a parser = function

          | Last e ->
              let prs = elt e in fun _ header ->
              ( match prs header with
                | Pass a -> parse_error ~header "not all input consumed"
                | Hit (a, buf') -> (a, buf') )

          | Pair (e, tl) ->
              let (prs1, prs2) = elt e, seq tl in fun eof header ->
              ( match prs1 header with
                | Hit (a, buf) when eof buf ->
                    ((a, default_or_error tl), buf)
                | Hit (a, buf) ->
                    let (b, buf') = prs2 eof (p_header buf) in ((a, b), buf')
                | Pass a ->
                    let (b, buf') = prs2 eof header in ((a, b), buf') )

        and default_or_error : type a. a sequence -> a = function
          | Last (Required (label, _)   ) -> missing label
          | Pair (Required (label, _), _) -> missing label
          | Last (Optional (_    , _)   ) -> None
          | Pair (Optional (_    , _), s) -> None, default_or_error s

        and missing : type a. string option -> a =
          fun label -> parse_error ("missing " ^ field label)
            
        in
        let prs = seq asns in
        describe "sequence" @@
          constructed @@ fun eof buf ->
            if eof buf then (default_or_error asns, buf)
            else prs eof (p_header buf)

    | Set asns ->

        let module P = Partial in
        let module TM = RichMap ( struct
          type t = tag let compare = compare
        end ) in

        let rec partial_e : type a. a element -> tags * a P.element parser
          = function
          | Required (label, asn) ->
              ( tag_set asn, 
                describe (field label) (parser_of_asn asn)
                >|= fun a -> P.Required (`Found a) )
          | Optional (label, asn) ->
              ( tag_set asn, 
                describe (field label) (parser_of_asn asn)
                >|= fun a -> P.Optional (`Found a) )

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

        describe "set" @@
          constructed @@ fun eof buf0 ->
            let rec scan partial buf =
              if eof buf then
                try ( P.to_complete_exn partial, buf ) with
                | P.Missing label -> parse_error ("missing " ^ field label)
              else
                let header = p_header buf in
                let prs =
                  try TM.find header.tag parsers with
                  | Not_found -> parse_error ~header "unexpected field" in
                let (f, buf') = prs header in
                scan (f partial) buf'
            in
            scan zero buf0

    | Sequence_of asn ->
        describe "sequence_of" @@
          sequence_of_parser @@ parser_of_asn asn 

    | Set_of asn ->
        describe "set_of" @@
          sequence_of_parser @@ parser_of_asn asn

    | Choice (asn1, asn2) ->

        let (prs1, prs2) = (parser_of_asn asn1, parser_of_asn asn2) in
        describe "choice" @@
          fun header ->
            if accepts (asn1, header) then
              let (a, buf') = prs1 header in (L a, buf')
            else
              let (b, buf') = prs2 header in (R b, buf')

    | Implicit (_, asn) -> parser_of_asn asn

    | Explicit (_, asn) ->
        describe "explicit" @@
          constructed @@ const (comp (parser_of_asn asn) p_header)

    | Prim p ->
        describe "primitive" @@
          parser_of_prim p


  let parser : 'a asn -> bytes -> 'a * bytes
  = fun asn ->
    let prs = parser_of_asn asn in
    fun buf0 ->
      let header = p_header buf0 in
      if accepts (asn, header) then prs header
      else parse_error ~header "unexpected header"

end

module W = struct

  let (<>) = Wr.(<>)

  let e_big_tag tag =
    let cons x = function [] -> [x] | xs -> (x lor 0x80)::xs in
    let rec loop acc = function
      | 0 -> acc
      | n -> loop (cons (n land 0x7f) acc) (n lsr 7) in
    loop [] tag

  let e_big_length length =
    let rec loop acc = function
      | 0 -> acc
      | n -> loop (n land 0xff :: acc) (n lsr 8) in
    loop [] length

  let e_header tag mode len =

    let (klass, tagn) = match tag with
      | Universal n        -> (0x00, n)
      | Application n      -> (0x40, n)
      | Context_specific n -> (0x80, n)
      | Private n          -> (0xc0, n) in

    let constructed = match mode with
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


  type conf = { der : bool }

  let (@?) o def =
    match o with | None -> def | Some x -> x

  let e_constructed tag body =
    e_header tag `Constructed (Wr.size body) <> body

  let e_primitive tag body =
    e_header tag `Primitive (Wr.size body) <> body

  let assert_length constr len_f a =
    match constr with
    | None   -> ()
    | Some s ->
        let len = len_f a in
        if len = s then () else
          invalid_arg @@
          Printf.sprintf "bad length: constraint: %d actual: %d" s len

  let rec encode : type a. conf -> tag option -> a -> a asn -> Wr.t
  = fun conf tag a -> function

    | Iso (_, g, asn) -> encode conf tag (g a) asn

    | Fix asn as fix -> encode conf tag a (asn fix)

    | Sequence asns ->
        e_constructed (tag @? sequence_tag) (e_seq conf a asns)

    | Sequence_of asn -> (* size/stack? *)
        e_constructed (tag @? sequence_tag) @@
          Wr.concat (List.map (fun e -> encode conf None e asn) a)

    | Set asns ->
        let h_sorted conf a asns =
          let fn = { Seq.f = fun a asn xs ->
                      ( Core.tag a asn, encode conf None a asn ) :: xs } in
          Wr.concat @@
            List.( map snd @@
              sort (fun (t1, _) (t2, _) -> compare t1 t2) @@
                Seq.fold_with_value fn [] a asns )
        in
        e_constructed (tag @? set_tag) @@
          if conf.der then h_sorted conf a asns else e_seq conf a asns

    | Set_of asn ->
        let ws = List.map (fun e -> encode conf None e asn) a in
        let body =
          Wr.concat @@
            if conf.der then
              List.(map Wr.bytes @@ sort compare_b @@ map Wr.to_bytes ws)
            else ws
        in
        e_constructed (tag @? set_tag) body

    | Choice (asn1, asn2) ->
      ( match a with
        | L a' -> encode conf tag a' asn1
        | R b' -> encode conf tag b' asn2 )

    | Implicit (t, asn) ->
        encode conf (Some (tag @? t)) a asn

    | Explicit (t, asn) ->
        e_constructed (tag @? t) (encode conf None a asn)

    | Prim p -> e_prim tag a p

  and e_seq : type a. conf -> a -> a sequence -> Wr.t = fun conf ->
    let f = { Seq.f = fun e asn w -> encode conf None e asn <> w } in
    Seq.fold_with_value f Wr.empty

  and e_prim : type a. tag option -> a -> a prim -> Wr.t
  = fun tag a prim ->

    let encode =
      e_primitive
        (match tag with Some x -> x | None -> tag_of_p prim) in

    let encode_s (type a) ?size a impl =
      let module P = (val impl : Prim.String_primitive with type t = a) in
      assert_length size P.length a;
      encode (P.to_bytes a) in

    match prim with
    | Bool      -> encode @@ Wr.byte (if a then 0xff else 0x00)

    | Int       -> encode @@ Prim.Integer.to_bytes a

    | Bits      -> encode @@ Prim.Bits.to_bytes a

    | Octets s  -> encode_s a (module Prim.Octets)

    | Null      -> encode Wr.empty

    | OID       -> encode @@ Prim.OID.to_bytes a

    | UTCTime ->
        encode Prim.Time.(Str.to_bytes @@ utc_time_to_string a)

    | GeneralizedTime ->
        encode Prim.Time.(Str.to_bytes @@ gen_time_to_string a)

    | UTF8String      -> encode @@ Prim.Gen_string.to_bytes a
    | NumericString   -> encode @@ Prim.Gen_string.to_bytes a
    | PrintableString -> encode @@ Prim.Gen_string.to_bytes a
    | TeletexString   -> encode @@ Prim.Gen_string.to_bytes a
    | VideotexString  -> encode @@ Prim.Gen_string.to_bytes a
    | IA5String       -> encode @@ Prim.Gen_string.to_bytes a
    | GraphicString   -> encode @@ Prim.Gen_string.to_bytes a
    | VisibleString   -> encode @@ Prim.Gen_string.to_bytes a
    | GeneralString   -> encode @@ Prim.Gen_string.to_bytes a
    | UniversalString -> encode @@ Prim.Gen_string.to_bytes a
    | BMPString       -> encode @@ Prim.Gen_string.to_bytes a


  let encode_ber_to_bytes asn a =
    Wr.to_bytes @@ encode { der = false } None a asn

  let encode_der_to_bytes asn a =
    Wr.to_bytes @@ encode { der = true } None a asn

end

