
(* XXX BOILERPLATE *)

module OID = Prim.OID
type oid = OID.t

module Time = Asn_time


(* /XXX *)

exception Parse_error       = Core.Parse_error
exception End_of_input      = Core.End_of_input
exception Ambiguous_grammar = Core.Ambiguous_grammar

let parse_error reason = raise (Parse_error reason)

type 'a t        = 'a Core.asn
type 'a element  = 'a Core.element
type 'a sequence = 'a Core.sequence

include Combinators

type encoding = {
  mk_decoder : 'a. 'a t -> Cstruct.t -> 'a * Cstruct.t;
  mk_encoder : 'a. 'a t -> 'a -> Writer.t
}

let ber = {
  mk_decoder = Ber_der.R.parser ;
  mk_encoder = Ber_der.W.ber_to_writer ;
}

let der = {
  mk_decoder = Ber_der.R.parser ;
  mk_encoder = Ber_der.W.der_to_writer ;
}

type 'a codec =
  Codec of (Cstruct.t -> ('a * Cstruct.t)) * ('a -> Writer.t)

let codec { mk_encoder ; mk_decoder } asn =
  let () = validate asn in
  Codec (mk_decoder asn, mk_encoder asn)

let encode (Codec (_, enc)) a =
  Writer.to_cstruct (enc a)

let encode_into (Codec (_, enc)) a =
  Writer.to_writer (enc a)

and decode_exn (Codec (dec, _)) b = dec b

and decode (Codec (dec, _)) b =
  try Some (dec b) with
  ( Core.End_of_input | Core.Parse_error _ ) -> None


let random = Asn_random.r_asn
