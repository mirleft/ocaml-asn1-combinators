
module OID  = Asn_oid
module Time = Asn_time

exception Parse_error       = Asn_core.Parse_error
exception Ambiguous_grammar = Asn_core.Ambiguous_grammar

type 'a t        = 'a Asn_core.asn
type 'a element  = 'a Asn_core.element
type 'a sequence = 'a Asn_core.sequence

include Asn_combinators

type encoding = {
  mk_decoder : 'a. 'a t -> Cstruct.t -> 'a * Cstruct.t;
  mk_encoder : 'a. 'a t -> 'a -> Asn_writer.t
}

let ber = {
  mk_decoder = Asn_ber_der.R.parser ;
  mk_encoder = Asn_ber_der.W.ber_to_writer ;
}

let der = {
  mk_decoder = Asn_ber_der.R.parser ;
  mk_encoder = Asn_ber_der.W.der_to_writer ;
}

type 'a codec =
  Codec of (Cstruct.t -> ('a * Cstruct.t)) * ('a -> Asn_writer.t)

let codec { mk_encoder ; mk_decoder } asn =
  let () = validate asn in
  Codec (mk_decoder asn, mk_encoder asn)

let encode (Codec (_, enc)) a =
  Asn_writer.to_cstruct (enc a)

let encode_into (Codec (_, enc)) a =
  Asn_writer.to_writer (enc a)

and decode_exn (Codec (dec, _)) b = dec b

and decode (Codec (dec, _)) b =
  try Some (dec b) with End_of_file | Parse_error _ -> None


let random = Asn_random.r_asn
