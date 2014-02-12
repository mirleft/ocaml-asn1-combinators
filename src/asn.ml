
type 'a t        = 'a Core.asn
type 'a element  = 'a Core.element
type 'a sequence = 'a Core.sequence

include Combinators

module B = Bytekit

type encoding = {
  mk_decoder : 'a. 'a t -> B.bytes -> 'a * B.bytes;
  mk_encoder : 'a. 'a t -> 'a -> B.bytes
}

let ber_der = {
  mk_encoder = Ber_der.W.encode_to_bytes ;
  mk_decoder = Ber_der.R.parser
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

