(* Copyright (c) 2014-2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

module Core = Asn_core
module OID  = Asn_oid
module Time = Asn_time

exception Parse_error       = Core.Parse_error
exception Ambiguous_grammar = Core.Ambiguous_grammar

type 'a t        = 'a Core.asn
type 'a element  = 'a Core.element
type 'a sequence = 'a Core.sequence

include Asn_combinators

type encoding = {
  mk_decoder : 'a. 'a t -> Cstruct.t -> 'a * Cstruct.t;
  mk_encoder : 'a. 'a t -> 'a -> Asn_writer.t
}

let ber = {
  mk_decoder = Asn_ber_der.R.compile_ber ;
  mk_encoder = Asn_ber_der.W.ber_to_writer ;
}

let der = {
  mk_decoder = Asn_ber_der.R.compile_der ;
  mk_encoder = Asn_ber_der.W.der_to_writer ;
}

type 'a codec =
  Codec of (Cstruct.t -> ('a * Cstruct.t)) * ('a -> Asn_writer.t)

let codec { mk_encoder ; mk_decoder } asn =
  let () = Core.validate asn in
  Codec (mk_decoder asn, mk_encoder asn)

let encode (Codec (_, enc)) a =
  Asn_writer.to_cstruct (enc a)

let encode_into (Codec (_, enc)) a =
  Asn_writer.to_writer (enc a)

and decode_exn (Codec (dec, _)) b = dec b

and decode (Codec (dec, _)) b =
  try Some (dec b) with End_of_file | Parse_error _ -> None

(* and decode (Codec (dec, _)) b = *)
(*   try Ok (dec b) with *)
(*   | End_of_file   -> Error "EOF" *)
(*   | Parse_error e -> Error e *)

let random = Asn_random.r_asn
