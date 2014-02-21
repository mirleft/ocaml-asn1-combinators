
open Prim
open Core

let replicate n f =
  let rec loop acc n =
    if n <= 0 then acc else
      loop (f () :: acc) (pred n) in
  loop [] n

let r_prim : type a. a Core.prim -> a = function

  | Bool            -> Random.bool ()
  | Int             -> Integer.random ()
  | Bits            -> Bits.random ()
  | Octets          -> Octets.random ()
  | Null            -> ()
  | OID             -> OID.random ()
  | UTCTime         -> Time.random ()
  | GeneralizedTime -> Time.random ~fraction:true ()

  | UTF8String      -> Gen_string.random ()
  | NumericString   -> Gen_string.random ()
  | PrintableString -> Gen_string.random ()
  | TeletexString   -> Gen_string.random ()
  | VideotexString  -> Gen_string.random ()
  | IA5String       -> Gen_string.random ()
  | GraphicString   -> Gen_string.random ()
  | VisibleString   -> Gen_string.random ()
  | GeneralString   -> Gen_string.random ()
  | UniversalString -> Gen_string.random ()
  | BMPString       -> Gen_string.random ()

let rec r_element : type a. a element -> a = function

  | Required (_, asn) -> r_asn asn
  | Optional (_, asn) ->
      if Random.int 3 = 0 then None
      else Some (r_asn asn)

and r_seq : type a. a sequence -> a = function

  | Last e       -> r_element e
  | Pair (e, es) -> (r_element e, r_seq es)

and r_seq_of : type a. a asn -> a list = fun asn ->

  replicate (Random.int 10) (fun () -> r_asn asn)

and r_asn : type a. a asn -> a = function

  | Iso (f, _, asn) -> f @@ r_asn asn
  | Fix f as fix    -> r_asn (f fix)

  | Sequence asns   -> r_seq asns
  | Set      asns   -> r_seq asns
  | Sequence_of asn -> r_seq_of asn
  | Set_of      asn -> r_seq_of asn

  | Choice (asn1, asn2) ->
      if Random.bool () then L (r_asn asn1) else R (r_asn asn2)

  | Implicit (_, asn) -> r_asn asn
  | Explicit (_, asn) -> r_asn asn

  | Prim p -> r_prim p
