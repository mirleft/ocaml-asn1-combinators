
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

