
open Asn_prim

let id x      = x
let const x _ = x
let o f g x   = f (g x)

type ('a, 'b) sum = L of 'a | R of 'b

module Tag = struct

  type t =
    | Universal        of int
    | Application      of int
    | Context_specific of int
    | Private          of int

  (* Specialized compare and an inlined eq give a significant speed boost in
   * BER/DER. *)

  let compare t1 t2 =
    match (t1, t2) with
    | (Universal        a, Universal        b)
    | (Application      a, Application      b)
    | (Context_specific a, Context_specific b)
    | (Private          a, Private          b) -> compare a b
    | (Universal        _, _)
    | (Application      _, (Context_specific _ | Private _))
    | (Context_specific _, Private _) -> -1
    | _ -> 1

  let eq t1 t2 =
    match (t1, t2) with
    | (Universal        a, Universal        b)
    | (Application      a, Application      b)
    | (Context_specific a, Context_specific b)
    | (Private          a, Private          b) -> a = b
    | _ -> false

  let to_string tag =
    let p = Printf.sprintf "(%s %d)" in
    match tag with
    | Universal n        -> p "univ" n
    | Application n      -> p "app" n
    | Context_specific n -> p "context" n
    | Private n          -> p "private" n

  let set_to_string tags =
    "(" ^ (String.concat " " @@ List.map to_string tags) ^ ")"

end

type tag  = Tag.t
type tags = Tag.t list

exception Ambiguous_grammar
exception Parse_error of string

type 'a rand = unit -> 'a

type _ asn =

  | Iso : ('a -> 'b) * ('b -> 'a) * 'b rand option * 'a asn -> 'b asn
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

  | Required : string option * 'a asn -> 'a element
  | Optional : string option * 'a asn -> 'a option element

and _ sequence =

  | Last : 'a element -> 'a sequence
  | Pair : 'a element * 'b sequence -> ('a * 'b) sequence

and _ prim =

  | Bool       : bool         prim
  | Int        : Integer.t    prim
  | Bits       : Bits.t       prim
  | Octets     : Octets.t     prim
  | Null       : unit         prim
  | OID        : OID.t        prim
  | CharString : Gen_string.t prim


let sequence_tag = Tag.Universal 0x10
and set_tag      = Tag.Universal 0x11

let tag_of_p : type a. a prim -> tag =
  let open Tag in function
  | Bool       -> Universal 0x01
  | Int        -> Universal 0x02
  | Bits       -> Universal 0x03
  | Octets     -> Universal 0x04
  | Null       -> Universal 0x05
  | OID        -> Universal 0x06
  | CharString -> Universal 0x1d


let rec tag_set : type a. a asn -> tags = function

  | Iso (_, _, _, asn) -> tag_set asn
  | Fix f as fix       -> tag_set (f fix)

  | Sequence    _ -> [ sequence_tag ]
  | Sequence_of _ -> [ sequence_tag ]
  | Set _         -> [ set_tag ]
  | Set_of _      -> [ set_tag ]
  | Choice (asn1, asn2) -> tag_set asn1 @ tag_set asn2

  | Implicit (t, _) -> [ t ]
  | Explicit (t, _) -> [ t ]

  | Prim p -> [ tag_of_p p ]

let rec tag : type a. a -> a asn -> tag = fun a -> function

  | Iso (_, g, _, asn) -> tag (g a) asn
  | Fix _ as fix       -> tag a fix
  | Sequence _         -> sequence_tag
  | Sequence_of _      -> sequence_tag
  | Set _              -> set_tag
  | Set_of _           -> set_tag
  | Choice (a1, a2)    -> (match a with L a' -> tag a' a1 | R b' -> tag b' a2)
  | Implicit (t, _)    -> t
  | Explicit (t, _)    -> t
  | Prim p             -> tag_of_p p

