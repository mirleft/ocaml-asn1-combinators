
module OID = Asn_oid

let id x      = x
let const x _ = x
let (&.) f g x = f (g x)

type 'a endo = 'a -> 'a

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

  let equal t1 t2 =
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

type bits_t = int * Cstruct.t


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

  | Bool       : bool      prim
  | Int        : Z.t       prim
  | Bits       : bits_t    prim
  | Octets     : Cstruct.t prim
  | Null       : unit      prim
  | OID        : OID.t     prim
  | CharString : string    prim


let seq_tag = Tag.Universal 0x10
and set_tag = Tag.Universal 0x11

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

  | Sequence    _ -> [ seq_tag ]
  | Sequence_of _ -> [ seq_tag ]
  | Set _         -> [ set_tag ]
  | Set_of _      -> [ set_tag ]
  | Choice (asn1, asn2) -> tag_set asn1 @ tag_set asn2

  | Implicit (t, _) -> [ t ]
  | Explicit (t, _) -> [ t ]

  | Prim p -> [ tag_of_p p ]

let rec tag : type a. a -> a asn -> tag = fun a -> function

  | Iso (_, g, _, asn) -> tag (g a) asn
  | Fix _ as fix       -> tag a fix
  | Sequence _         -> seq_tag
  | Sequence_of _      -> seq_tag
  | Set _              -> set_tag
  | Set_of _           -> set_tag
  | Choice (a1, a2)    -> (match a with L a' -> tag a' a1 | R b' -> tag b' a2)
  | Implicit (t, _)    -> t
  | Explicit (t, _)    -> t
  | Prim p             -> tag_of_p p


(* Check tag ambiguity.
 * XXX: Would be _epic_ to move this to the type-checker.
 *)

module FSet = struct
  type f = Fn : ('a -> 'b) -> f
  include Set.Make ( struct
    type t = f
    let compare (Fn f1) (Fn f2) = Hashtbl.(compare (hash f1) (hash f2))
  end )
  let mem f s = mem (Fn f) s
  and add f s = add (Fn f) s
end

let validate asn =

  let rec check : type a. ?tag:tag -> FSet.t -> a asn -> unit =
    fun ?tag fs -> function
    | Iso (_, _, _, a)         -> check ?tag fs a
    | Fix f when FSet.mem f fs -> ()
    | Fix f as fix             -> check ?tag FSet.(add f fs) (f fix)

    | Sequence s    -> disjoint_seq s ; check_s fs s
    | Set s         -> disjoint (seq_tags s) ; check_s fs s
    | Sequence_of a -> check fs a
    | Set_of      a -> check fs a

(*     | Choice (a1, a2) when tag <> None -> raise Ambiguous_grammar *)
    | Choice (a1, a2) ->
        disjoint [tag_set a1; tag_set a2] ; check fs a1 ; check fs a2

    | Implicit (t, a) -> check ~tag:t fs a
    | Explicit (_, a) -> check fs a
    | Prim _          -> ()

  and check_s : type a. FSet.t -> a sequence -> unit = fun fs -> function
    | Last (Required (_, a))    -> check fs a
    | Last (Optional (_, a))    -> check fs a
    | Pair (Required (_, a), s) -> check fs a ; check_s fs s
    | Pair (Optional (_, a), s) -> check fs a ; check_s fs s

  and seq_tags : type a. a sequence -> tags list = function
    | Last (Required (_, a))    -> [tag_set a]
    | Last (Optional (_, a))    -> [tag_set a]
    | Pair (Required (_, a), s) -> tag_set a :: seq_tags s
    | Pair (Optional (_, a), s) -> tag_set a :: seq_tags s

  and disjoint_seq : type a. a sequence -> unit = fun s ->
    let f1 : type a. tags list -> a element -> tags list = fun tss -> function
      | Required (_, a) -> disjoint (tag_set a :: tss) ; []
      | Optional (_, a) -> disjoint (tag_set a :: tss) ; tag_set a :: tss in
    let rec f2 : type a. tags list -> a sequence -> unit = fun tss -> function
      | Last e      -> ignore (f1 tss e)
      | Pair (e, s) -> f2 (f1 tss e) s in
    f2 [] s

  and disjoint tss =
    let rec go = function
      | t::(u::_ as ts) ->
          if Tag.equal t u then raise Ambiguous_grammar else go ts
      | _ -> () in
    go List.(sort Tag.compare @@ concat tss) in

  check FSet.empty asn
