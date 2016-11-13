(* Copyright (c) 2014-2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** Embed typed ASN.1 grammars in OCaml

    Skip the notation part of Abstract Syntax Notation, and embed the abstract
    syntax directly in OCaml.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

open Result

(** ASN.1 [OBJECT IDENTIFIER].

    Magic numbers in a suit and tie. Their consulting fee is astronomical. *)
module OID : sig

  (** Object identifier. Every OID has at least two components. *)
  type t = private Oid of int * int * int list

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val seeded_hash : int -> t -> int

  (** {2 Construction} *)

  val base : int -> int -> t
  (** [base v1 v2] is the OID [v1.v2].

      Either [v1] is [[0..1]] and [v2] is [[0..39]] (inclusive), or [v1] is [2]
      and [v2] is non-negative.

      @raise Invalid_argument if the components are out of range. *)

  val (<|) : t -> int -> t
  (** [oid <| n] is the OID [oid.n].

      @raise Invalid_argument if [n] is negative. *)

  val (<||) : t -> int list -> t
  (** [oid <|| ns] is the old [oid.n1.n2. ...] if [ns] is [[n1; n2; ...]].

      @raise Invalid_argument if any of [ns] is negative. *)

  (** {2 Conversion} *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf oid] pretty-prints [oid] on [ppf] as dotted-decimal. *)

  val to_list : t -> int list
  (** [to_list oid] is the list [n1; n2; ...] if the OID is [n1.n2. ...]. *)

  val of_string : string -> t
  (** [of_string s] is the OID represented by [s].

      @raise Invalid_argument if [s] is not dotted-decimal, or the components
      are out of range. *)
end

(** [Time] needs to go. *)
module Time : sig

  type t = {
    date : (int * int * int) ;
    time : (int * int * int * float) ;
    tz   : (int * int * [ `W | `E ]) option ;
  }

  val to_posix_time : t -> float

  val date_to_posix_time :
    y:int -> m:int -> d:int ->
    hh:int -> mm:int -> ss:int ->
    ff:float -> tz_mm:int -> float
  (** [date_to_posix_time ~y ~m ~d ~hh ~mm ~ss ~ff ~tz_mm] is the POSIX
      time corresponding to the calendar date [y-m-d] at time [hh:ss:mm.ff]
      with time zone offset [tz_mm] in minutes.

      {b Warning.} Does not check ranges or that [y-m-d] is a valid calendar
      date. *)
end

(** ASN.1 Abstract Syntax.

    This module is the OCaml term-level analogue of ASN.1's surface notation.

    It provides a ground type {{!Sx.t}['a t]} representing typed abstract syntax,
    a suite of primitives that correspond to ASN.1 primitives, and a suite of
    combinators that correspond to the combining structures of ASN.1.

    ASN.1 naming and modules are provided by the host language instead. *)
module S : sig

  type 'a t
  (** ASN.1 abstract syntax of values of type ['a]. *)

  (** {2 Basic combinators} *)

  val fix : ('a t -> 'a t) -> 'a t
  (** [fix fasn] is the fixpoint, allowing [fasn] to construct a syntax that
      refers to itself. *)

  val map : ?random:(unit -> 'b) -> ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t
  (** [map ?random f g asn] creates a derived syntax that encodes and decodes
      like [asn], but uses [f] to project and [g] to inject. *)

  (** {2 Tags} *)

  type cls = [ `Universal | `Application | `Private ]
  (** ASN.1 tag CLASS. *)

  val implicit : ?cls:cls -> int -> 'a t -> 'a t
  (** [implicit ?cls n asn] is the ASN.1 [IMPLICIT] construct, changing the tag
      of [asn] to [(cls, n)].

      [n] is the tag value.

      [~cls] is the class. Defaults to [CONTEXT SPECIFIC]. *)

  val explicit : ?cls:cls -> int -> 'a t -> 'a t
  (** [explicit ?cls n asn] is the ASN.1 [EXPLICIT] construct, changing the tag
      of [asn] to [(cls, n)].

      [n] is the tag value.

      [~cls] is the class. Defaults to [CONTEXT SPECIFIC]. *)

  (** {2 Combining constructs}

      These look like
{[sequence @@
    (required ~label:"l1" asn1)
  @ (optional ~label:"l2" asn2)
  @ (required ~label:"l3" asn3)
 -@ (optional ~label:"l4" asn4)]}
      or
{[choice3 asn1 asn2 asn3]} *)

  type 'a element
  (** An [element] is a single slot in a {{!sequence}[sequence]}. *)

  val required : ?label:string -> 'a t -> 'a element
  (** [required ?label asn] is a regular {{!sequence}[sequence]} element.

      [~label] is the name of the element. *)

  val optional : ?label:string -> 'a t -> 'a option element
  (** [optional ?label asn] is a {{!sequence}[sequence]} element marked with the
      ASN.1 [OPTIONAL] keyword.

      [~label] is the name of the element. *)

  type 'a sequence
  (** A [sequence] is the body of a multi-field ASN.1 construct, like
     [SEQUENCE] and [SET]. *)

  val single : 'a element -> 'a sequence
  (** [single e] is the singleton sequence containing just [e]. *)

  val ( @ ) : 'a element -> 'b sequence -> ('a * 'b) sequence
  (** [e @ seq] extends [seq] by prepending [e]. *)

  val ( -@ ) : 'a element -> 'b element  -> ('a * 'b) sequence
  (** [e -@ e1] is [e @ single e1] *)

  val sequence : 'a sequence -> 'a t
  (** [sequence seq] is the ASN.1 [SEQUENCE] construct, with the body [seq]. *)

  val sequence_of : 'a t -> 'a list t
  (** [sequence_of] is the ASN.1 [SEQUENCE OF] construct. *)

  val sequence2 : 'a element -> 'b element -> ('a * 'b) t
  (** [sequence2 e1 e2] is [sequence (e1 -@ e2)]. *)

  val sequence3 :
    'a element ->
    'b element -> 'c element -> ('a * 'b * 'c) t

  val sequence4 :
    'a element ->
    'b element ->
    'c element -> 'd element -> ('a * 'b * 'c * 'd) t

  val sequence5 :
    'a element ->
    'b element ->
    'c element ->
    'd element -> 'e element -> ('a * 'b * 'c * 'd * 'e) t

  val sequence6 :
    'a element ->
    'b element ->
    'c element ->
    'd element ->
    'e element ->
    'f element -> ('a * 'b * 'c * 'd * 'e * 'f) t

  val set : 'a sequence -> 'a t
  (** [seq seq] is the ASN.1 [SET] construct, with the body [seq]. *)

  val set_of : 'a t -> 'a list t
  (** [set_of asn] is the ASN.1 [SET OF] construct. *)

  val set2 : 'a element -> 'b element -> ('a * 'b) t
  (** [set2 e1 e2] is [set (e1 -@ e2)]. *)

  val set3 :
    'a element ->
    'b element -> 'c element -> ('a * 'b * 'c) t

  val set4 :
    'a element ->
    'b element ->
    'c element -> 'd element -> ('a * 'b * 'c * 'd) t

  val set5 :
    'a element ->
    'b element ->
    'c element ->
    'd element -> 'e element -> ('a * 'b * 'c * 'd * 'e) t

  val set6 :
    'a element ->
    'b element ->
    'c element ->
    'd element ->
    'e element ->
    'f element -> ('a * 'b * 'c * 'd * 'e * 'f) t

  val choice2 :
    'a t -> 'b t -> [ `C1 of 'a | `C2 of 'b ] t
  (** [choice2 asn1 asn2] is the ASN.1 [CHOICE] construct, choosing between
      [asn1] and [asn2].

      Larger [CHOICE] can be obtained by nesting [choiceN] variants.

      {b Note} [CHOICE] containing elements with the same tag yields an illegal
      syntax. This will be detected by {!codec}. *)

  val choice3 :
    'a t -> 'b t -> 'c t
    -> [ `C1 of 'a | `C2 of 'b | `C3 of 'c ] t

  val choice4 :
    'a t -> 'b t -> 'c t -> 'd t
    -> [ `C1 of 'a | `C2 of 'b | `C3 of 'c | `C4 of 'd ] t

  val choice5 :
    'a t -> 'b t -> 'c t -> 'd t -> 'e t
    -> [ `C1 of 'a | `C2 of 'b | `C3 of 'c | `C4 of 'd | `C5 of 'e ] t

  val choice6 :
    'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
    -> [ `C1 of 'a | `C2 of 'b | `C3 of 'c | `C4 of 'd | `C5 of 'e | `C6 of 'f ] t

  (** {2 Primitives} *)

  val bool : bool t
  (** [bool] is ASN.1 [BOOLEAN]. *)

  val integer : Z.t t
  (** [integer] is ASN.1 [INTEGER]. *)

  val bit_string : bool array t
  (** [bit_string] is ASN.1 [BIT STRING]. *)

  val bit_string_cs : Cstruct.t t
  (** [bit_string_cs] is ASN.1 [BIT STRING], represented as {!Cstruct.t}. *)

  val octet_string : Cstruct.t  t
  (** [octet_string] is ASN.1 [OCTET STRING]. *)

  val null : unit t
  (** [null] is ASN.1 [NULL]. *)

  val oid : OID.t t
  (** [oid] is ASN.1 [OBJECT IDENTIFIER]. *)

  val utc_time : Time.t t
  (** [utc_time] is ASN.1 [UTCTime]. *)

  val generalized_time : Time.t     t
  (** [generalized_time] is ASN.1 [GeneralizedTime]. *)

  (** {2 String primitives}

      Various ASN.1 stringy types.

      {b Note} Presently, no conversion or validation is performed on strings.
      They differ only in tags. *)

  val utf8_string      : string t
  val numeric_string   : string t
  val printable_string : string t
  val teletex_string   : string t
  val videotex_string  : string t
  val ia5_string       : string t
  val graphic_string   : string t
  val visible_string   : string t
  val general_string   : string t
  val universal_string : string t
  val bmp_string       : string t

  (** {2 Derived primitives} *)

  val int : int t
  (** [int] is ASN.1 [INTEGER], projected into an OCaml [int]. *)

  val bit_string_flags : (int * 'a) list -> 'a list t
  (** [bit_string_flags xs] is ASN.1 [BIT STRING], represented as a collection
      of values.

      [xs] is a list of [(bit, x)], where bit [bit] denotes the presence of [x]. *)

  (** {2 Errors} *)

  (* XXX repeats *)
  val error : [ `Parse of string ] -> 'a
  (** [error err] aborts parsing with the {{!error}[error]} [err].

      Aborting the parse is useful, for example, in the [f] argument to
      {{!map}[map]}. *)

  val parse_error : ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** [parse_error fmt ...] aborts parsing with the message produced by using
      [fmt] to format arguments [...]. *)
end

type 'a t = 'a S.t
(** Abstract syntax of values of type ['a]. See {!S.t}. *)

(** {2 Encodings} *)

type encoding

val ber : encoding
(** [ber] is ASN.1 Basic Encoding Rules (BER). *)

val der : encoding
(** [der] is ASN.1 Distinguished Encoding Rules (DER). *)


(** {2 Encoding and decoding} *)

type 'a codec

exception Ambiguous_syntax

val codec : encoding -> 'a t -> 'a codec
(** [codec enc asn] represents the syntax [asn] encoded under the rules [enc].

    This function performs work up-front, and is generally expected to be called
    in the static context on statically known syntaxes.

    @raise Ambiguous_syntax if [asn] contains [CHOICE] constructs over
    sub-syntaxes with the same tags. *)

val encode : 'a codec -> 'a -> Cstruct.t
(** [encode codec x] is the encoding of [x], using [codec]. *)

val encode_into : 'a codec -> 'a -> (int * (Cstruct.t -> unit))
(** [encode_into codec x] is the pair [(n, f)], where [n] is the length of [x]
    encoded with [codec], and [f] is a function that will write the encoded [x]
    to the first [n] bytes of the provided {!Cstruct.t}. *)

type error = [ `Parse of string ]
(** Parse errors. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf err] pretty-prints [err] on [ppf]. *)

val decode : 'a codec -> Cstruct.t -> ('a * Cstruct.t, error) result
(** [decode codec cs] is the pair [(x, cs')], where [x] is the result of
    decoding the prefix of [cs] with [codec] and [cs'] are the trailing bytes,
    or an {!error}. *)

(** {2 Misc} *)

val random : 'a t -> 'a
(** [random asn] is a random inhabitant of ['a]. *)
