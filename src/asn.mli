(* Copyright (c) 2014-2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** Embed typed ASN.1 grammars in OCaml

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

  val to_string : t -> string [@@deprecated "use pp"]
  (** [to_string t] is the dotted-decimal representation of [t]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf oid] pretty-prints [oid] on [ppf] as dotted-decimal. *)

  val to_list : t -> int list
  (** [to_list oid] is the list [n1; n2; ...] if the OID is [n1.n2. ...]. *)

  val of_string : string -> t
  (** [of_string s] is the OID represented by [s].

      @raise Invalid_argument if [s] is not dotted-decimal, or the components
      are out of range. *)
end

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

exception Ambiguous_grammar

type error = [ `Parse of string ] (* XXX finer-grained *)
val pp_error : Format.formatter -> error -> unit

val parse_error_fmt : ('a, Format.formatter, unit, 'b) format4 -> 'a
val parse_error : string -> 'a

type 'a t
type 'a sequence
type 'a element

type tag_class = [ `Universal | `Application | `Private ]

val fix : ('a t -> 'a t) -> 'a t

val map : ?random:(unit -> 'b) -> ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t

val implicit : ?cls:tag_class -> int -> 'a t -> 'a t
val explicit : ?cls:tag_class -> int -> 'a t -> 'a t

val single : 'a element -> 'a sequence
val (  @ ) : 'a element -> 'b sequence -> ('a * 'b) sequence
val ( -@ ) : 'a element -> 'b element  -> ('a * 'b) sequence

val optional : ?label:string -> 'a t -> 'a option element
val required : ?label:string -> 'a t -> 'a element

val sequence : 'a sequence -> 'a t
val sequence2 : 'a element -> 'b element -> ('a * 'b) t
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

val sequence_of : 'a t -> 'a list t

val set : 'a sequence -> 'a t
val set2 : 'a element -> 'b element -> ('a * 'b) t
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

val set_of : 'a t -> 'a list t

val choice2 :
  'a t -> 'b t -> [ `C1 of 'a | `C2 of 'b ] t
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


val bool             : bool       t
val integer          : Z.t        t
val bit_string       : bool array t
val octet_string     : Cstruct.t  t
val null             : unit       t
val oid              : OID.t      t
val utc_time         : Time.t     t
val generalized_time : Time.t     t

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

val int : int t
val bit_string_cs    : Cstruct.t  t
val bit_string_flags : (int * 'a) list -> 'a list t

type encoding
val ber : encoding
val der : encoding

type 'a codec

val codec       : encoding -> 'a t -> 'a codec
val encode      : 'a codec -> 'a -> Cstruct.t
val encode_into : 'a codec -> 'a -> (int * (Cstruct.t -> unit))
val decode      : 'a codec -> Cstruct.t -> ('a * Cstruct.t, error) result

val random : 'a t -> 'a

