(* Copyright (c) 2014-2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

type t = private Oid of int * int * int list

val compare     : t -> t -> int
val equal       : t -> t -> bool
val hash        : t -> int
val seeded_hash : int -> t -> int

val base  : int -> int -> t
val (<|)  : t -> int -> t
val (<||) : t -> int list -> t

val pp        : Format.formatter -> t -> unit
val to_list   : t -> int list
val of_string : string -> t
