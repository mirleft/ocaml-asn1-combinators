(* Copyright (c) 2014-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

val lex_compare : string -> string -> int

type t

val immediate : int -> (int -> bytes -> unit) -> t

val len    : t -> int
val empty  : t
val (<+>)  : t -> t -> t
val append : t -> t -> t
val concat : t list -> t

val of_list    : int list -> t
val of_octets  : string -> t
val of_byte    : int -> t

val to_octets  : t -> string
val to_writer  : t -> int * (bytes -> unit)

