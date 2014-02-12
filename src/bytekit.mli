
open Bigarray

type bytes = (int, int8_unsigned_elt, c_layout) Array1.t

module Rd : sig
  val drop : int -> bytes -> bytes
  val isolate : int -> bytes -> bytes * bytes
  val eof : bytes -> bool
  val empty : bytes
end

module Wr : sig
  type t
  val size : t -> int
  val empty : t
  val (<>) : t -> t -> t
  val list : int list -> t
  val array : int array -> t
  val string : ?f:(int -> int) -> string -> t
  val bytes : bytes -> t
  val byte : int -> t
  val to_bytes : t -> bytes
end
