open Bytekit

module Integer : sig
  type t = [ `B of Big_int.big_int | `I of int ]
  val of_bytes : int -> bytes -> t
  val to_bytes : t -> Wr.t
end

module String : sig
  val ascii_of_bytes : int -> bytes -> string
  val ascii_to_bytes : string -> Wr.t
end

module Bit_string : sig
  val bits_of_bytes : int -> bytes -> bool array
end
