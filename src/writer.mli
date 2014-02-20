
val cs_compare : Cstruct.t -> Cstruct.t -> int

type t

val immediate : int -> (int -> Cstruct.t -> unit) -> t

val size    : t -> int
val empty   : t
val (<>)    : t -> t -> t
val append  : t -> t -> t
val concat  : t list -> t

val list    : int list -> t
val array   : int array -> t
val string  : string -> t
val cstruct : Cstruct.t -> t
val byte    : int -> t

val to_cstruct : t -> Cstruct.t
val to_writer  : t -> int * (Cstruct.t -> unit)

