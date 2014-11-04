
(*
 * Evil, horrible hack used for caching the HOAS-stype fixpoint combinator.
 * Asn_core.asn should be represented as a partial fold to eliminate this.
 *)
module Make (T : sig type 'a k type 'a v end) : sig
  type t
  val create : unit -> t
  val add    : t -> 'a T.k -> 'a T.v -> 'a T.v
  val find   : t -> 'a T.k -> 'a T.v
  val once   : t -> 'a T.k -> (unit -> 'a T.v) -> 'a T.v
end
  =
struct

  type t = (unit, unit) Hashtbl.t

  let cast = Obj.magic

  let create () = Hashtbl.create 16

  let add t k v = ( Hashtbl.add t (cast k) (cast v) ; v )
  let find t k  = cast @@ Hashtbl.find t @@ cast k

  let once t k cons =
    let k' = cast k in
    try cast @@ Hashtbl.find t k'
    with Not_found -> add t k (cons ())
end
