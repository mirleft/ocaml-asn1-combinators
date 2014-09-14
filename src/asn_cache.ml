
(*
 * Evil, horrible hack used for caching the HOAS-stype fixpoint combinator.
 * Asn_core.asn should be represented as a partial fold to eliminate this.
 *)
module Make (T : sig type 'a k type 'a v end) : sig
  val cached : 'a T.k -> (unit -> 'a T.v) -> 'a T.v
  val add    : 'a T.k -> 'a T.v -> 'a T.v
  val find   : 'a T.k -> 'a T.v
end
  =
struct

  let cast = Obj.magic

  let cache = Hashtbl.((create 100 : (unit, unit) t))

  let cached key cons =
    let key' = cast key in
    try cast (Hashtbl.find cache key') with
    | Not_found ->
        let res = cons () in
        ( Hashtbl.add cache key' (cast res) ; res )
  let add k v = ( Hashtbl.add cache (cast k) (cast v) ; v )

  let find k  = cast (Hashtbl.find cache @@ cast k)
end
