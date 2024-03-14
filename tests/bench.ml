(* Copyright (c) 2014-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

let measure f =
  let t1  = Sys.time () in
  let res = f () in
  let t2  = Sys.time () in
  Printf.printf "[time] %.03f s\n%!" (t2 -. t1) ;
  res

let time ?(iter=1) f =
  let rec go = function
    | 1 -> f ()
    | n -> ignore (f ()) ; go (pred n) in
  measure @@ fun () -> go iter

let read filename =
  let fd = Unix.(openfile filename [O_RDONLY] 0) in
  Fun.protect ~finally:(fun () -> Unix.close fd)
    (fun () ->
       let chunk_size = 2048 in
       let rec read acc =
         let buf = Bytes.create chunk_size in
         let r = Unix.read fd buf 0 chunk_size in
         if r = chunk_size then
           read (buf :: acc)
         else
           Bytes.sub buf 0 r :: acc
           |> List.rev
           |> List.map Bytes.unsafe_to_string
           |> String.concat ""
       in
       read [])

let bench_certs filename =
  let cs = read filename in
  let rec bench n cs =
    if String.length cs = 0 then n else
      match Asn.decode X509.cert_ber cs with
      | Ok (_, cs) -> bench (succ n) cs
      | Error e -> invalid_arg (Format.asprintf "%a" Asn.pp_error e) in
  time ~iter:1 @@ fun () ->
    let n = bench 0 cs in
    Printf.printf "parsed %d certs.\n%!" n

let _ = bench_certs "./rondom/certs.bin"
