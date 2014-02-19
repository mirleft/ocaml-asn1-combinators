
open Bigarray

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


let mmap filename =
  let fd  = Unix.(openfile filename [O_RDONLY] 0) in
  let arr = Array1.map_file fd int8_unsigned c_layout false (-1) in
  ( Unix.close fd ; arr )

let serially_bytes f =
  let rec go acc bytes =
    if Array1.dim bytes = 0 then acc
    else let (a, bytes') = f bytes in
    go (a :: acc) bytes' in
  go []

let bench_certs filename =
  let arr = mmap filename in
  let bench () =
    let cs =
      serially_bytes (Asn.decode_exn X509.cert_ber) arr in
    Printf.printf "parsed %d cers.\n%!"
      (List.length cs) in
  time bench

let _ = bench_certs "./rondom/certs.bin"
