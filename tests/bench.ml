
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

let bench_certs filename =
  let cs = Unix_cstruct.of_fd @@ Unix.(openfile filename [O_RDONLY] 0)
  and dec = Asn.decode_exn X509.cert_ber in
  let rec bench n cs =
    if Cstruct.len cs = 0 then n else
      let (a, cs') = dec cs in
      bench (succ n) cs' in
  time ~iter:1 @@ fun () ->
    let n = bench 0 cs in
    Printf.printf "parsed %d certs.\n%!" n

let _ = bench_certs "./rondom/certs.bin"
