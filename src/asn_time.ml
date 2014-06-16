(* Modified julian days. Algorithms from the Calendar FAQ: 
   http://www.tondering.dk/claus/calendar.html *)

let mjd_posix_epoch = 40587                 (* origin of posix epoch in mdj. *)
let mjd_origin_jd = 2_400_001                (* origin of mdj in julian day. *)
let mjd_of_gregorian year month day =
  let a = (14 - month) / 12 in
  let y = year + 4800 - a in
  let m = month + 12 * a - 3 in
  let jd =                                                   (* julian day *)
    day + ((153 * m) + 2)/ 5 + 365 * y + 
    (y / 4) - (y / 100) + (y / 400) - 32045
  in
  jd - mjd_origin_jd

(* Posix time counts seconds since 1970-01-01 00:00:00 UTC without
   counting leap seconds (when a leap second occurs a posix second can
   be two SI seconds or zero SI second). Hence 864000 posix seconds
   always represent an UTC day and the translation below is completly
   accurate. Note that by definition a unix timestamp cannot represent a
   leap second. *)

let date_to_posix_time ~y ~m ~d ~hh ~mm ~ss ~ff ~tz_mm =
  let days_since_epoch = mjd_of_gregorian y m d - mjd_posix_epoch in 
  (float days_since_epoch) *. 86_400. +. 
  (float hh) *. 3600. +. 
  (float mm) *. 60. +. 
  (float ss) +.
  ff -.
  (float tz_mm) *. 60.

let test () = (* round trip with result of Unix.gmtime  *)
  let about_200_years = Int64.(mul 200L (mul 365L 86_400L)) in
  let rtime span = Int64.(sub (Random.int64 (add span one)) (div span 2L)) in
  let test () = 
    let ti = rtime about_200_years (* around the unix epoch *) in
    let t = Int64.to_float ti in
    let tm = Unix.gmtime t in
    let t' = date_to_posix_time 
        ~y:(tm.Unix.tm_year + 1900)
        ~m:(tm.Unix.tm_mon + 1) 
        ~d:(tm.Unix.tm_mday)
        ~hh:(tm.Unix.tm_hour)
        ~mm:(tm.Unix.tm_min)
        ~ss:(tm.Unix.tm_sec) 
        ~ff:0.
        ~tz_mm:0
    in
    if t <> t' then Printf.eprintf "Failure on posix time: %Ld\n%!" ti else
    ()
(*    Printf.eprintf "Success on posix time: %Ld\n%!" ti *)
  in
  for i = 1 to 1_000_000_000 do test () done

let () = test ()
