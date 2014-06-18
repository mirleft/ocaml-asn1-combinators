

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



type t = {
  date : (int * int * int) ;
  time : (int * int * int * float) ;
  tz   : (int * int * [ `W | `E ]) option ;
}

(* XXX Hide repr, add validating ctor. *)

let to_posix_time { date ; time ; tz } =
  let (y, m, d)        = date
  and (hh, mm, ss, ff) = time
  and tz_mm =
    match tz with
    | Some (tzh, tzm, `W) -> - (tzh * 60 + tzm)
    | Some (tzh, tzm, `E) -> tzh * 60 + tzm
    | None                -> 0 in
  date_to_posix_time ~y ~m ~d ~hh ~mm ~ss ~ff ~tz_mm
