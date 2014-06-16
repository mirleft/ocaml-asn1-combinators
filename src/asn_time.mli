
val date_to_posix_time :
  y:int -> m:int -> d:int ->
  hh:int -> mm:int -> ss:int ->
  ff:float -> tz_mm:int -> float
(** [date_to_posix_time ~y ~m ~d ~hh ~mm ~ss ~ff ~tz_mm] is the POSIX
    time corresponding to the calendar date [y-m-d] at time [hh:ss:mm.ff] 
    with time zone offset [tz_mm] in minutes. 

    {b Warning.} Does check ranges or that [y-m-d] is a valid calendar date. *)
