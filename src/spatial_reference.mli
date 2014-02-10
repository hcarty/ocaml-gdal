type t
val t : t Ctypes.typ
val t_opt : t option Ctypes.typ

val make :
  [ `proj4 | `wkt ] ->
  string ->
  [ `Error of [> `Invalid_spec ] | `Ok of t ]

val to_proj4 :
  t -> [ `Error of [> `Unable_to_export_to_proj4 ] | `Ok of string ]
val to_wkt :
  ?pretty:bool ->
  ?simplify:bool ->
  t -> [ `Error of [> `Unable_to_export_to_wkt ] | `Ok of string ]
