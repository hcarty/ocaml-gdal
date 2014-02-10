type t
val t : t Ctypes.typ
val t_opt : t option Ctypes.typ

val make : [ `proj4 | `wkt ] -> string -> t

val to_proj4 : t -> string
val to_wkt : ?pretty:bool -> ?simplify:bool -> t -> string
