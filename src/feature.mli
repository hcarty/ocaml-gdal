type t

val t : t Ctypes.typ
val t_opt : t option Ctypes.typ

val get_as_integer : t -> int -> int
val get_as_double : t -> int -> float
val get_as_string : t -> int -> string
val get_geometry_ref : t -> Geometry.t

val destroy : t -> unit

module Defn : sig
  type t

  val t : t Ctypes.typ

  val get_field_count : t -> int
  val get_field_defn : t -> int -> Field.Defn.t
end
