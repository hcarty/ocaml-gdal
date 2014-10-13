(** {1 Features} *)

(** These functions come from OGR C API's [OGR_F_*] namespace. *)

type t

val t : t Ctypes.typ
val t_opt : t option Ctypes.typ
(** Values for proper Ctypes integration/extension *)

val get_as_integer : t -> int -> int
val get_as_double : t -> int -> float
val get_as_string : t -> int -> string
(** [get_as_* t i] returns field [i] from feature [t]. *)

val get_geometry_ref : t -> Ogr_geometry.t
(** [get_geometry_ref t] returns the geometry associated with [t]. *)

val destroy : t -> unit
(** [destroy t] frees the feature [t]. *)

module Defn : sig
  (** {1 Feature Definitions} *)

  (** These functions come from the OGR C API's [OGR_FD_*] namespace. *)

  type t

  val t : t Ctypes.typ
  (** Values for proper Ctypes integration/extension *)

  val get_field_count : t -> int
  (** [get_field_count t] returns the number of fields associated with the
      feature definition [t]. *)

  val get_field_defn : t -> int -> Ogr_field.Defn.t
  (** [get_field_defn t i] returns the [i]th {!Ogr_field.Defn.t} from [t]. *)
end
