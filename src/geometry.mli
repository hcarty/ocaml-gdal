(** {1 Geometries} *)

(** These functions come from OGR C API's [OGR_G_*] namespace. *)

exception Geometry_error
(** Exception raised on invalid geometry operations *)

type envelope_t = {
  min_x : float;
  max_x : float;
  min_y : float;
  max_y : float;
}
(** Bounding box type, used internally by other OGR modules *)

type wkb_t =
  | Unknown
  | Point
  | LineString
  | Polygon
  | MultiPoint
  | MultiLineString
  | MultiPolygon
  | GeometryCollection
  | None
  | LinearRing
  | Point25D
  | LineString25D
  | Polygon25D
  | MultiPoint25D
  | MultiLineString25D
  | MultiPolygon25D
  | GeometryCollection25D
(** Geometry types *)

val wkb_of_int : int -> wkb_t
val int_of_wkb : wkb_t -> int
(** Conversion between {!wkb_t} and the C API's integer values *)

type t
(** An OGR geometry *)

val t : t Ctypes.typ
(** Values for proper Ctypes integration/extension *)

val get_type : t -> wkb_t
(** [get_type t] returns the type of the given geometry [t]. *)

val get_x : t -> int -> float
val get_y : t -> int -> float
(** [get_* t i] returns the position of the [i]th point in the geometry [t]. *)
