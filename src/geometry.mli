type envelope_t = {
  min_x : float;
  max_x : float;
  min_y : float;
  max_y : float;
}

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

val wkb_of_int : int -> wkb_t
val int_of_wkb : wkb_t -> int

type t

val t : t Ctypes.typ

val get_type : t -> int
val get_x : t -> int -> float
val get_y : t -> int -> float
