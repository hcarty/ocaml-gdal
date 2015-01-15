open Ctypes
open Foreign

exception Geometry_error

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

let int_of_wkb = function
  | Unknown -> 0
  | Point -> 1       
  | LineString -> 2
  | Polygon -> 3
  | MultiPoint -> 4
  | MultiLineString -> 5
  | MultiPolygon -> 6
  | GeometryCollection -> 7
  | None -> 100
  | LinearRing -> 101
  | Point25D -> 0x80000001
  | LineString25D -> 0x80000002
  | Polygon25D -> 0x80000003
  | MultiPoint25D -> 0x80000004
  | MultiLineString25D -> 0x80000005
  | MultiPolygon25D -> 0x80000006
  | GeometryCollection25D -> 0x80000007

let wkb_of_int = function
  | 0 -> Unknown
  | 1 -> Point
  | 2 -> LineString
  | 3 -> Polygon
  | 4 -> MultiPoint
  | 5 -> MultiLineString
  | 6 -> MultiPolygon
  | 7 -> GeometryCollection
  | 100 -> None
  | 101 -> LinearRing
  | 0x80000001 -> Point25D
  | 0x80000002 -> LineString25D 
  | 0x80000003 -> Polygon25D 
  | 0x80000004 -> MultiPoint25D
  | 0x80000005 -> MultiLineString25D
  | 0x80000006 -> MultiPolygon25D
  | 0x80000007 -> GeometryCollection25D
  | _ -> raise Geometry_error

type t = T.t
let t = T.t

let get_type =
  Lib.c "OGR_G_GetGeometryType"
    (t @-> returning int)

let get_type t =
  get_type t
  |> wkb_of_int

let get_x =
  Lib.c "OGR_G_GetX"
    (t @-> int @-> returning float)

let get_y =
  Lib.c "OGR_G_GetY"
    (t @-> int @-> returning float)
