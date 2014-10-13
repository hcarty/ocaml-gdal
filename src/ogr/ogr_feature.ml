open Ctypes
open Foreign

module Defn = struct
  type t = Ogr_t.Defn.t
  let t = Ogr_t.Defn.t

  let get_field_count =
    Ogr_lib.c "OGR_FD_GetFieldCount"
      (t @-> returning int)

  let get_field_defn =
    Ogr_lib.c "OGR_FD_GetFieldDefn"
      (t @-> int @-> returning Ogr_field.Defn.t)
end

type t = Ogr_t.t
let t = Ogr_t.t
let t_opt = Ogr_t.t_opt

let get_as_integer =
  Ogr_lib.c "OGR_F_GetFieldAsInteger"
    (t @-> int @-> returning int)

let get_as_double =
  Ogr_lib.c "OGR_F_GetFieldAsDouble"
    (t @-> int @-> returning float)

let get_as_string =
  Ogr_lib.c "OGR_F_GetFieldAsString"
    (t @-> int @-> returning string)

let get_geometry_ref =
  Ogr_lib.c "OGR_F_GetGeometryRef"
    (t @-> returning Ogr_geometry.t)

let destroy =
  Ogr_lib.c "OGR_F_Destroy"
    (t @-> returning void)
