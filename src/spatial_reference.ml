open Ctypes
open Foreign

type t = T.t
let t = T.t
let t_opt = T.t_opt

let new_spatial_reference =
  Lib.c "OSRNewSpatialReference"
    (ptr_opt string @-> returning t)

let import_from_proj4 =
  Lib.c "OSRImportFromProj4"
    (t @-> string @-> returning int)

let import_from_wkt =
  Lib.c "OSRImportFromWkt"
    (t @-> ptr string @-> returning int)

let destroy_spatial_reference =
  Lib.c "OSRDestroySpatialReference"
    (t @-> returning void)

let export_to_proj4 =
  Lib.c "OSRExportToProj4"
    (t @-> ptr string @-> returning int)

let export_to_wkt =
  Lib.c "OSRExportToWkt"
    (t @-> ptr string @-> returning int)

let export_to_pretty_wkt =
  Lib.c "OSRExportToPrettyWkt"
    (t @-> ptr string @-> int @-> returning int)

let free =
  Lib.c "OGRFree"
    (ptr void @-> returning void)

(* Higher level, wrapping functions *)

let make kind spec =
  let sr = new_spatial_reference None in
  Gc.finalise (fun x -> prerr_endline "sr free"; destroy_spatial_reference x) sr;
  let result =
    match kind with
    | `proj4 -> import_from_proj4 sr spec
    | `wkt ->
      let spec_ptr = allocate string spec in
      import_from_wkt sr spec_ptr
  in
  if result = 0 then
    `Ok sr
  else
    `Error `Invalid_spec

let to_proj4 sr =
  let s = allocate string "" in
  if export_to_proj4 sr s = 0 then (
    let result = !@ s in
    `Ok result
  )
  else
    `Error `Unable_to_export_to_proj4

let to_wkt ?(pretty = false) ?(simplify = false) sr =
  let s = allocate string "" in
  let f x =
    if pretty then (
      export_to_pretty_wkt sr x (if simplify then 1 else 0)
    )
    else (
      export_to_wkt sr x
    )
  in
  if f s = 0 then (
    let result = !@ s in
    `Ok result
  )
  else
    `Error `Unable_to_export_to_wkt
