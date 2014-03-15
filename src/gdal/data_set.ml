module Oarray = Array
open Ctypes
open Foreign
module Carray = Array
module Array = Oarray

type t = T.t
let t = T.t

exception Data_set_error

let err = T.err Data_set_error

let open_ = (* 'open' is a keyword in OCaml *)
  Lib.c "GDALOpen"
    (string @-> int @-> returning t)

let close =
  Lib.c "GDALClose"
    (t @-> returning void)

let of_source ?(write = false) name =
  let h = open_ name (if write then 1 else 0) in
  if h = null then
    `Invalid_source
  else
    `Ok h

let with_source ?write name f =
  match of_source ?write name with
  | `Ok h -> `Ok (Lib.protect f h ~finally:close)
  | `Invalid_source -> `Invalid_source

let get_driver =
  Lib.c "GDALGetDatasetDriver"
    (t @-> returning Driver.t)

let get_projection =
  Lib.c "GDALGetProjectionRef"
    (t @-> returning string)

let get_geo_transform =
  Lib.c "GDALGetGeoTransform"
    (t @-> ptr double @-> returning err)

let get_geo_transform t =
  let ca = Carray.make double 6 in
  get_geo_transform t (Carray.start ca);
  Array.init 6 (fun i -> Carray.get ca i)

let origin_of_transform gt =
  gt.(0), gt.(3)

let pixel_size_of_transform gt =
  gt.(1), gt.(5)

let get_origin t =
  let gt = get_geo_transform t in
  origin_of_transform gt

let get_pixel_size t =
  let gt = get_geo_transform t in
  pixel_size_of_transform gt

let get_x_size =
  Lib.c "GDALGetRasterXSize"
    (t @-> returning int)

let get_y_size =
  Lib.c "GDALGetRasterYSize"
    (t @-> returning int)

let get_count =
  Lib.c "GDALGetRasterCount"
    (t @-> returning int)

let get_band =
  Lib.c "GDALGetRasterBand"
    (t @-> int @-> returning Raster.t)
