open Ctypes
open Foreign

type t = T.t
let t = T.t

type geotransform_t = float CArray.t

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
  let ca = CArray.make double 6 in
  get_geo_transform t (CArray.start ca);
  ca

let get_origin gt =
  CArray.get gt 0, CArray.get gt 3

let get_pixel_size gt =
  CArray.get gt 1, CArray.get gt 5

let get_rotation gt =
  CArray.get gt 2, CArray.get gt 4

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
    (t @-> int @-> returning Band.t)

let create_copy =
  Lib.c "GDALCreateCopy" (
    Driver.t @->
    string @->
    t @->
    int @->
    ptr void @-> ptr void @-> ptr void @->
    returning t
  )

let create_copy ?(strict = false) ?(options = []) src driver name =
  let options = Lib.convert_creation_options options in
  let dst =
    create_copy driver name src
      (if strict then 1 else 0)
      options null null
  in
  if dst = null then
    `Invalid_source
  else
    `Ok dst

let create =
  Lib.c "GDALCreate" (
    Driver.t @-> string @-> int @-> int @-> int @-> int @-> ptr void @->
    returning t
  )

let create ?(options = []) ?bands driver name (nx, ny) =
  let nbands, kind =
    match bands with
    | None -> 0, None
    | Some (n, kind) -> n, Some kind
  in
  let options = Lib.convert_creation_options options in
  create
    driver name nx ny nbands (Band.Data.to_int_opt kind) options

let carray_of_array kind a =
  let ca = CArray.make kind (Array.length a) in
  for i = 0 to Array.length a - 1 do
    CArray.set ca i a.(i);
  done;
  ca

let set_geo_transform =
  Lib.c "GDALSetGeoTransform"
    (t @-> ptr double @-> returning err)

let make_geo_transform ~origin ~pixel_size ~rotation =
  let ca = CArray.make double 6 in
  let s = CArray.set ca in
  s 0 @@ fst origin;
  s 1 @@ fst pixel_size;
  s 2 @@ fst rotation;
  s 3 @@ snd origin;
  s 4 @@ snd rotation;
  s 5 @@ snd pixel_size;
  ca

let set_geo_transform t gt =
  set_geo_transform t (CArray.start gt)

let set_projection =
  Lib.c "GDALSetProjection"
    (t @-> string @-> returning err)

let of_band =
  Lib.c "GDALGetBandDataset"
    (Band.t @-> returning t)

let apply_geo_transform_array =
  Lib.c "GDALApplyGeoTransform"
    (ptr double @->
     double @-> double @->
     ptr double @-> ptr double @->
     returning void)

let apply_geo_transform_array gt ~pixel ~line =
  let cx = allocate double 0.0 in
  let cy = allocate double 0.0 in
  let ca = carray_of_array double gt |> CArray.start in
  apply_geo_transform_array ca pixel line cx cy;
  !@cx, !@cy
