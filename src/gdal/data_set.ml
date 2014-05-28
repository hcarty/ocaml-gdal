open Ctypes

type t = T.t
let t = T.t

exception Data_set_error
exception Wrong_data_type

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

let get_band_data_type t i =
  let c = get_band t i in
  Band.get_data_type c

let get_band t i kind =
  let c = get_band t i in
  if Band.check_data_type c kind then
    (c, Band.Data.to_ba_kind kind)
  else
    raise Wrong_data_type

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

let set_projection =
  Lib.c "GDALSetProjection"
    (t @-> string @-> returning err)

let of_band =
  Lib.c "GDALGetBandDataset"
    (Band.t @-> returning t)

let of_band (band, _) =
  of_band band
