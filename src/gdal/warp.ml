open Ctypes

type options_t = unit ptr

exception Warp_error

let err = T.err Warp_error

type resample_t =
  | Nearest_neighbor
  | Bilinear
  | Cubic
  | Cubic_spline
  | Lanczos

let int_of_resample = function
  | Nearest_neighbor -> 0
  | Bilinear -> 1
  | Cubic -> 2
  | Cubic_spline -> 3
  | Lanczos -> 4

let reproject_image =
  Lib.c "GDALReprojectImage"
    (Data_set.t @-> string_opt @-> Data_set.t @-> string_opt @->
     int @-> double @-> double @->
     ptr void @-> ptr void @->
     ptr void @-> returning err)

let reproject_image ?(memory_limit = 0.0) ?(max_error = 0.0) ?(options = null)
    ?src_wkt ?dst_wkt ~src ~dst alg =
  reproject_image src src_wkt dst dst_wkt (int_of_resample alg)
    memory_limit max_error null null options
