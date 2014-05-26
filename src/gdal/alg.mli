(** {1 GDAL Algorithms} *)

type t = T.t
val t : T.t Ctypes.typ

exception Algorithm_error

val proximity : ?options:string list -> src:Band.t -> test:Band.t -> unit

val fill_nodata :
  target:Band.t -> mask:Band.t -> float -> int -> string list -> unit

type interpolate_t

val inverse_distance_to_a_power :
  power:float ->
  smoothing:float ->
  anisotropy_ratio:float ->
  anisotropy_angle:float ->
  radius:float * float ->
  angle:float ->
  points:int * int ->
  no_data_value:float ->
  interpolate_t

val moving_average :
  radius:float * float ->
  angle:float ->
  min_points:int ->
  no_data_value:float ->
  interpolate_t

val nearest_neighbor :
  radius:float * float ->
  angle:float ->
  no_data_value:float ->
  interpolate_t

type metric_t =
  radius:float * float ->
  angle:float ->
  min_points:int ->
  no_data_value:float ->
  interpolate_t

val metric_minimum : metric_t
val metric_maximum : metric_t
val metric_range : metric_t
val metric_count : metric_t
val metric_average_distance : metric_t
val metric_average_distance_points : metric_t

val grid_create :
  interpolate_t ->
  (float * float * float) list ->
  xrange:int * float * float ->
  yrange:int * float * float -> ('v, 'e) Band.Data.t ->
  ('v, 'e, Bigarray.c_layout) Bigarray.Array2.t
