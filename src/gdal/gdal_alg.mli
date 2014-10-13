(** {1 GDAL Algorithms} *)

exception Algorithm_error

val proximity : ?options:string list -> src:(_, _) Gdal_band.t -> test:(_, _) Gdal_band.t -> unit
(** [proximity ?options ~src ~test] computes the pixel-by-pixel proximity
    between [src] and [test]. *)

val fill_nodata :
  ?options:string list ->
  target:(_, _) Gdal_band.t -> mask:(_, _) Gdal_band.t -> float -> int -> unit
(** [fill_nodata ?options ~target ~mask distance iterations] will fill in
    [target]'s missing data pixels.  See GDAL's [GDALFillNoData] documentation
    for an explanation of the function parameters. *)

module Grid : sig
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

  val make :
    interpolate_t ->
    (float * float * float) list ->
    xrange:int * float * float ->
    yrange:int * float * float -> ('v, 'e) Gdal_band.Data.t ->
    ('v, 'e, Bigarray.c_layout) Bigarray.Array2.t
  (** [make interp points ~xrange ~yrange kind] interpolates the data in
      [points] onto a regular grid specified by [xrange] and [yrange].

      @param interp specifies the interpolation method to use.
      @param points is a list of [(x, y, z)] triples.
      @param xrange is a [(steps, min, max)] triple.
      @param yrange is a [(steps, min, max)] triple.
      @param kind specifies the data type to use for the resulting values. *)
end
