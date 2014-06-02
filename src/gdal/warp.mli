type options_t

type resample_t =
  | Nearest_neighbor
  | Bilinear
  | Cubic
  | Cubic_spline
  | Lanczos

exception Warp_error

val reproject_image :
  ?memory_limit:float ->
  ?max_error:float ->
  ?options:options_t ->
  ?src_wkt:string ->
  ?dst_wkt:string ->
  src:Data_set.t ->
  dst:Data_set.t ->
  resample_t ->
  unit
(** [reproject_image ?memory_limit ?max_error ?options ?src_wkt ?dst_wkt ~src ~dst alg]
    reprojects the image [src] to [dst], overwriting [dst] in the process.

    @param memory_limit is specified in bytes.
    @param max_error is the maximum error allowed between the [src] and [dst].
    Defaults to [0.0].
    @param src_wkt may be used to override the projection information in [src].
    @param dst_wkt may be used to override the projection information in [dst].
    @param alg specifies which resampling algorithm to use.

    @raise Warp_error if the operation can not be performed. *)
