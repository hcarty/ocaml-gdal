type resample_t =
  | Nearest_neighbor
  | Bilinear
  | Cubic
  | Cubic_spline
  | Lanczos

exception Warp_error

module Options : sig
  type t

  val create : unit -> t
  (** [create ()] creates a warping options value initialized with sane
      defaults. *)

  val clone : t -> t
  (** [clone t] returns a copy of [t]. *)

  val set_warp_options : t -> string list -> unit
  val set_memory_limit : t -> float -> unit
  val set_resample_alg : t -> resample_t -> unit
  val set_working_data_type : t -> (_, _) Band.Data.t -> unit
  val set_src : t -> Data_set.t -> unit
  val set_dst : t -> Data_set.t -> unit
  val set_bands : t -> (int * int) list -> unit
  val set_src_no_data_real : t -> float list -> unit
  val set_src_no_data_imag : t -> float list -> unit
  val set_dst_no_data_real : t -> float list -> unit
  val set_dst_no_data_real : t -> float list -> unit
  (** [set_* t ...] set warp option fields.  See the [gdalwarper.h]
      documentation for descriptions of the affected fields. *)

  val make :
    ?warp_options:string list ->
    ?memory_limit:float ->
    ?resample_alg:resample_t ->
    ?working_data_type:(_, _) Band.Data.t ->
    ?src:Data_set.t ->
    ?dst:Data_set.t ->
    ?bands:(int * int) list ->
    ?src_no_data_real:float list ->
    ?src_no_data_imag:float list ->
    ?dst_no_data_real:float list ->
    ?dst_no_data_imag:float list ->
    unit -> t
  (** Create and initialize warp options.  The arguments to [make] can be used
      to override GDAL's defaults.  The parameters match the [set_*] functions
      above. *)
end

val reproject_image :
  ?memory_limit:float ->
  ?max_error:float ->
  ?options:Options.t ->
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

val create_and_reproject_image :
  ?memory_limit:float ->
  ?max_error:float ->
  ?options:Options.t ->
  ?src_wkt:string ->
  ?dst_wkt:string ->
  ?create_options:string list ->
  Data_set.t ->
  filename:string ->
  Driver.t ->
  resample_t ->
  unit
(** [create_and_reproject_image ?memory_limit ?max_error ?options
     ?src_wkt ?dst_wkt ?create_options src filename driver alg]
    reprojects the image [src] to [filename].

    @param memory_limit is specified in bytes.
    @param max_error is the maximum error allowed between the [src] and [dst].
    Defaults to [0.0].
    @param src_wkt may be used to override the projection information in [src].
    @param dst_wkt specifies the projection to use when creating [filename].
    Defaults to the same projection as [src].
    @param filename specifies the file to write the result to.
    @param driver specifies the driver to use when creating [filename].
    @param alg specifies which resampling algorithm to use.

    @raise Warp_error if the operation can not be performed. *)

val auto_create_warped_vrt :
  ?src_wkt:string ->
  ?dst_wkt:string ->
  ?max_error:float ->
  ?options:Options.t ->
  Data_set.t ->
  resample_t ->
  Data_set.t
(** [auto_create_warped_vrt ?src_wkt ?dst_wkt ?max_error ?options src alg]
    creates a virtual dataset warped from [src].

    @param src_wkt may be used to override the projection information in [src].
    @param dst_wkt specifies the projection to use when creating [filename].
    Defaults to the same projection as [src].
    @param max_error is the maximum error allowed between the [src] and [dst].
    Defaults to [0.0].
    @param alg specifies which resampling algorithm to use.

    @raise Warp_error if the operation can not be performed. *)
