exception Invalid_transform

type data_t =
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
(** Input coordinates are provided as bigarrays. *)

type result_t =
  (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t option
(** Success/failure of coordinate transformations. *)

type t
(** Transformation *)

val make_gen_img :
  ?gcp:bool * int ->
  src:[ `data_set of Data_set.t | `wkt of string ] ->
  dst:[ `data_set of Data_set.t | `wkt of string ] ->
  t
(** [make_gen_img ?gcp ~src ~dst] creates a transformation definition between
    the coordinate system of [src] and [dst].

    @param gcp defaults to [(false, 0)].  See GDAL's
    [GDALCreateGenImgProjTransformer] documentation for an explanation of how
    GCPs may be used. *)

val make_reprojection : src:string -> dst:string -> t
(** [make_reprojection ~src ~dst] creates a transformation definition between
    the WKT definition in [src] and the WKT definition in [dst]. *)

val transform : t -> bool -> data_t -> data_t -> data_t -> result_t
(** [transform t invert xs ys zs] converts the coordinates [xs], [ys], [zs]
    according to the transformation defined in [t].

    @param invert reverses the direction of the transformation [t].
    @param xs is modified in place with the transformation result.
    @param ys is modified in place with the transformation result.
    @param zs is modified in place with the transformation result.

    @return [Some success] where [success] is an array of values indicating
    whether an individual point's transformation is successful or not.  [None]
    is returned if the overall transformation fails. *)
