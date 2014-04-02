(** {1 Raster Bands} *)

type t
val t : t Ctypes.typ

module Data : sig
  type 'a t

  val byte : int t
  val uint16 : int t
  val int16 : int t
  val uint32 : Unsigned.uint32 t
  val int32 : int32 t
  val float32 : float t
  val float64 : float t

  (**/**)
  val to_int : _ t -> int
  (**/**)
end

val get_size : t -> int * int
(** [get_size t] returns the [(x, y)] dimensions in pixels. *)

val get_data_type : t -> [
    `int of int Data.t
  | `uint32 of Unsigned.uint32 Data.t
  | `int32 of int32 Data.t
  | `float of float Data.t
  | `unknown
  | `unhandled
  ]
(** [get_data_type t] returns the data type of the given raster band.

    @return `unknown if GDAL does not know the data type
    @return `unhandled if the data type is recognized by GDAL but unhandled by
    the OCaml bindings. *)

val read : t -> 'a Data.t -> 'a array
(** [read t kind] reads the values from [t] as [kind] values. *)

val write : t -> 'a Data.t -> 'a array -> unit
(** [write t kind data] writes the values from [t] as [kind] values. *)

val get_description : t -> string
(** [get_description t] returns the description of the current band.  If no
    description exists then the returned string will be empty. *)

val set_description : t -> string -> unit
(** [set_description t desc] sets the description of [t] to [desc]. *)

module Block : sig
  val get_size : t -> int * int
  (** [get_size t] returns the native [(x, y)] dimensions of the blocks making
      up [t]. *)
end

(**/**)
val get_x_size : t -> int
val get_y_size : t -> int
(**/**)
