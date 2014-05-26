(** {1 Raster Bands} *)

type t
val t : t Ctypes.typ

module Data : sig
  type ('v, 'e) t =
    | Byte : (int, Bigarray.int8_unsigned_elt) t
    | UInt16 : (int, Bigarray.int16_unsigned_elt) t
    | Int16 : (int, Bigarray.int16_signed_elt) t
    | UInt32 : (int32, Bigarray.int32_elt) t
    | Int32 : (int32, Bigarray.int32_elt) t
    | Float32 : (float, Bigarray.float32_elt) t
    | Float64 : (float, Bigarray.float64_elt) t

  val to_ba_kind : ('v, 'e) t -> ('v, 'e) Bigarray.kind
  (** [to_ba_kind t] returns the {!Bigarray.kind} matching [t]. *)

  (**/**)
  val to_int : (_, _) t -> int
  val to_int_opt : (_, _) t option -> int
  (**/**)
end

exception IO_error
exception Invalid_dimensions

val get_size : t -> int * int
(** [get_size t] returns the [(x, y)] dimensions in pixels. *)

val get_data_type : t -> [
    `byte
  | `uint16
  | `int16
  | `uint32
  | `int32
  | `float32
  | `float64
  | `unknown
  | `unhandled
  ]
(** [get_data_type t] returns the data type of the given raster band.

    @return `unknown if GDAL does not know the data type
    @return `unhandled if the data type is recognized by GDAL but unhandled by
    the OCaml bindings. *)

val get_band_number : t -> int option
(** [get_band_number t] returns the index of [t] in its dataset or [None] if
    the index is unknown. *)

val read :
  ?offset:int * int ->
  ?size:int * int ->
  ?pixel_spacing:int ->
  ?line_spacing:int ->
  ?buffer_size:int * int ->
  t -> ('v, 'e) Data.t -> ('v, 'e, Bigarray.c_layout) Bigarray.Array2.t
(** [read t kind] reads the values from [t] as [kind] values. *)

val write :
  ?offset:int * int ->
  ?size:int * int ->
  ?pixel_spacing:int ->
  ?line_spacing:int ->
  t -> ('v, 'e) Data.t -> ('v, 'e, Bigarray.c_layout) Bigarray.Array2.t ->
  unit
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
