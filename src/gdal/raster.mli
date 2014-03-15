(** {1 Raster Bands} *)

type t
val t : t Ctypes.typ

val get_size : t -> int * int
(** [get_size t] returns the [(x, y)] dimensions in pixels. *)

val read_int : t -> int array
(** [read_int t] reads the values from [t] as integers. *)

val read_float : t -> float array
(** [read_float t] reads the values from [t] as floating point values. *)

val write_int : t -> int array -> unit
val write_float : t -> float array -> unit
(** [write_* t data] writes [data] to [t]. *)

module Block : sig
  val get_size : t -> int * int

(*
  val read_int : t -> int * int -> int array array
  val read_float : t -> int * int -> float array array

  val write_int : t -> int * int -> int array array -> unit
  val write_float : t -> int * int -> float array array -> unit
*)
end

(**/**)
val get_x_size : t -> int
val get_y_size : t -> int
(**/**)
