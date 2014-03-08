val init_dynamic : ?lib:string -> unit -> unit
(** [init_dynamic ?lib ()] will load the shared object [lib].  It must be run
    before using this library.

    @param lib defaults to ["libgdal.so"] *)

val register_all : unit -> unit
(** [register_all ()] will register all OGR data sources.  This should be run
    before using most IO can be performed. *)

val get_last_error_message : unit -> string
(** [get_last_error_message ()] returns a string representation of the last
    error to occur. *)

(**/**)
val c : string -> ('a -> 'b) Ctypes.fn -> 'a -> 'b
val protect : ('a -> 'b) -> 'a -> finally:('a -> unit) -> 'b
(** Internal support functions *)
