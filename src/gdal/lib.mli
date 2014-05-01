val init_dynamic : ?lib:string -> unit -> unit
(** [init_dynamic ?lib ()] will load the shared object [lib].  It must be run
    before using this library.

    @param lib defaults to ["libgdal.so"] *)

val all_register : unit -> unit
val register_all : unit -> unit
(** [register_all ()] will register all GDAL data sources.  This should be run
    before using most IO can be performed.

    {!all_register} is an alias to match the GDAL C API spelling. *)

(**/**)
val c : string -> ('a -> 'b) Ctypes.fn -> 'a -> 'b
val protect : ('a -> 'b) -> 'a -> finally:('a -> unit) -> 'b
val convert_creation_options : string list -> unit Ctypes.ptr
(** Internal support functions *)
