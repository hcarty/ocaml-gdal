val init_dynamic : ?lib:string -> unit -> unit
(** [init_dynamic ?lib ()] will load the shared object [lib].  It must be run
    before using this library.

    @param lib defaults to ["libgdal.so"] *)

val all_register : unit -> unit
val register_all : unit -> unit
(** [register_all ()] will register all GDAL data sources.  This should be run
    before using most IO can be performed.

    {!all_register} is an alias to match the GDAL C API spelling. *)

val check_version : ?caller:string -> major:int -> minor:int -> bool
(** [check_version ~major ~minor] returns [true] if the loaded GDAL version
    matches [major.minor]. *)

val set_cache_max : int64 -> unit
(** [set_cache_max bytes] sets the maximum cache size in bytes used by GDAL for
    raster block IO. *)

val get_cache_max : unit -> int64
(** [get_cache_max ()] returns the maximum block IO cache size in bytes. *)

val get_cache_used : unit -> int64
(** [get_cache_used ()] returns the number of bytes in GDAL's IO cache. *)

(**/**)
val c : string -> ('a -> 'b) Ctypes.fn -> 'a -> 'b
val protect : ('a -> 'b) -> 'a -> finally:('a -> unit) -> 'b
val convert_creation_options : string list -> unit Ctypes.ptr
(** Internal support functions *)
