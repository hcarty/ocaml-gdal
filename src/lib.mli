val init_dynamic : ?lib:string -> unit -> unit
val register_all : unit -> unit
val get_last_error_message : unit -> string

val c : string -> ('a -> 'b) Ctypes.fn -> 'a -> 'b

val protect : ('a -> 'b) -> 'a -> finally:('a -> unit) -> 'b
