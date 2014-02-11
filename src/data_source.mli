type t

exception Invalid_source
exception Data_source_error

val of_source : ?write:bool -> string -> t

val destroy : t -> unit
val release : t -> unit

val with_source :
  ?write:bool ->
  string ->
  (t -> 'a) ->
  'a

val get_layer_by_name : t -> string -> Layer.t
val get_layer : t -> int -> Layer.t
