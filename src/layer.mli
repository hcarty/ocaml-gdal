type t

val t : t Ctypes.typ

val reset_reading : t -> unit

val get_next_feature : t -> Feature.t option
val get_layer_defn : t -> Feature.Defn.t

val map_features : t -> (Feature.t -> 'a) -> 'a list
val iter_features : t -> (Feature.t -> unit) -> unit
