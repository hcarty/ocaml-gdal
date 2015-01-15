(** {1 OGR Data Sources} *)

(** These are bindings and wrappers around the [OGR_DS_*] API. *)

type t
(** Data source *)

val of_source : ?write:bool -> string -> [ `Invalid_source | `Ok of t ]
(** [of_source ?write name] opens the source [name] for access.

    @param write defaults to false (read-only)
    @param name is source-type specific.  See the upstream OGR documentation
           for more information.

    @return `Invalid_source if [name] does not represent a valid data source. *)

val of_source_exn : ?write:bool -> string -> t
(** Like {!of_source} but raises {!Invalid_source} if there is an error with
    the data source. *)

val destroy : t -> unit
(** [destroy t] releases the resources associated with [t]. *)

val with_source :
  ?write:bool ->
  string ->
  (t -> 'a) ->
  [ `Invalid_source | `Ok of 'a ]
(** [with_source ?write name f] opens [name] and calls [f src] if [name] is a
    valid data source.  The data source passed to [f] will be closed if [f]
    returns normally or raises an exception.

    This is a wrapper around {!of_source}.  See its documentation for a
    description of the expected arguments. *)

val with_source_exn : ?write:bool -> string -> (t -> 'a) -> 'a
(** Like {!with_source} but raises {!Invalid_source} if there is an error with
    the data source. *)

val get_layer_by_name : t -> string -> Layer.t
val get_layer : t -> int -> Layer.t
(** [get_layer* src id] returns a {!Layer.t} extracted from [src]. *)

(**/**)

(** {2 Low level wrappers} *)

(** These should not be necessary under normal circumstances. *)

exception Data_source_error

val release : t -> unit
(** @raise Data_source_error *)
