type t =
  | Integer
  | IntegerList
  | Real
  | RealList
  | String
  | StringList
  | WideString
  | WideStringList
  | Binary
  | Date
  | Time
  | DateTime

module Defn : sig
  type t
  val t : t Ctypes.typ
end

val get_type : Defn.t -> t
