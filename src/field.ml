open Ctypes
open Foreign
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

let oft_of_int = function
  | 0 -> Integer
  | 1 -> IntegerList
  | 2 -> Real
  | 3 -> RealList
  | 4 -> String
  | 5 -> StringList
  | 6 -> WideString
  | 7 -> WideStringList
  | 8 -> Binary
  | 9 -> Date
  | 10 -> Time
  | 11 -> DateTime
  | _ -> raise (Invalid_argument "Field.of_int")

let int_of_oft = function
  | Integer -> 0
  | IntegerList -> 1
  | Real -> 2
  | RealList -> 3
  | String -> 4
  | StringList -> 5
  | WideString -> 6
  | WideStringList -> 7
  | Binary -> 8
  | Date -> 9
  | Time -> 10
  | DateTime -> 11

module Defn = struct
  type t = T.Defn.t
  let t = T.Defn.t
end

let get_type =
  Lib.c "OGR_Fld_GetType"
    (Defn.t @-> returning int)

let get_type defn =
  get_type defn
  |> oft_of_int
