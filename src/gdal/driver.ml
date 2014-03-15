open Ctypes
open Foreign

type t = T.t
let t = T.t

let get_short_name =
  Lib.c "GDALGetDriverShortName"
    (t @-> returning string)

let get_long_name =
  Lib.c "GDALGetDriverLongName"
    (t @-> returning string)
