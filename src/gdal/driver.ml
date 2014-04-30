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

let get_by_name =
  Lib.c "GDALGetDriverByName"
    (string @-> returning t)

let get_by_name name =
  let driver = get_by_name name in
  if driver = null then
    None
  else
    Some driver
