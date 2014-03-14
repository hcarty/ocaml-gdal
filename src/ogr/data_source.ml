open Ctypes
open Foreign

type t = T.t
let t = T.t

exception Data_source_error

let err = T.err Data_source_error

let open_ = (* 'open' is a keyword in OCaml *)
  Lib.c "OGROpen"
    (string @-> int @-> ptr_opt Driver.t @-> returning t)

let destroy =
  Lib.c "OGR_DS_Destroy"
    (t @-> returning void)

let release =
  Lib.c "OGRReleaseDataSource"
    (t @-> returning err)

let get_layer_by_name =
  Lib.c "OGR_DS_GetLayerByName"
    (t @-> string @-> returning Layer.t)

let get_layer =
  Lib.c "OGR_DS_GetLayer"
    (t @-> int @-> returning Layer.t)

let of_source ?(write = false) name =
  let h = open_ name (if write then 1 else 0) None in
  if h = null then
    `Invalid_source
  else
    `Ok h

let with_source ?write name f =
  match of_source ?write name with
  | `Ok h -> `Ok (Lib.protect f h ~finally:destroy)
  | `Invalid_source -> `Invalid_source
