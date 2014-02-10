open Ctypes
open Foreign

type t = T.t
let t = T.t

exception Invalid_source

let open_ = (* 'open' is a keyword in OCaml *)
  Lib.c "OGROpen"
    (string @-> int @-> ptr_opt Driver.t @-> returning t)

let destroy =
  Lib.c "OGR_DS_Destroy"
    (t @-> returning void)

let release =
  Lib.c "OGRReleaseDataSource"
    (t @-> returning int)

let get_layer_by_name =
  Lib.c "OGR_DS_GetLayerByName"
    (t @-> string @-> returning Layer.t)

let get_layer =
  Lib.c "OGR_DS_GetLayer"
    (t @-> int @-> returning Layer.t)

let of_source ?(write = false) name =
  let h = open_ name (if write then 1 else 0) None in
  if h = null then
    raise Invalid_source
  else
    h

let with_source ?write name f =
  let h = of_source ?write name in
  Lib.protect f h ~finally:destroy
