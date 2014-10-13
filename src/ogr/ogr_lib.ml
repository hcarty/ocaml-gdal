open Ctypes
open Foreign

(* Are we grabbing a .so at runtime? *)
let dynamic = ref None

(* Load the GDAL .so *)
let init_dynamic ?(lib = "libgdal.so") () =
  dynamic := Some (Dl.dlopen ~filename:lib ~flags:[Dl.RTLD_NOW])

(* Simple wrapper to use !from when it's set *)
let c name f x = foreign ?from:!dynamic name f x

let register_all =
  c "OGRRegisterAll"
    (void @-> returning void)

let get_last_error_message =
  c "CPLGetLastErrorMsg"
    (void @-> returning string)

let protect f x ~finally =
  let res = try f x with e -> finally x; raise e in
  finally x;
  res
