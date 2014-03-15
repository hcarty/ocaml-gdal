open Ctypes
open Foreign

(* Are we grabbing a .so at runtime? *)
let dynamic = ref None

(* Load the GDAL .so *)
let init_dynamic ?(lib = "libgdal.so") () =
  dynamic := Some (Dl.dlopen ~filename:lib ~flags:[Dl.RTLD_NOW])

(* Simple wrapper to use !from when it's set *)
let c name f x = foreign ?from:!dynamic name f x

let all_register =
  c "GDALAllRegister"
    (void @-> returning void)

let register_all = all_register

let protect f x ~finally =
  let res = try f x with e -> finally x; raise e in
  finally x;
  res
