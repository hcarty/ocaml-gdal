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

(* Safely convert string lists (NULL for empty lists) *)
let convert_creation_options options =
  match options with
  | [] -> from_voidp string_opt null
  | _ ->
    (* We need a null pointer at the end of the array *)
    let n = List.length options + 1 in
    let ca = CArray.make string_opt n in
    List.iteri (
      fun i o ->
        CArray.set ca i (Some o)
    ) options;
    CArray.set ca (n - 1) None;
    CArray.start ca

let set_cache_max =
  c "GDALSetCacheMax64"
    (int64_t @-> returning void)

let get_cache_max =
  c "GDALGetCacheMax64"
    (void @-> returning int64_t)

let get_cache_used =
  c "GDALGetCacheUsed64"
    (void @-> returning int64_t)

let check_version =
  c "GDALCheckVersion"
    (int @-> int @-> string_opt @-> returning int)

let check_version ?caller ~major ~minor =
  check_version major minor caller <> 0

let set_config_option =
  c "CPLSetConfigOption"
    (string @-> string_opt @-> returning void)

let get_config_option =
  c "CPLGetConfigOption"
    (string @-> string_opt @-> returning string_opt)

let get_config_option ?default key =
  get_config_option key default
