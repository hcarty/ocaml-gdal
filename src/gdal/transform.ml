open Ctypes

exception Invalid_transform

type data_t = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
type result_t =
  (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t option

type 'a transform_t =
  'a -> int -> int -> float ptr -> float ptr -> float ptr -> int ptr -> int

let transform_t t =
  t @->
  int @-> int @->
  ptr double @-> ptr double @-> ptr double @->
  ptr int @->
  returning int

let transform_c name t =
  Lib.c name (transform_t t)

let transform_ml c t forward (xs : data_t) (ys : data_t) (zs : data_t) =
  let n =
    let open Bigarray in
    let nx = Array1.dim xs in
    let ny = Array1.dim ys in
    let nz = Array1.dim zs in
    if nx = ny && nx = nz then
      nx
    else
      invalid_arg "Coordinate mismatch"
  in
  let success =
    let open Bigarray in
    let a = Array1.create int c_layout n in
    Array1.fill a 1;
    a
  in
  let result =
    c t (if forward then 1 else 0) n
      (bigarray_start array1 xs)
      (bigarray_start array1 ys)
      (bigarray_start array1 zs)
      (bigarray_start array1 success)
  in
  if result = 0 then
    None
  else
    Some success

module Gen_img = struct
  type t = T.t
  let t = T.t

  type reference_t = [
      `data_set of Data_set.t
    | `wkt of string
  ]

  let tuple_of_transform = function
    | `data_set ds -> Some ds, None
    | `wkt wkt -> None, Some wkt

  let create =
    Lib.c "GDALCreateGenImgProjTransformer"
      (Data_set.t_opt @-> string_opt @-> Data_set.t_opt @-> string_opt @->
       int @-> double @-> int @->
       returning t)

  let destroy =
    Lib.c "GDALDestroyGenImgProjTransformer"
      (t @-> returning void)

  let c =
    transform_c "GDALGenImgProjTransform" t

  let ml t =
    transform_ml c t

  let create ?gcp ~src ~dst =
    let gcp_ok, gcp_order =
      match gcp with
      | None -> false, 0
      | Some (_ as g) -> g
    in
    let gcp_ok = if gcp_ok then 1 else 0 in
    let src_ds, src_wkt = tuple_of_transform src in
    let dst_ds, dst_wkt = tuple_of_transform dst in
    let t = create src_ds src_wkt dst_ds dst_wkt gcp_ok 0.0 gcp_order in
    if t = null then raise Invalid_transform;
    Gc.finalise destroy t;
    t
end

module Repojection = struct
  type t = T.t
  let t = T.t

  let create =
    Lib.c "GDALCreateReprojectionTransformer"
      (string @-> string @-> returning t)

  let destroy =
    Lib.c "GDALDestroyReprojectionTransformer"
      (t @-> returning void)

  let c =
    transform_c "GDALReprojectionTransform" t

  let ml t =
    transform_ml c t

  let create ~src ~dst =
    let t = create src dst in
    if t = null then raise Invalid_transform;
    Gc.finalise destroy t;
    t
end

type t =
  | Gen_img of Gen_img.t
  | Repojection of Repojection.t

let make_gen_img ?gcp ~src ~dst =
  let t = Gen_img.create ?gcp ~src ~dst in
  Gen_img t

let make_reprojection ~src ~dst =
  let t = Repojection.create ~src ~dst in
  Repojection t

let transform = function
  | Gen_img g -> Gen_img.ml g
  | Repojection r -> Repojection.ml r

let get_transform_t = function
  | Gen_img g -> g
  | Repojection r -> r

let get_transform_c = function
  | Gen_img _ -> Gen_img.c
  | Repojection _ -> Repojection.c
