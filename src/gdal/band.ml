open Ctypes

type c = T.t
type ('v, 'e) t = c * ('v, 'e) Bigarray.kind
let t = T.t

module Data = struct
  type (_, _) t =
    | Byte : (int, Bigarray.int8_unsigned_elt) t
    | UInt16 : (int, Bigarray.int16_unsigned_elt) t
    | Int16 : (int, Bigarray.int16_signed_elt) t
    | UInt32 : (int32, Bigarray.int32_elt) t
    | Int32 : (int32, Bigarray.int32_elt) t
    | Float32 : (float, Bigarray.float32_elt) t
    | Float64 : (float, Bigarray.float64_elt) t

  let to_int : type v e. (v, e) t -> int = function
    | Byte -> 1
    | UInt16 -> 2
    | Int16 -> 3
    | UInt32 -> 4
    | Int32 -> 5
    | Float32 -> 6
    | Float64 -> 7

  let to_int_opt = function
    | None -> 0
    | Some x -> to_int x

  let of_int = function
    | 1 -> `byte
    | 2 -> `uint16
    | 3 -> `int16
    | 4 -> `uint32
    | 5 -> `int32
    | 6 -> `float32
    | 7 -> `float64
    | 0 -> `unknown
    | _ -> `unhandled

  let is_matching_int kind i =
    to_int kind = i

  let to_ba_kind : type v e. (v, e) t -> (v, e) Bigarray.kind = function
    | Byte -> Bigarray.int8_unsigned
    | UInt16 -> Bigarray.int16_unsigned
    | Int16 -> Bigarray.int16_signed
    | UInt32 -> Bigarray.int32
    | Int32 -> Bigarray.int32
    | Float32 -> Bigarray.float32
    | Float64 -> Bigarray.float64

  let byte = Byte
  let uint16 = UInt16
  let int16 = Int16
  let uint32 = UInt32
  let int32 = Int32
  let float32 = Float32
  let float64 = Float64
end

exception IO_error
exception Invalid_dimensions

let err = T.err IO_error

let get_x_size =
  Lib.c "GDALGetRasterBandXSize"
    (t @-> returning int)

let get_x_size (t, _) = get_x_size t

let get_y_size =
  Lib.c "GDALGetRasterBandYSize"
    (t @-> returning int)

let get_y_size (t, _) = get_y_size t

let get_size t =
  get_x_size t, get_y_size t

let get_data_type =
  Lib.c "GDALGetRasterDataType"
    (t @-> returning int)

let check_data_type c kind =
  Data.is_matching_int kind (get_data_type c)

let get_data_type c =
  get_data_type c
  |> Data.of_int

let to_ba_kind (_, kind) = kind

let get_band_number =
  Lib.c "GDALGetBandNumber"
    (t @-> returning int)

let get_band_number (t, _) =
  match get_band_number t with
  | 0 -> None
  | i -> Some i

let io =
  Lib.c "GDALRasterIO" (
    t @->
    int @->
    int @-> int @-> int @-> int @->
    ptr void @->
    int @-> int @->
    int @->
    int @-> int @->
    returning err
  )

let io
    ?(write : ('v, 'e, Bigarray.c_layout) Bigarray.Array2.t option)
    ?(offset = 0, 0)
    ?size
    ?(pixel_spacing = 0)
    ?(line_spacing = 0)
    ?buffer_size
    ((c, _) as t)
    (kind : ('v, 'e) Data.t)
  =
  let (x_size, y_size) as size =
    match size with
    | None -> get_size t
    | Some s -> s
  in
  let buffer_x, buffer_y =
    let bx, by =
      match buffer_size with
      | None -> size
      | Some s -> s
    in
    match write with
    | None -> bx, by
    | Some a ->
      if
        bx = Bigarray.Array2.dim1 a &&
        by = Bigarray.Array2.dim2 a
      then
        bx, by
      else
        raise Invalid_dimensions
  in
  let ba =
    match write with
    | None ->
      let open Bigarray in
      Array2.create (Data.to_ba_kind kind) c_layout buffer_x buffer_y
    | Some buffer -> buffer
  in
  io
    c
    (if write = None then 0 else 1)
    (fst offset)
    (snd offset)
    (fst size)
    (snd size)
    (bigarray_start array2 ba |> to_voidp)
    buffer_x
    buffer_y
    (Data.to_int kind)
    pixel_spacing
    line_spacing;
  ba

let read ?offset ?size ?pixel_spacing ?line_spacing ?buffer_size t kind =
  io ?offset ?size ?pixel_spacing ?line_spacing ?buffer_size t kind

let write ?offset ?size ?pixel_spacing ?line_spacing t kind data =
  ignore (io ~write:data ?offset ?size ?pixel_spacing ?line_spacing t kind)

let get_description =
  Lib.c "GDALGetDescription"
    (t @-> returning string)

let get_description (t, _) = get_description t

let set_description =
  Lib.c "GDALSetDescription"
    (t @-> string @-> returning void)

let set_description (t, _) s = set_description t s

module Block = struct
  let get_size =
    Lib.c "GDALGetBlockSize"
      (t @-> ptr int @-> ptr int @-> returning void)

  let get_size t =
    let i = allocate int 0 in
    let j = allocate int 0 in
    get_size t i j;
    !@i, !@j
end
