open Ctypes

type t = T.t
let t = T.t

module Data = struct
  type _ t =
    | Byte : int t
    | UInt16 : int t
    | Int16 : int t
    | UInt32 : Unsigned.uint32 t
    | Int32 : int32 t
    | Float32 : float t
    | Float64 : float t

  let to_int : type s. s t -> int = function
    | Byte -> 1
    | UInt16 -> 2
    | Int16 -> 3
    | UInt32 -> 4
    | Int32 -> 5
    | Float32 -> 6
    | Float64 -> 7

  let to_element_t : type s. s t -> s typ = function
    | Byte -> int
    | UInt16 -> int
    | Int16 -> int
    | UInt32 -> uint32_t
    | Int32 -> int32_t
    | Float32 -> float
    | Float64 -> double

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

let get_y_size =
  Lib.c "GDALGetRasterBandYSize"
    (t @-> returning int)

let get_size t =
  get_x_size t, get_y_size t

let get_data_type =
  Lib.c "GDALGetRasterDataType"
    (t @-> returning int)

let get_data_type t =
  let open Data in
  match get_data_type t with
  | 1 -> `int Byte
  | 2 -> `int UInt16
  | 3 -> `int Int16
  | 4 -> `uint32 UInt32
  | 5 -> `int32 Int32
  | 6 -> `float Float32
  | 7 -> `float Float64
  | 0 -> `unknown
  | _ -> `unhandled

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
    ?(write : 'a array option)
    ?(offset = 0, 0)
    ?size
    ?(pixel_spacing = 0)
    ?(line_spacing = 0)
    ?buffer_size
    t
    (kind : 'a Data.t)
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
      if bx * by <= Array.length a then
        bx, by
      else
        raise Invalid_dimensions
  in
  let c_buffer =
    match write with
    | None ->
      CArray.make (Data.to_element_t kind) (buffer_x * buffer_y)
    | Some buffer ->
      Array.to_list buffer
      |> CArray.of_list (Data.to_element_t kind)
  in
  io
    t
    (if write = None then 0 else 1)
    (fst offset)
    (snd offset)
    (fst size)
    (snd size)
    (to_voidp (CArray.start c_buffer))
    buffer_x
    buffer_y
    (Data.to_int kind)
    pixel_spacing
    line_spacing;
  c_buffer

let read ?offset ?size ?pixel_spacing ?line_spacing ?buffer_size t kind =
  let c_array =
    io ?offset ?size ?pixel_spacing ?line_spacing ?buffer_size t kind
  in
  CArray.to_list c_array
  |> Array.of_list

let write ?offset ?size ?pixel_spacing ?line_spacing t kind data =
  ignore (io ~write:data ?offset ?size ?pixel_spacing ?line_spacing t kind)

let get_description =
  Lib.c "GDALGetDescription"
    (t @-> returning string)

let set_description =
  Lib.c "GDALSetDescription"
    (t @-> string @-> returning void)

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
