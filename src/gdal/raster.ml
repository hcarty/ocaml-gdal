module Oarray = Array
open Ctypes
open Foreign
module Carray = Array
module Array = Oarray

type t = T.t
let t = T.t

type _ data_t =
  | Int : int data_t
  | Float : float data_t

let int_of_data_t : type s. s data_t -> int = function
  | Int -> 5
  | Float -> 7

let element_t_of_data_t : type s. s data_t -> s typ = function
  | Int -> int
  | Float -> double

exception IO_error

let err = T.err IO_error

let get_x_size =
  Lib.c "GDALGetRasterBandXSize"
    (t @-> returning int)

let get_y_size =
  Lib.c "GDALGetRasterBandYSize"
    (t @-> returning int)

let get_size t =
  get_x_size t, get_y_size t

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
    ?(write : 'a std_array option)
    ?(offset = 0, 0)
    ?size
    ?(pixel_spacing = 0)
    ?(line_spacing = 0)
    ?buffer_size
    t
    (kind : 'a data_t)
  =
  let (x_size, y_size) as size =
    match size with
    | None -> get_size t
    | Some s -> s
  in
  let buffer_x, buffer_y =
    match buffer_size with
    | None -> size
    | Some s -> s
  in
  let c_buffer =
    match write with
    | None ->
      Carray.make (element_t_of_data_t kind) (buffer_x * buffer_y)
    | Some buffer ->
      Array.to_list buffer
      |> Carray.of_list (element_t_of_data_t kind)
  in
  io
    t
    (if write = None then 0 else 1)
    (fst offset)
    (snd offset)
    (fst size)
    (snd size)
    (to_voidp (Carray.start c_buffer))
    buffer_x
    buffer_y
    (int_of_data_t kind)
    pixel_spacing
    line_spacing;
  c_buffer

let read t kind =
  let c_array = io t kind in
  Carray.to_list c_array
  |> Array.of_list

let write t kind data =
  ignore (io ~write:data t kind)

let read_int t = read t Int
let read_float t = read t Float

let write_int t data = write t Int data
let write_float t data = write t Float data

module Block = struct
  let get_size =
    Lib.c "GDALGetBlockSize"
      (t @-> ptr int @-> ptr int @-> returning void)

  let get_size t =
    let i = allocate int 0 in
    let j = allocate int 0 in
    get_size t i j;
    !@i, !@j

  let read_int t (i, j) = assert false
  let read_float t (i, j) = assert false

  let write_int t (i, j) data = assert false
  let write_float t (i, j) data = assert false
end

let int = Int
let float = Float
