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

let fill =
  Lib.c "GDALFillRaster"
    (t @-> double @-> double @-> returning err)

let fill ?(imaginary = 0.0) (t, _) real =
  fill t real imaginary

let get_description =
  Lib.c "GDALGetDescription"
    (t @-> returning string)

let get_description (t, _) = get_description t

let get_no_data_value =
  Lib.c "GDALGetRasterNoDataValue"
    (t @-> ptr int @-> returning double)

let get_no_data_value (t, _) =
  let ok = allocate int 0 in
  let result = get_no_data_value t ok in
  if to_voidp ok = null || !@ok = 1 then
    Some result
  else
    None

let set_description =
  Lib.c "GDALSetDescription"
    (t @-> string @-> returning void)

let set_description (t, _) s = set_description t s

let set_no_data_value =
  Lib.c "GDALSetRasterNoDataValue"
    (t @-> double @-> returning err)

let set_no_data_value (t, _) x =
  set_no_data_value t x

module Block = struct
  exception Wrong_dimensions

  let get_band_size = get_size

  let get_size =
    Lib.c "GDALGetBlockSize"
      (t @-> ptr int @-> ptr int @-> returning void)

  let get_size (t, _) =
    let i = allocate int 0 in
    let j = allocate int 0 in
    get_size t i j;
    !@i, !@j

  let get_block_count t =
    let bandx, bandy = get_band_size t in
    let blockx, blocky = get_size t in
    (bandx + blockx - 1) / blockx,
    (bandy + blocky - 1) / blocky

  let read =
    Lib.c "GDALReadBlock"
      (t @-> int @-> int @-> ptr void @-> returning err)

  let read ?data ((c, k) as t) ~i ~j =
    let nx, ny = get_size t in
    let ba =
      match data with
      | None -> Bigarray.(Array2.create k c_layout nx ny)
      | Some ba ->
        if nx * ny <= Bigarray.Array2.dim1 ba * Bigarray.Array2.dim2 ba then
          ba
        else
          raise Wrong_dimensions
    in
    read c i j (bigarray_start array2 ba |> to_voidp);
    ba

  let write =
    Lib.c "GDALWriteBlock"
      (t @-> int @-> int @-> ptr void @-> returning err)

  let write (t, _) ~i ~j data =
    write t i j (bigarray_start array2 data |> to_voidp)

  let read' = read
  let write' = write

  let iter ((_c, k) as t) ~read ~write f =
    let ni, nj = get_block_count t in
    let nx, ny = get_size t in
    let bandx, bandy = get_band_size t in
    let data = Bigarray.(Array2.create k c_layout nx ny) in
    for i = 0 to ni - 1 do
      for j = 0 to nj - 1 do
        let data =
          if read then read' ~data t ~i ~j
          else data
        in
        let valid, valid_x, valid_y =
          if
            (i + 1) * nx > bandx ||
            (j + 1) * ny > bandy
          then (
            (* Only pass valid data to f *)
            let valid_x = bandx - i * nx in
            let valid_y = bandy - j * ny in
            let valid =
              Bigarray.Array2.create k Bigarray.c_layout valid_x valid_y
            in
            begin
              if read then (
                for si = 0 to valid_x - 1 do
                  for sj = 0 to valid_y - 1 do
                    valid.{si, sj} <- data.{si, sj}
                  done;
                done;
              )
              else (
              )
            end;
            valid, valid_x, valid_y
          )
          else
            data, nx, ny
        in
        f i j valid;
        if write then (
          (* If we created a fresh bigarray copy the values back to data for
             writing. Physical equality is intentional here. *)
          if valid != data then (
            for si = 0 to valid_x - 1 do
              for sj = 0 to valid_y - 1 do
                data.{si, sj} <- valid.{si, sj}
              done;
            done;
          );
          write' t ~i ~j data;
        );
      done;
    done;
    ()

  let iter_read t f = iter ~read:true ~write:false t f
  let iter_write t f = iter ~read:false ~write:true t f
end

let iter t f =
  let block_x_size, block_y_size = Block.get_size t in
  Block.iter t ~read:true ~write:true (
    fun block_i block_j data ->
      let open Bigarray in
      let ni = Array2.dim1 data in
      let nj = Array2.dim2 data in
      for data_i = 0 to ni - 1 do
        for data_j = 0 to nj - 1 do
          let i = block_i * block_x_size + data_i in
          let j = block_j * block_y_size + data_j in
          let v = data.{data_i, data_j} in
          let result = f i j v in
          data.{data_i, data_j} <- result;
        done;
      done;
      ()
  )

let iter_read t f =
  let block_x_size, block_y_size = Block.get_size t in
  Block.iter t ~read:true ~write:false (
    fun block_i block_j data ->
      let open Bigarray in
      let ni = Array2.dim1 data in
      let nj = Array2.dim2 data in
      for data_i = 0 to ni - 1 do
        for data_j = 0 to nj - 1 do
          let i = block_i * block_x_size + data_i in
          let j = block_j * block_y_size + data_j in
          let v = data.{data_i, data_j} in
          f i j v;
        done;
      done;
      ()
  )

let iter_write t f =
  let block_x_size, block_y_size = Block.get_size t in
  Block.iter t ~read:false ~write:true (
    fun block_i block_j data ->
      let open Bigarray in
      let ni = Array2.dim1 data in
      let nj = Array2.dim2 data in
      for data_i = 0 to ni - 1 do
        for data_j = 0 to nj - 1 do
          let i = block_i * block_x_size + data_i in
          let j = block_j * block_y_size + data_j in
          let result = f i j in
          data.{data_i, data_j} <- result;
        done;
      done;
      ()
  )

let fold t f init =
  let accu = ref init in
  iter_read t (
    fun i j v ->
      accu := f i j v !accu
  );
  !accu
