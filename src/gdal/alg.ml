module Oarray = Array
open Ctypes
open Foreign
module Carray = Array
module Array = Oarray

type t = T.t
let t = T.t

exception Algorithm_error

let err = T.err Algorithm_error

let proximity =
  Lib.c "GDALComputeProximity"
    (Band.t @-> Band.t @-> ptr void @-> ptr void @-> ptr void @-> returning err)

let proximity ?(options = []) ~src ~test =
  let options = Lib.convert_creation_options options in
  proximity src test options null null

let fill_nodata =
  Lib.c "GDALFillNoData" (
    Band.t @-> Band.t @->
    double @-> int @-> int @->
    ptr void @->
    ptr void @-> ptr void @->
    returning err
  )

let fill_nodata ~target ~mask search_distance smoothing_iterations options =
  fill_nodata target mask search_distance 0 smoothing_iterations
    (Lib.convert_creation_options options) null null

type interpolate_t = {
  algorithm : int;
  options : unit ptr;
}

let inverse_distance_to_a_power
    ~power
    ~smoothing
    ~anisotropy_ratio
    ~anisotropy_angle
    ~radius
    ~angle
    ~points
    ~no_data_value
  =
  let options = structure "GDALGridInverseDistanceToAPowerOptions" in
  let add t l = field options l t in
  let dfPower = add double "dfPower" in
  let dfSmoothing = add double "dfSmoothing" in
  let dfAnisotropyRatio = add double "dfAnisotropyRatio" in
  let dfAnisotropyAngle = add double "dfAnisotropyAngle" in
  let dfRadius1 = add double "dfRadius1" in
  let dfRadius2 = add double "dfRadius2" in
  let dfAngle = add double "dfAngle" in
  let nMaxPoints = add uint32_t "nMaxPoints" in
  let nMinPoints = add uint32_t "nMinPoints" in
  let dfNoDataValue = add double "dfNoDataValue" in
  seal (options : interpolate_t structure typ);
  let o = make options in
  setf o dfPower power;
  setf o dfSmoothing smoothing;
  setf o dfAnisotropyRatio anisotropy_ratio;
  setf o dfAnisotropyAngle anisotropy_angle;
  setf o dfRadius1 (fst radius);
  setf o dfRadius2 (snd radius);
  setf o dfAngle angle;
  setf o nMaxPoints (Unsigned.UInt32.of_int (snd points));
  setf o nMinPoints (Unsigned.UInt32.of_int (fst points));
  setf o dfNoDataValue no_data_value;
  { algorithm = 1; options = to_voidp (addr o) }

let moving_average
    ~radius
    ~angle
    ~min_points
    ~no_data_value
  =
  let options = structure "GDALGridMovingAverageOptions" in
  let add t l = field options l t in
  let dfRadius1 = add double "dfRadius1" in
  let dfRadius2 = add double "dfRadius2" in
  let dfAngle = add double "dfAngle" in
  let nMinPoints = add uint32_t "nMinPoints" in
  let dfNoDataValue = add double "dfNoDataValue" in
  seal (options : interpolate_t structure typ);
  let o = make options in
  setf o dfRadius1 (fst radius);
  setf o dfRadius2 (snd radius);
  setf o dfAngle angle;
  setf o nMinPoints (Unsigned.UInt32.of_int min_points);
  setf o dfNoDataValue no_data_value;
  { algorithm = 2; options = to_voidp (addr o) }

let nearest_neighbor
    ~radius
    ~angle
    ~no_data_value
  =
  let options = structure "GDALGridNearestNeighborOptions" in
  let add t l = field options l t in
  let dfRadius1 = add double "dfRadius1" in
  let dfRadius2 = add double "dfRadius2" in
  let dfAngle = add double "dfAngle" in
  let dfNoDataValue = add double "dfNoDataValue" in
  seal (options : interpolate_t structure typ);
  let o = make options in
  setf o dfRadius1 (fst radius);
  setf o dfRadius2 (snd radius);
  setf o dfAngle angle;
  setf o dfNoDataValue no_data_value;
  { algorithm = 3; options = to_voidp (addr o) }

type metric_t =
  radius:float * float ->
  angle:float ->
  min_points:int ->
  no_data_value:float ->
  interpolate_t

let metric algorithm
    ~radius
    ~angle
    ~min_points
    ~no_data_value
  =
  let options = structure "GDALGridDataMetricsOptions" in
  let add t l = field options l t in
  let dfRadius1 = add double "dfRadius1" in
  let dfRadius2 = add double "dfRadius2" in
  let dfAngle = add double "dfAngle" in
  let nMinPoints = add uint32_t "nMinPoints" in
  let dfNoDataValue = add double "dfNoDataValue" in
  seal (options : interpolate_t structure typ);
  let o = make options in
  setf o dfRadius1 (fst radius);
  setf o dfRadius2 (snd radius);
  setf o dfAngle angle;
  setf o nMinPoints (Unsigned.UInt32.of_int min_points);
  setf o dfNoDataValue no_data_value;
  { algorithm; options = to_voidp (addr o) }

let metric_minimum = metric 4
let metric_maximum = metric 5
let metric_range = metric 6
let metric_count = metric 7
let metric_average_distance = metric 8
let metric_average_distance_points = metric 9

let grid_create =
  Lib.c "GDALGridCreate" (
    int @-> ptr void @->
    uint32_t @-> ptr double @-> ptr double @-> ptr double @->
    double @-> double @-> double @-> double @->
    uint32_t @-> uint32_t @->
    int @->
    ptr void @->
    ptr void @-> ptr void @->
    returning err
  )

let of_list l =
  Carray.of_list double l
  |> Carray.start

let grid_create
    interpolation points
    ~xrange:(nx, xmin, xmax)
    ~yrange:(ny, ymin, ymax) data_type =
  let xs, ys, zs, npts =
    List.fold_left (
      fun (xs, ys, zs, npts) (x, y, z) ->
        x :: xs, y :: ys, z :: zs, succ npts
    ) ([], [], [], 0) points
  in
  let t = Band.Data.to_element_t data_type in
  let c_array = Carray.make t (nx * ny) in
  grid_create
    interpolation.algorithm interpolation.options
    (Unsigned.UInt32.of_int npts) (of_list xs) (of_list ys) (of_list zs)
    xmin xmax ymin ymax
    (Unsigned.UInt32.of_int nx) (Unsigned.UInt32.of_int ny)
    (Band.Data.to_int data_type)
    (c_array |> Carray.start |> to_voidp)
    null null;
  Carray.to_list c_array
  |> Array.of_list