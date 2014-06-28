open Ctypes

exception Warp_error

let err = T.err Warp_error

type resample_t =
  | Nearest_neighbor
  | Bilinear
  | Cubic
  | Cubic_spline
  | Lanczos

let int_of_resample = function
  | Nearest_neighbor -> 0
  | Bilinear -> 1
  | Cubic -> 2
  | Cubic_spline -> 3
  | Lanczos -> 4

module Options = struct
  type warp_options_t
  type t = warp_options_t structure ptr

  let t : warp_options_t structure typ = structure "GDALWarpOptions"
  let f n k = field t n k
  let warp_options = f "warp_options" (ptr string_opt)
  let memory_limit = f "memory_limit" double
  let resample_alg = f "resample_alg" int
  let data_type = f "data_type" int
  let src_ds = f "src_ds" Data_set.t
  let dst_ds = f "dst_ds" Data_set.t
  let band_count = f "band_count" int
  let src_bands = f "src_bands" (ptr int)
  let dst_bands = f "dst_bands" (ptr int)
  let src_alpha_band = f "src_alpha_band" int
  let dst_alpha_band = f "dst_alpha_band" int
  let src_no_data_real = f "src_no_data_real" (ptr_opt double)
  let src_no_data_imag = f "src_no_data_imag" (ptr_opt double)
  let dst_no_data_real = f "dst_no_data_real" (ptr_opt double)
  let dst_no_data_imag = f "dst_no_data_imag" (ptr_opt double)
  let progress = f "progress" (ptr void)
  let progress_arg = f "progress_arg" (ptr void)
  let transformer = f "transformer" (ptr void)
  let transformer_arg = f "transformer_arg" (ptr void)
  let src_per_band_validity_mask_func =
    f "src_per_band_validity_mask_func" (ptr void)
  let src_per_band_validity_mask_func_arg =
    f "src_per_band_validity_mask_func_arg" (ptr (ptr void))
  let src_validity_mask_func =
    f "src_validity_mask_func" (ptr void)
  let src_validity_mask_func_arg =
    f "src_validity_mask_func_arg" (ptr void)
  let src_density_mask_func =
    f "src_density_mask_func" (ptr void)
  let src_density_mask_func_arg =
    f "src_density_mask_func_arg" (ptr void)
  let dst_density_mask_func =
    f "dst_density_mask_func" (ptr void)
  let dst_density_mask_func_arg =
    f "dst_density_mask_func_arg" (ptr void)
  let dst_validity_mask_func =
    f "dst_validity_mask_func" (ptr void)
  let dst_validity_mask_func_arg =
    f "dst_validity_mask_func_arg" (ptr void)
  let pre_warp_chunk_processor =
    f "pre_warp_chunk_processor" (ptr void)
  let pre_warp_chunk_processor_arg =
    f "pre_warp_chunk_processor_arg" (ptr void)
  let post_warp_chunk_processor =
    f "post_warp_chunk_processor" (ptr void)
  let post_warp_chunk_processor_arg =
    f "post_warp_chunk_processor_arg" (ptr void)
  let cutline = f "cutline" (ptr void)
  let cutline_blend_dist = f "cutline_blend_dist" double
  let () = seal t

  let create =
    Lib.c "GDALCreateWarpOptions"
      (void @-> returning (ptr t))

  let destroy =
    Lib.c "GDALDestroyWarpOptions"
      (ptr t @-> returning void)

  let create () =
    let o = create () in
    Gc.finalise destroy o;
    o

  let clone =
    Lib.c "GDALCloneWarpOptions"
      (ptr t @-> returning (ptr t))

  let clone t =
    let o = clone t in
    Gc.finalise destroy o;
    o

  let set_warp_options o options =
    let options = Lib.convert_creation_options options in
    setf !@o warp_options options;
    ()

  let set_memory_limit o l =
    setf !@o memory_limit l;
    ()

  let set_resample_alg o a =
    let i = int_of_resample a in
    setf !@o resample_alg i;
    ()

  let set_working_data_type o t =
    let i = Band.Data.to_int t in
    setf !@o data_type i;
    ()

  let set_src o ds =
    setf !@o src_ds ds;
    ()

  let set_dst o ds =
    setf !@o dst_ds ds;
    ()

  let set_bands o bands =
    let src = List.map fst bands in
    let dst = List.map snd bands in
    let src = CArray.of_list int src in
    let dst = CArray.of_list int dst in
    let n = CArray.length src in
    setf !@o band_count n;
    setf !@o src_bands (CArray.start src);
    setf !@o dst_bands (CArray.start dst);
    ()

  let ptr_opt_of_option typ = function
    | Some x -> Some (allocate typ x)
    | None -> None

  let set_src_no_data o ~real ~imag =
    let real = ptr_opt_of_option double real in
    let imag = ptr_opt_of_option double imag in
    setf !@o src_no_data_real real;
    setf !@o src_no_data_imag imag;
    ()

  let set_dst_no_data o ~real ~imag =
    let real = ptr_opt_of_option double real in
    let imag = ptr_opt_of_option double imag in
    setf !@o dst_no_data_real real;
    setf !@o dst_no_data_imag imag;
    ()
end

let reproject_image =
  Lib.c "GDALReprojectImage"
    (Data_set.t @-> string_opt @-> Data_set.t @-> string_opt @->
     int @-> double @-> double @->
     ptr void @-> ptr void @->
     ptr Options.t @-> returning err)

let reproject_image ?(memory_limit = 0.0) ?(max_error = 0.0)
    ?(options = from_voidp Options.t null)
    ?src_wkt ?dst_wkt ~src ~dst alg =
  reproject_image src src_wkt dst dst_wkt (int_of_resample alg)
    memory_limit max_error null null options

let create_and_reproject_image =
  Lib.c "GDALCreateAndReprojectImage"
    (Data_set.t @-> string_opt @-> string @-> string_opt @-> Driver.t @->
     ptr string_opt @-> int @-> double @-> double @->
     ptr void @-> ptr void @-> ptr Options.t @-> returning err)

let create_and_reproject_image
    ?(memory_limit = 0.0) ?(max_error = 0.0)
    ?(options = from_voidp Options.t null)
    ?src_wkt ?dst_wkt
    ?(create_options = [])
    src ~filename driver alg =
  let create_options = Lib.convert_creation_options create_options in
  create_and_reproject_image src src_wkt filename dst_wkt driver create_options
    (int_of_resample alg) memory_limit max_error null null options

let auto_create_warped_vrt =
  Lib.c "GDALAutoCreateWarpedVRT"
    (Data_set.t @-> string_opt @-> string_opt @-> int @-> float @->
     ptr Options.t @-> returning Data_set.t_opt)

let auto_create_warped_vrt ?src_wkt ?dst_wkt ?(max_error = 0.0)
    ?(options = from_voidp Options.t null)
    src alg =
  let result =
    auto_create_warped_vrt src src_wkt dst_wkt (int_of_resample alg) max_error
      options
  in
  match result with
  | Some ds -> ds
  | None -> raise Warp_error
