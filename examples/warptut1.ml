open Gdal

let () =
  Lib.init_dynamic ();
  Lib.register_all ();
  let src = Data_set.of_source_exn "in.tif" in
  let dst = Data_set.of_source_exn ~write:true "out.tif" in
  let warp_options = Warp.Options.create () in
  Warp.Options.set_src warp_options src;
  Warp.Options.set_dst warp_options dst;
  Warp.Options.set_bands warp_options [1, 1];
  (* TODO: Support and wrap progress reporting functions *)
  let transformer = Transform.make_gen_img @@ `data_set (src, dst) in
  Warp.Options.set_transformer warp_options transformer;
  let warp_operation = Warp.Operation.create warp_options in
  Warp.Operation.chunk_and_warp_image warp_operation
    ~offset:(0, 0) ~size:Data_set.(get_x_size dst, get_y_size dst);
  Data_set.close dst;
  Data_set.close src;
  ()
