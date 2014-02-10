(* A simple sanity check against the OGR bindings *)

let p i = Printf.printf "%d\n%!" i

let parse_args () =
  try
    Sys.argv.(1)
  with
  | _ ->
    prerr_endline "You must provide a shapefile to load";
    exit 1

let () =
  let shapefile = parse_args () in
  Ogr.Lib.init_dynamic ();
  Ogr.Lib.register_all ();
  Ogr.Data_source.with_source shapefile (
    fun ds ->
      p 1;
      let layer = Ogr.Data_source.get_layer ds 0 in
      p 2;
      Ogr.Layer.reset_reading layer;
      p 3;
      Ogr.Layer.iter_features layer (
        fun feature ->
          p 4;
          let feature_defn = Ogr.Layer.get_layer_defn layer in
          p 5;
          let count = Ogr.Feature.Defn.get_field_count feature_defn in
          p 6;
          let field_defn = Ogr.Feature.Defn.get_field_defn feature_defn 0 in
          p 7;
          let field_type = Ogr.Field.get_type field_defn in
          p 8;
          for i = 0 to count - 1 do
            print_endline @@ Ogr.Feature.get_as_string feature i;
          done;
          p 9;
      );
  );
  p 10;
  ()
