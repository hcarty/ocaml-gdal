OCaml GDAL and OGR bindings
---------------------------

This library provides access to the GDAL library (http://www.gdal.org/).  It
provides both direct, low-level access to GDAL and OGR library functions as
well as a higher level, more OCaml-like interface.

Using the bindings
------------------

The bindings are currently split into two libraries: ogr for working with
vector data (toplevel module is `Ogr`) and gdal for working with raster data
(toplevel module is `Gdal`).  Linking to the underlying GDAL library is
performed when the program runs.  `Ogr` and `Gdal` need to be initialized
separately.  To initialize:

    Gdal.Lib.init_dynamic ();
    Ogr.Lib.init_dynamic ();

Each `init_dynamic` function takes an optional `~lib` argument which may be
used to specify the specific shared object to link against.  It defaults to
`libgdal.so`.
