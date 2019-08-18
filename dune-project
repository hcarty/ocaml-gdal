(lang dune 1.11)
(using fmt 1.2 (enabled_for dune))
(generate_opam_files true)
(name gdal)

(source (github hcarty/ocaml-gdal))
(license MIT)
(authors "Hezekiah M. Carty")
(maintainers "hez@0ok.org")

(package
 (name gdal)
 (synopsis "Bindings to the GDAL geospatial data library")
 (depends
  (ocaml (>= 4.03))
  (ctypes (>= 0.4.0))
  (ctypes-foreign (>= 0.4.0))
))

