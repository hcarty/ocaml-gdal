open Ctypes

type t = unit ptr
let t : t typ = ptr void
let t_opt : t option typ = ptr_opt void

let err exn =
  view
    ~read:(fun i -> if i = 0 then () else raise exn)
    ~write:(fun () -> 0)
    int
