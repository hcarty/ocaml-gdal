open Ctypes

module Defn = struct
  type t = unit ptr
  let t : t typ = ptr void
end
type t = unit ptr
let t : t typ = ptr void
let t_opt : t option typ = ptr_opt void

type envelope
let envelope : envelope structure typ = structure "envelope"
let envelope_minx = field envelope "MinX" double
let envelope_maxx = field envelope "MaxX" double
let envelope_miny = field envelope "MinY" double
let envelope_maxy = field envelope "MaxY" double
let () = seal envelope
