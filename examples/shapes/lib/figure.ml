(* Figures built from Geo's vectors: no interface, so everything is exported. *)

(** A figure on the plane. *)
type t =
  | Dot of Geo.vec
  | Segment of Geo.vec * Geo.vec
  | Arc of Geo.vec * Geo.Angle.t

let length (f : t) : Geo.length =
  match f with
  | Dot _ -> Geo.mm 0
  | Segment (a, b) -> Geo.norm1 { x = b.x - a.x; y = b.y - a.y }
  | Arc (_, a) -> Geo.mm (Geo.Angle.to_deg a)

let shift (d : Geo.vec) (f : t) : t =
  match f with
  | Dot p -> Dot (Geo.add p d)
  | Segment (a, b) -> Segment (Geo.add a d, Geo.add b d)
  | Arc (c, a) -> Arc (Geo.add c d, a)

let rotate ~(by : Geo.Angle.t) (f : t) : t =
  match f with
  | Arc (c, _) -> Arc (c, Geo.Angle.flip by)
  | Dot _ | Segment _ -> f

let describe ?(unit_name = "mm") (f : t) : string =
  string_of_int (Geo.to_mm (length f)) ^ unit_name

let unit_dot : t = Dot { x = 0; y = 0 }

(* not written yet *)
let outline (_ : t list) : (Geo.vec * Geo.vec) option = assert false

let test_shift_dot () = assert (shift { x = 1; y = 2 } unit_dot = Dot { x = 1; y = 2 })
let test_length_of_dot () = assert (Geo.to_mm (length unit_dot) = 0)
