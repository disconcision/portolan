(* A small vector-graphics scene model: the Hazel demo prompt, by hand. *)

type point = int * int

(** A shape is a sum whose variants carry tuple payloads. *)
type shape =
  | Circle of point * int
  | Rect of point * point
  | Line of point * point

type scene = shape list

(** Twice the area, so everything stays in the integers. *)
let area (s : shape) : int =
  match s with
  | Circle (_, r) -> 3 * r * r
  | Rect ((x0, y0), (x1, y1)) -> abs (x1 - x0) * abs (y1 - y0)
  | Line _ -> 0

let%test "area of a rect" = area (Rect ((0, 0), (2, 3))) = 6
let%test "area of a line" = area (Line ((0, 0), (5, 5))) = 0

let translate ((dx, dy) : point) (s : shape) : shape =
  let move (x, y) = (x + dx, y + dy) in
  match s with
  | Circle (c, r) -> Circle (move c, r)
  | Rect (a, b) -> Rect (move a, move b)
  | Line (a, b) -> Line (move a, move b)

let%test "translate moves a circle" =
  translate (1, 1) (Circle ((0, 0), 2)) = Circle ((1, 1), 2)

let scale (k : int) (s : shape) : shape =
  let grow (x, y) = (k * x, k * y) in
  match s with
  | Circle (c, r) -> Circle (grow c, k * r)
  | Rect (a, b) -> Rect (grow a, grow b)
  | Line (a, b) -> Line (grow a, grow b)

let total_area (sc : scene) : int = List.fold_left (fun acc s -> acc + area s) 0 sc

let%test "total area adds up" =
  total_area [ Rect ((0, 0), (1, 1)); Rect ((0, 0), (2, 2)) ] = 5

let move_all (d : point) (sc : scene) : scene = List.map (translate d) sc

(* not written yet *)
let bounding_box (_ : scene) : point * point = failwith "TODO"

let origin : point = (0, 0)
