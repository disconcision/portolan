/* CanvasRim — see the interface. */
let gap = 110.; /* from the board's edge to a column */
let step = 78.; /* between two nodes of a column */

let place =
    (
      rim: list(CanvasGraph.rim),
      placed: list((string, (float, float), float)),
    )
    : list((string, (float, float))) => {
  let side = k =>
    List.find_opt((r: CanvasGraph.rim) => r.r_key == k, rim)
    |> Option.map((r: CanvasGraph.rim) => r.r_side);
  let inner = List.filter(((k, _, _)) => side(k) == None, placed);
  let on = s => List.filter(((k, _, _)) => side(k) == Some(s), placed);
  switch (inner) {
  | [] => []
  | [(_, (x0, y0), r0), ...rest] =>
    let (l, r, t, b) =
      List.fold_left(
        ((l, r, t, b), (_, (x, y), rad)) =>
          (
            min(l, x -. rad),
            max(r, x +. rad),
            min(t, y -. rad),
            max(b, y +. rad),
          ),
        (x0 -. r0, x0 +. r0, y0 -. r0, y0 +. r0),
        rest,
      );
    let column = (x, nodes) => {
      let nodes =
        List.stable_sort(
          ((_, (_, a), _), (_, (_, b), _)) => compare(a, b),
          nodes,
        );
      let top =
        (t +. b) /. 2. -. step *. float_of_int(List.length(nodes) - 1) /. 2.;
      List.mapi(
        (i, (k, _, _)) => (k, (x, top +. step *. float_of_int(i))),
        nodes,
      );
    };
    column(l -. gap, on(CanvasGraph.Users))
    @ column(r +. gap, on(CanvasGraph.Used));
  };
};
