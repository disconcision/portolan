/* CanvasOptions — the layouts and edge styles a reader can switch between.
   The lab's own (CanvasLayoutExperiments) are listed here too; the ones
   written for Portolan are below: three placements (compact, ledger,
   clusters) and two edge styles (metro, straight). Pure geometry over the
   graph record: CanvasLayout calls in at three marked places. */
module Lab = CanvasLayoutExperiments;
type point = Lab.point;

let layouts = [
  ("current", "layered"),
  ("compact", "compact"),
  ("ledger", "ledger"),
  ("clusters", "clusters"),
  ("flow", "function layers"),
  ("dependencies", "type layers"),
  ("packed", "packed groups"),
  ("grid", "grid"),
  ("stable", "stable growth"),
  ("relaxed", "soft anchors"),
];
let edge_styles = [
  ("curves", "curves"),
  ("metro", "metro"),
  ("straight", "straight"),
  ("circuit", "circuit"),
];

let layout = () => Lab.mode^;
let edges = () => Lab.wires^;
let set_layout = (k: string): bool =>
  if (List.mem_assoc(k, layouts)) {
    Lab.mode := k;
    Lab.reset();
    true;
  } else {
    false;
  };
let set_edges = (k: string): bool =>
  if (List.mem_assoc(k, edge_styles)) {
    Lab.wires := k;
    true;
  } else {
    false;
  };

let aspect: ref(float) = ref(1.6);
let grid = 14.;
let snap = (v: float): float => Float.round(v /. grid) *. grid;

/* ---------- source order ---------- */

/* definitions by (file, line); what has no place in the source comes
   after, in the record's order */
let source_order = (g: CanvasGraph.t): (string => int, string => int) => {
  let at: Hashtbl.t(string, (string, int)) = Hashtbl.create(64);
  List.iter(
    ((id, h: CanvasGraph.handle)) =>
      Hashtbl.replace(at, id, (h.file, h.line)),
    g.handles,
  );
  let where = (i, id) =>
    switch (Option.bind(id, Hashtbl.find_opt(at))) {
    | Some((f, l)) => (0, f, l, i)
    | None => (1, "", 0, i)
    };
  let defs =
    List.mapi(
      (i, e: CanvasGraph.edge) =>
        (where(i, Some(e.e_id)), "e:" ++ e.e_name),
      g.edges,
    )
    @ List.mapi(
        (i, n: CanvasGraph.tynode) => (where(i, n.n_id), "n:" ++ n.key),
        List.filter((n: CanvasGraph.tynode) => n.kind == Alias, g.nodes),
      )
    |> List.sort(compare);
  let rank: Hashtbl.t(string, int) = Hashtbl.create(64);
  List.iteri((i, (_, k)) => Hashtbl.replace(rank, k, i), defs);
  let far = List.length(defs);
  let edge_ord = name =>
    Option.value(~default=far, Hashtbl.find_opt(rank, "e:" ++ name));
  /* a node that is not a definition: where it is first used */
  let first_use: Hashtbl.t(string, int) = Hashtbl.create(64);
  List.iter(
    (e: CanvasGraph.edge) => {
      let o = edge_ord(e.e_name);
      List.iter(
        k =>
          switch (Hashtbl.find_opt(first_use, k)) {
          | Some(o') when o' <= o => ()
          | _ => Hashtbl.replace(first_use, k, o)
          },
        [e.e_src, e.dst],
      );
    },
    g.edges,
  );
  let index: Hashtbl.t(string, int) = Hashtbl.create(64);
  List.iteri(
    (i, n: CanvasGraph.tynode) => Hashtbl.replace(index, n.key, i),
    g.nodes,
  );
  let node_ord = key =>
    switch (Hashtbl.find_opt(rank, "n:" ++ key)) {
    | Some(o) => o
    | None =>
      switch (Hashtbl.find_opt(first_use, key)) {
      | Some(o) => o
      | None =>
        far + 1 + Option.value(~default=0, Hashtbl.find_opt(index, key))
      }
    };
  (node_ord, edge_ord);
};

/* ---------- filling a pane-shaped area with boxes ---------- */

/* Boxes (id, w, h) in the order given, stacked into columns left to right;
   the number of columns is the one whose outline is nearest the pane's
   shape. Returns each box's top-left corner. */
let columns =
    (~aspect: float, ~gap: float, boxes: list((string, float, float)))
    : list((string, float, float)) =>
  if (boxes == []) {
    [];
  } else {
    let total = List.fold_left((s, (_, _, h)) => s +. h +. gap, 0., boxes);
    let tallest = List.fold_left((s, (_, _, h)) => max(s, h), 0., boxes);
    let fill = (limit: float) => {
      let (cols, cur, _) =
        List.fold_left(
          ((cols, cur, y), (id, w, h)) =>
            if (cur != [] && y +. h > limit) {
              (cols @ [cur], [(id, w, h, 0.)], h +. gap);
            } else {
              (cols, cur @ [(id, w, h, y)], y +. h +. gap);
            },
          ([], [], 0.),
          boxes,
        );
      let cols = cols @ [cur];
      let (out, width, height) =
        List.fold_left(
          ((out, x, height), col) => {
            let w =
              List.fold_left((s, (_, w, _, _)) => max(s, w), 0., col);
            let h =
              List.fold_left((s, (_, _, h, y)) => max(s, y +. h), 0., col);
            (
              out @ List.map(((id, _, _, y)) => (id, x, y), col),
              x +. w +. gap,
              max(height, h),
            );
          },
          ([], 0., 0.),
          cols,
        );
      (out, width -. gap, height);
    };
    let tries = min(List.length(boxes), 24);
    let (best, _) =
      List.fold_left(
        ((best, score), k) => {
          let (out, w, h) =
            fill(max(tallest, total /. float_of_int(k)) +. 1.);
          let s = abs_float(log(max(1., w) /. max(1., h) /. aspect));
          s < score ? (out, s) : (best, score);
        },
        ([], infinity),
        List.init(tries, i => i + 1),
      );
    best;
  };

/* the same, in rows that wrap */
let shelves = (~aspect, ~gap, boxes) =>
  columns(
    ~aspect=1. /. aspect,
    ~gap,
    List.map(((id, w, h)) => (id, h, w), boxes),
  )
  |> List.map(((id, y, x)) => (id, x, y));

/* ---------- the default's packing, without the list searches ---------- */

/* `Lab.place("components", items, links, [])`, to the same result: a host
   and what docks to it move as one unit, and the connected pieces are
   shelf-packed. The lab finds roots, groups and members by searching lists
   by key for every node, which at a thousand nodes was nearly all of a
   layout's time; this keeps tables. The arithmetic is the lab's. */
let components =
    (items: list(Lab.item), links: list((string, string))): list(Lab.item) => {
  let here: Hashtbl.t(string, Lab.item) = Hashtbl.create(256);
  List.iter(
    (n: Lab.item) =>
      if (!Hashtbl.mem(here, n.key)) {
        Hashtbl.add(here, n.key, n);
      },
    items,
  );
  let rec root = (seen, n: Lab.item) =>
    switch (n.anchor) {
    | Some(k) when !List.mem(k, seen) =>
      switch (Hashtbl.find_opt(here, k)) {
      | Some(host) => root([n.key, ...seen], host)
      | None => n.key
      }
    | _ => n.key
    };
  let root_of: Hashtbl.t(string, string) = Hashtbl.create(256);
  List.iter(
    (n: Lab.item) =>
      if (!Hashtbl.mem(root_of, n.key)) {
        Hashtbl.add(root_of, n.key, root([], n));
      },
    items,
  );
  let members: Hashtbl.t(string, list(Lab.item)) = Hashtbl.create(256);
  List.iter(
    (n: Lab.item) => {
      let r = root([], n);
      Hashtbl.replace(
        members,
        r,
        [n, ...Option.value(~default=[], Hashtbl.find_opt(members, r))],
      );
    },
    List.rev(items),
  );
  let units =
    List.filter_map(
      (host: Lab.item) =>
        if (root([], host) != host.key) {
          None;
        } else {
          let (left, top, right, bottom) =
            List.fold_left(
              ((left, top, right, bottom), n: Lab.item) =>
                (
                  min(left, n.p.x -. n.w /. 2.),
                  min(top, n.p.y -. n.h /. 2.),
                  max(right, n.p.x +. n.w /. 2.),
                  max(bottom, n.p.y +. n.h /. 2.),
                ),
              (infinity, infinity, neg_infinity, neg_infinity),
              Option.value(~default=[], Hashtbl.find_opt(members, host.key)),
            );
          Some({
            ...host,
            p: {
              x: (left +. right) /. 2.,
              y: (top +. bottom) /. 2.,
            },
            w: right -. left,
            h: bottom -. top,
            anchor: None,
          });
        },
      items,
    );
  let links =
    Lab.links_of(items, links)
    |> List.filter_map(((a, b)) =>
         switch (Hashtbl.find_opt(root_of, a), Hashtbl.find_opt(root_of, b)) {
         | (Some(a), Some(b)) when a != b => Some((a, b))
         | _ => None
         }
       )
    |> List.sort_uniq(compare);
  /* the units' connected pieces, each in the units' order, the pieces in
     the order of their first unit */
  let piece: Hashtbl.t(string, string) = Hashtbl.create(256);
  let rec find = k =>
    switch (Hashtbl.find_opt(piece, k)) {
    | Some(p) when p != k =>
      let r = find(p);
      Hashtbl.replace(piece, k, r);
      r;
    | _ => k
    };
  let is_unit: Hashtbl.t(string, unit) = Hashtbl.create(256);
  List.iter((n: Lab.item) => Hashtbl.replace(is_unit, n.key, ()), units);
  List.iter(
    ((a, b)) =>
      if (Hashtbl.mem(is_unit, a) && Hashtbl.mem(is_unit, b)) {
        let ra = find(a)
        and rb = find(b);
        if (ra != rb) {
          Hashtbl.replace(piece, ra, rb);
        };
      },
    links,
  );
  let in_piece: Hashtbl.t(string, list(Lab.item)) = Hashtbl.create(64);
  List.iter(
    (n: Lab.item) => {
      let r = find(n.key);
      Hashtbl.replace(
        in_piece,
        r,
        [n, ...Option.value(~default=[], Hashtbl.find_opt(in_piece, r))],
      );
    },
    List.rev(units),
  );
  let seen: Hashtbl.t(string, unit) = Hashtbl.create(64);
  let pieces =
    List.filter_map(
      (n: Lab.item) => {
        let r = find(n.key);
        if (Hashtbl.mem(seen, r)) {
          None;
        } else {
          Hashtbl.replace(seen, r, ());
          Hashtbl.find_opt(in_piece, r);
        };
      },
      units,
    );
  /* from here on: Lab.packed, per piece */
  let blocks =
    List.map(
      (ns: list(Lab.item)) => {
        let minx =
          List.fold_left(
            (x, n: Lab.item) => min(x, n.p.x -. n.w /. 2.),
            infinity,
            ns,
          );
        let intervals =
          List.map(
            (n: Lab.item) => (n.p.y -. n.h /. 2., n.p.y +. n.h /. 2.),
            ns,
          )
          |> List.sort(compare);
        let (_, gaps) =
          List.fold_left(
            ((end_y, gaps), (lo, hi)) =>
              (
                max(end_y, hi),
                lo -. end_y > 100. && end_y != neg_infinity
                  ? gaps @ [(lo, lo -. end_y -. 100.)] : gaps,
              ),
            (neg_infinity, []),
            intervals,
          );
        let miny =
          List.fold_left(
            (v, n: Lab.item) => min(v, n.p.y -. n.h /. 2.),
            infinity,
            ns,
          );
        let ns =
          List.map(
            (n: Lab.item) =>
              {
                ...n,
                p: {
                  x: n.p.x -. minx,
                  y:
                    n.p.y
                    -. miny
                    -. List.fold_left(
                         (v, (at, amount)) => n.p.y >= at ? v +. amount : v,
                         0.,
                         gaps,
                       ),
                },
              },
            ns,
          );
        let w =
          List.fold_left(
            (v, n: Lab.item) => max(v, n.p.x +. n.w /. 2.),
            0.,
            ns,
          );
        let h =
          List.fold_left(
            (v, n: Lab.item) => max(v, n.p.y +. n.h /. 2.),
            0.,
            ns,
          );
        (ns, w, h);
      },
      pieces,
    );
  let area =
    List.fold_left(
      (a, (_, w, h)) => a +. (w +. 60.) *. (h +. 60.),
      0.,
      blocks,
    );
  let target = max(650., sqrt(area *. 1.5));
  let after: Hashtbl.t(string, point) = Hashtbl.create(256);
  let _ =
    List.fold_left(
      ((x, y, rowh), (ns, w, h)) => {
        let (x, y, rowh) =
          x > 70. && x +. w > target
            ? (70., y +. rowh +. 70., 0.) : (x, y, rowh);
        List.iter(
          (n: Lab.item) =>
            Hashtbl.replace(
              after,
              n.key,
              Lab.{
                x: Lab.snap(n.p.x +. x),
                y: Lab.snap(n.p.y +. y),
              },
            ),
          ns,
        );
        (x +. w +. 70., y, max(rowh, h));
      },
      (70., 70., 0.),
      blocks,
    );
  let before: Hashtbl.t(string, point) = Hashtbl.create(256);
  List.iter((n: Lab.item) => Hashtbl.replace(before, n.key, n.p), units);
  List.map(
    (n: Lab.item) => {
      let k = root([], n);
      switch (Hashtbl.find_opt(before, k), Hashtbl.find_opt(after, k)) {
      | (Some(b), Some(a)) => {
          ...n,
          p: {
            x: n.p.x +. a.x -. b.x,
            y: n.p.y +. a.y -. b.y,
          },
        }
      | _ => n
      };
    },
    items,
  );
};

/* ---------- compact: the layered arrangement, packed ---------- */

let arrange = (g: CanvasGraph.t, items: list(Lab.item)): list(Lab.item) => {
  let (node_ord, _) = source_order(g);
  let parent: Hashtbl.t(string, string) = Hashtbl.create(64);
  let rec find = k =>
    switch (Hashtbl.find_opt(parent, k)) {
    | Some(p) when p != k =>
      let r = find(p);
      Hashtbl.replace(parent, k, r);
      r;
    | _ => k
    };
  let here: Hashtbl.t(string, Lab.item) = Hashtbl.create(64);
  List.iter((n: Lab.item) => Hashtbl.replace(here, n.key, n), items);
  let join = (a, b) =>
    if (Hashtbl.mem(here, a) && Hashtbl.mem(here, b)) {
      let ra = find(a)
      and rb = find(b);
      if (ra != rb) {
        Hashtbl.replace(parent, ra, rb);
      };
    };
  List.iter((e: CanvasGraph.edge) => join(e.e_src, e.dst), g.edges);
  List.iter(
    (n: CanvasGraph.tynode) => {
      List.iter(join(n.key), n.deps @ n.parts);
      switch (n.m_path) {
      | [root, ..._] => join(n.key, "{}@" ++ root)
      | [] => ()
      };
    },
    g.nodes,
  );
  List.iter((n: Lab.item) => Option.iter(join(n.key), n.anchor), items);
  let groups: Hashtbl.t(string, list(Lab.item)) = Hashtbl.create(16);
  List.iter(
    (n: Lab.item) => {
      let r = find(n.key);
      Hashtbl.replace(
        groups,
        r,
        [n, ...Option.value(~default=[], Hashtbl.find_opt(groups, r))],
      );
    },
    items,
  );
  let blocks =
    Hashtbl.fold(
      (r, ns, acc) => {
        let (x0, y0, x1, y1, first) =
          List.fold_left(
            ((x0, y0, x1, y1, first), n: Lab.item) =>
              (
                min(x0, n.p.x -. n.w /. 2.),
                min(y0, n.p.y -. n.h /. 2.),
                max(x1, n.p.x +. n.w /. 2.),
                max(y1, n.p.y +. n.h /. 2.),
                min(first, node_ord(n.key)),
              ),
            (infinity, infinity, neg_infinity, neg_infinity, max_int),
            ns,
          );
        [((first, r), x0, y0, x1 -. x0, y1 -. y0), ...acc];
      },
      groups,
      [],
    )
    |> List.sort(compare);
  let corners =
    shelves(
      ~aspect=aspect^,
      ~gap=42.,
      List.map((((_, r), _, _, w, h)) => (r, w, h), blocks),
    );
  let shift: Hashtbl.t(string, (float, float)) = Hashtbl.create(16);
  List.iter(
    (((_, r), x0, y0, _, _)) =>
      switch (List.find_opt(((id, _, _)) => id == r, corners)) {
      | Some((_, x, y)) => Hashtbl.replace(shift, r, (x -. x0, y -. y0))
      | None => ()
      },
    blocks,
  );
  List.map(
    (n: Lab.item) =>
      switch (Hashtbl.find_opt(shift, find(n.key))) {
      | Some((dx, dy)) => {
          ...n,
          p: {
            x: snap(n.p.x +. dx),
            y: snap(n.p.y +. dy),
          },
        }
      | None => n
      },
    items,
  );
};

/* ---------- ledger and clusters: a type and what is made of and from it ---------- */

let places = (k: string): bool => k == "ledger" || k == "clusters";

/* extents around a head's centre, and where its dependants sit */
type box = {
  l: float,
  r: float,
  u: float,
  d: float,
  at: list((string, float, float)),
};

let place =
    (~radius: string => float, kind: string, g: CanvasGraph.t)
    : list((string, point)) => {
  let (node_ord, edge_ord) = source_order(g);
  let node: Hashtbl.t(string, CanvasGraph.tynode) = Hashtbl.create(64);
  List.iter(
    (n: CanvasGraph.tynode) => Hashtbl.replace(node, n.key, n),
    g.nodes,
  );
  let is_former = (k: string) =>
    String.length(k) >= 3 && String.sub(k, 0, 3) == "{}@";
  /* everything that touches a node: the functions into and out of it
     (other end, whether the node is the result) and what it is part of */
  let touching: Hashtbl.t(string, list((string, bool))) =
    Hashtbl.create(64);
  let touch = (k, use) =>
    Hashtbl.replace(
      touching,
      k,
      [use, ...Option.value(~default=[], Hashtbl.find_opt(touching, k))],
    );
  List.iter(
    (e: CanvasGraph.edge) =>
      if (e.e_src != e.dst) {
        touch(e.e_src, (e.dst, false));
        touch(e.dst, (e.e_src, true));
      },
    g.edges,
  );
  List.iter(
    (n: CanvasGraph.tynode) =>
      List.iter(
        p =>
          if (p != n.key) {
            touch(p, (n.key, false));
          },
        List.sort_uniq(compare, n.parts @ n.deps),
      ),
    g.nodes,
  );
  /* a terminal hangs off its anchor; a tuple that includes a shared type
     hangs off the first one, on its output side; an unnamed type used in
     one place only hangs off that place */
  let host_of = (n: CanvasGraph.tynode): option((string, bool)) =>
    switch (n.sat) {
    | Some((a, out)) when a != n.key && Hashtbl.mem(node, a) =>
      Some((a, out))
    | Some(_) => None
    | None when is_former(n.key) => None
    | None =>
      /* the tuple's first named type, else its first shared part */
      let shared = (want: CanvasGraph.tynode => bool) =>
        List.find_opt(
          p =>
            p != n.key
            && (
              switch (Hashtbl.find_opt(node, p)) {
              | Some(m) => Option.map(fst, m.sat) != Some(n.key) && want(m)
              | None => false
              }
            ),
          n.parts,
        );
      let shared_part =
        n.kind != Product
          ? None
          : (
            switch (shared(m => m.kind == Alias)) {
            | Some(p) => Some(p)
            | None =>
              switch (shared(m => m.kind == Ghost)) {
              | Some(p) => Some(p)
              | None => shared(_ => true)
              }
            }
          );
      let own_terminal = other =>
        switch (Hashtbl.find_opt(node, other)) {
        | Some(m) => Option.map(fst, m.sat) == Some(n.key)
        | None => true
        };
      switch (shared_part, Hashtbl.find_opt(touching, n.key)) {
      | (Some(p), _) => Some((p, true))
      | (None, Some([(other, is_result)]))
          when n.kind != Alias && !own_terminal(other) =>
        Some((other, is_result))
      | _ => None
      };
    };
  let host: Hashtbl.t(string, (string, bool)) = Hashtbl.create(64);
  List.iter(
    (n: CanvasGraph.tynode) =>
      Option.iter(h => Hashtbl.replace(host, n.key, h), host_of(n)),
    g.nodes,
  );
  /* a chain that comes back on itself has no head: cut it */
  let rec head = (seen, k) =>
    switch (Hashtbl.find_opt(host, k)) {
    | Some((h, _)) when !List.mem(h, seen) && List.length(seen) < 24 =>
      head([k, ...seen], h)
    | Some(_) =>
      Hashtbl.remove(host, k);
      k;
    | None => k
    };
  let head_of: Hashtbl.t(string, string) = Hashtbl.create(64);
  List.iter(
    (n: CanvasGraph.tynode) =>
      Hashtbl.replace(head_of, n.key, head([], n.key)),
    g.nodes,
  );
  let kids: Hashtbl.t((string, bool), list(string)) = Hashtbl.create(64);
  /* the function a dependant belongs to, for its order and label room */
  let link: Hashtbl.t(string, (int, float)) = Hashtbl.create(64);
  List.iter(
    (e: CanvasGraph.edge) => {
      let chip = float_of_int(String.length(e.e_label)) *. 7.4 +. 24.;
      List.iter(
        k =>
          switch (Hashtbl.find_opt(link, k)) {
          | Some((o, w)) =>
            Hashtbl.replace(
              link,
              k,
              (min(o, edge_ord(e.e_name)), max(w, chip)),
            )
          | None => Hashtbl.replace(link, k, (edge_ord(e.e_name), chip))
          },
        [e.e_src, e.dst],
      );
    },
    g.edges,
  );
  let kid_ord = k =>
    switch (Hashtbl.find_opt(link, k)) {
    | Some((o, _)) => o
    | None => node_ord(k)
    };
  List.iter(
    (n: CanvasGraph.tynode) =>
      switch (Hashtbl.find_opt(host, n.key)) {
      | Some(side) =>
        Hashtbl.replace(
          kids,
          side,
          [
            n.key,
            ...Option.value(~default=[], Hashtbl.find_opt(kids, side)),
          ],
        )
      | None => ()
      },
    g.nodes,
  );
  let kids_of = (k, right) =>
    Option.value(~default=[], Hashtbl.find_opt(kids, (k, right)))
    |> List.map(c => (kid_ord(c), c))
    |> List.sort(compare)
    |> List.map(snd);
  let orbits: Hashtbl.t(string, int) = Hashtbl.create(8);
  List.iter(
    (e: CanvasGraph.edge) =>
      if (e.e_src == e.dst) {
        Hashtbl.replace(
          orbits,
          e.dst,
          1 + Option.value(~default=0, Hashtbl.find_opt(orbits, e.dst)),
        );
      },
    g.edges,
  );
  let own = (k: string) => {
    let rad = radius(k);
    let n = Hashtbl.find_opt(node, k);
    let text =
      switch (n) {
      | Some(n) when n.kind != Product => String.length(n.label)
      | _ => 0
      };
    let loops = Option.value(~default=0, Hashtbl.find_opt(orbits, k));
    /* a very long label overflows its place rather than widening it */
    let half = max(rad, min(70., float_of_int(text) *. 3.3)) +. 4.;
    let up =
      rad +. 4. +. (loops == 0 ? 0. : float_of_int(loops) *. 24. +. 40.);
    let down = rad +. (text == 0 ? 4. : 17.);
    (rad, half, up, down);
  };
  let chip = k =>
    switch (Hashtbl.find_opt(link, k)) {
    | Some((_, w)) => w
    | None => 0.
    };

  /* ledger: a head with what makes it in rows on its left and what is
     made from it in rows on its right, in source order */
  let rec rows = (depth: int, k: string): box => {
    let (rad, half, up, down) = own(k);
    let side = (right: bool) => {
      let cs = List.map(c => (c, rows(depth + 1, c)), kids_of(k, right));
      if (cs == []) {
        (0., 0., []);
      } else {
        let room = List.fold_left((s, (c, _)) => max(s, chip(c)), 0., cs);
        let inner =
          List.fold_left(
            (s, (_, b: box)) => max(s, right ? b.l : b.r),
            0.,
            cs,
          );
        /* the level run between the bend and the row's end holds the label */
        let bend =
          52. +. 5. *. float_of_int(min(6, (List.length(cs) + 1) / 2));
        let reach =
          snap(
            max(
              half +. 14. +. inner,
              rad +. (room == 0. ? 30. : bend +. room +. 24.) +. inner,
            ),
          );
        /* fields that are plain terminals share rows, a few abreast:
           a record of a dozen strings is not a dozen rows */
        let plain = ((c, b: box)) =>
          b.at == []
          && chip(c) == 0.
          && (
            switch (Hashtbl.find_opt(node, c)) {
            | Some(n) => n.kind == Builtin
            | None => false
            }
          );
        let (plains, others) = List.partition(plain, cs);
        let abreast =
          List.length(plains) >= 9 ? 3 : List.length(plains) >= 4 ? 2 : 1;
        let rec chunk = l =>
          switch (l) {
          | [] => []
          | _ =>
            let row = List.filteri((i, _) => i < abreast, l);
            [row, ...chunk(List.filteri((i, _) => i >= abreast, l))];
          };
        let groups = List.map(x => [x], others) @ chunk(plains);
        let stride = 50.;
        let above = row =>
          List.fold_left((s, (_, b: box)) => max(s, b.u), 0., row);
        let pitch = row =>
          Float.ceil(
            List.fold_left((s, (_, b: box)) => max(s, b.u +. b.d), 0., row)
            /. grid,
          )
          *. grid;
        let total = List.fold_left((s, row) => s +. pitch(row), 0., groups);
        let sx = right ? 1. : (-1.);
        /* the head is level with the middle of its first and last rows */
        let (first, last) =
          switch (groups, List.rev(groups)) {
          | ([f, ..._], [l, ..._]) => (
              above(f),
              total -. pitch(l) +. above(l),
            )
          | _ => (0., 0.)
          };
        let top = -. snap((first +. last) /. 2.);
        let (at, _, out) =
          List.fold_left(
            ((at, y, out), row) => {
              let cy = y +. above(row);
              let (at, out, _) =
                List.fold_left(
                  ((at, out, j), (c, b: box)) => {
                    let x = sx *. (reach +. float_of_int(j) *. stride);
                    (
                      at
                      @ [(c, x, cy)]
                      @ List.map(
                          ((k, x', y')) => (k, x +. x', y' +. cy),
                          b.at,
                        ),
                      max(out, abs_float(x) +. (right ? b.r : b.l)),
                      j + 1,
                    );
                  },
                  (at, out, 0),
                  row,
                );
              (at, y +. pitch(row), out);
            },
            ([], top, 0.),
            groups,
          );
        (out, max(-. top, total +. top), at);
      };
    };
    let (lo, lh, la) = side(false)
    and (ro, rh, ra) = side(true);
    {
      l: max(half, lo),
      r: max(half, ro),
      u: max(up, max(lh, rh)),
      d: max(down, max(lh, rh)),
      at: la @ ra,
    };
  };

  /* clusters: the same dependants on arcs around the head, what makes it
     to the left and what is made from it to the right */
  let flower = (k: string): box => {
    let (rad, half, up, down) = own(k);
    let side = (right: bool) => {
      let cs = List.map(c => (c, rows(1, c)), kids_of(k, right));
      let weight = (b: box) => max(1., (b.u +. b.d +. 6.) /. 40.);
      let total = List.fold_left((s, (_, b)) => s +. weight(b), 0., cs);
      let rings = List.length(cs) > 7 ? 2. : 1.;
      let span =
        min(2.9, max(List.length(cs) < 2 ? 0.1 : 1.3, total *. 0.5 /. rings));
      let room = List.fold_left((s, (c, _)) => max(s, chip(c)), 0., cs);
      let rho = max(rad +. 40. +. room *. 0.8, total *. 40. /. rings /. span);
      let (at, _) =
        List.fold_left(
          ((at, (before, i)), (c, b: box)) => {
            let w = weight(b);
            let phi = -. span /. 2. +. span *. (before +. w /. 2.) /. total;
            let rho = rho +. (rings > 1. && i mod 2 == 1 ? 72. : 0.);
            let x = (right ? 1. : (-1.)) *. cos(phi) *. rho
            and y = sin(phi) *. rho;
            (
              at
              @ [(c, x, y, b)]
              @ List.map(
                  ((k, x', y')) =>
                    (
                      k,
                      x +. x',
                      y +. y',
                      {
                        ...b,
                        at: [],
                      },
                    ),
                  b.at,
                ),
              (before +. w, i + 1),
            );
          },
          ([], (0., 0)),
          cs,
        );
      at;
    };
    let at = side(false) @ side(true);
    let (l, r, u, d) =
      List.fold_left(
        ((l, r, u, d), (c, x, y, _)) => {
          let (_, half, up, down) = own(c);
          (
            max(l, half -. x),
            max(r, x +. half),
            max(u, up -. y),
            max(d, y +. down),
          );
        },
        (half, half, up, down),
        at,
      );
    {
      l,
      r,
      u,
      d,
      at: List.map(((c, x, y, _)) => (c, x, y), at),
    };
  };

  let heads =
    List.filter(
      (n: CanvasGraph.tynode) => !Hashtbl.mem(host, n.key),
      g.nodes,
    );
  let boxes: Hashtbl.t(string, box) = Hashtbl.create(32);
  List.iter(
    (n: CanvasGraph.tynode) =>
      Hashtbl.replace(
        boxes,
        n.key,
        kind == "clusters" ? flower(n.key) : rows(0, n.key),
      ),
    heads,
  );
  /* columns read left to right: a head sits after the heads it is made
     from (a cycle is cut where source order meets it again) */
  let feeds: Hashtbl.t(string, list(string)) = Hashtbl.create(32);
  let feed = (a, b) =>
    switch (Hashtbl.find_opt(head_of, a), Hashtbl.find_opt(head_of, b)) {
    | (Some(ha), Some(hb)) when ha != hb =>
      let l = Option.value(~default=[], Hashtbl.find_opt(feeds, hb));
      if (!List.mem(ha, l)) {
        Hashtbl.replace(feeds, hb, [ha, ...l]);
      };
    | _ => ()
    };
  List.iter((e: CanvasGraph.edge) => feed(e.e_src, e.dst), g.edges);
  List.iter(
    (n: CanvasGraph.tynode) => List.iter(d => feed(d, n.key), n.deps),
    g.nodes,
  );
  let tier: Hashtbl.t(string, int) = Hashtbl.create(32);
  let rec tier_of = (stack, k) =>
    switch (Hashtbl.find_opt(tier, k)) {
    | Some(t) => t
    | None =>
      let t =
        List.fold_left(
          (t, f) =>
            List.mem(f, stack) ? t : max(t, 1 + tier_of([k, ...stack], f)),
          0,
          Option.value(~default=[], Hashtbl.find_opt(feeds, k)),
        );
      Hashtbl.replace(tier, k, t);
      t;
    };
  let sorted =
    List.map((n: CanvasGraph.tynode) => (node_ord(n.key), n), heads)
    |> List.sort(((a, _), (b, _)) => compare(a, b))
    |> List.map(((o, n: CanvasGraph.tynode)) => (tier_of([], n.key), o, n));
  /* a module's members stay together, where its first one falls */
  let group = (n: CanvasGraph.tynode) =>
    switch (n.m_path) {
    | [root, ..._] => root
    | [] => ""
    };
  let group_at: Hashtbl.t(string, (int, int)) = Hashtbl.create(8);
  List.iter(
    ((t, o, n)) =>
      if (group(n) != "") {
        switch (Hashtbl.find_opt(group_at, group(n))) {
        | Some(at) when at <= (t, o) => ()
        | _ => Hashtbl.replace(group_at, group(n), (t, o))
        };
      },
    sorted,
  );
  let sorted =
    List.map(
      ((t, o, n: CanvasGraph.tynode)) =>
        (
          Option.value(
            ~default=(t, o),
            Hashtbl.find_opt(group_at, group(n)),
          ),
          group(n),
          t,
          o,
          n.key,
        ),
      sorted,
    )
    |> List.sort(compare);
  let corners =
    columns(
      ~aspect=aspect^,
      ~gap=kind == "clusters" ? 28. : 42.,
      List.map(
        ((_, _, _, _, k)) => {
          let b = Hashtbl.find(boxes, k);
          (k, b.l +. b.r, b.u +. b.d);
        },
        sorted,
      ),
    );
  List.concat_map(
    ((k, x, y)) => {
      let b = Hashtbl.find(boxes, k);
      let cx = snap(x +. b.l)
      and cy = snap(y +. b.u);
      [
        (
          k,
          Lab.{
            x: cx,
            y: cy,
          },
        ),
        ...List.map(
             ((c, dx, dy)) =>
               (
                 c,
                 Lab.{
                   x: snap(cx +. dx),
                   y: snap(cy +. dy),
                 },
               ),
             b.at,
           ),
      ];
    },
    corners,
  );
};

/* ---------- edge styles ---------- */

/* what an edge is drawn as: a polyline (rounded when drawn), or a curve
   by its two control points; and where its label would like to sit */
type wire = {
  points: list(point),
  ctrl: option((point, point)),
  label: point,
};

let routes = (): bool => Lab.wires^ == "metro" || Lab.wires^ == "straight";

type end_side =
  | Right
  | Left
  | Below
  | Above;

/* metro: every edge runs level, turns once on a diagonal (or, where it is
   too steep, down a vertical between two diagonals) and runs level again.
   Edges that share a node's side leave it side by side, and turn in the
   order that keeps them from crossing each other. */
let metro =
    (
      ~nodes: list((string, point, float)),
      ~room: string => float,
      edges: list((string, string, string)),
    )
    : list((string, wire)) => {
  let at: Hashtbl.t(string, (point, float)) = Hashtbl.create(64);
  List.iter(((k, p, r)) => Hashtbl.replace(at, k, (p, r)), nodes);
  let plan =
    List.filter_map(
      ((name, a, b)) =>
        switch (Hashtbl.find_opt(at, a), Hashtbl.find_opt(at, b)) {
        | (Some((pa, ra)), Some((pb, rb))) =>
          let dx = pb.x -. pa.x
          and dy = pb.y -. pa.y;
          let level =
            abs_float(dx) >= ra
            +. rb
            +. 30.
            || abs_float(dx) >= abs_float(dy);
          let (sa, sb) =
            level
              ? dx >= 0. ? (Right, Left) : (Left, Right)
              : dy >= 0. ? (Below, Above) : (Above, Below);
          Some((name, a, b, pa, ra, pb, rb, level, sa, sb));
        | _ => None
        },
      edges,
    );
  /* who shares a side of a node, in the order of where they come from */
  let sides:
    Hashtbl.t((string, end_side), list(((float, float, string), string))) =
    Hashtbl.create(64);
  let join = (k, side, other: point, name) => {
    let key =
      switch (side) {
      | Right
      | Left => (other.y, other.x, name)
      | Below
      | Above => (other.x, other.y, name)
      };
    Hashtbl.replace(
      sides,
      (k, side),
      [
        (key, name),
        ...Option.value(~default=[], Hashtbl.find_opt(sides, (k, side))),
      ],
    );
  };
  List.iter(
    ((name, a, b, pa, _, pb, _, _, sa, sb)) => {
      join(a, sa, pb, name);
      join(b, sb, pa, name);
    },
    plan,
  );
  let slot: Hashtbl.t((string, end_side, string), (int, int)) =
    Hashtbl.create(64);
  Hashtbl.iter(
    ((k, side), l) => {
      let l = List.sort(compare, l);
      let n = List.length(l);
      List.iteri(
        (i, (_, name)) => Hashtbl.replace(slot, (k, side, name), (i, n)),
        l,
      );
    },
    sides,
  );
  let port = (p: point, r: float, side, (i, n)): point => {
    let gap = min(5., 1.5 *. r /. float_of_int(max(1, n - 1)));
    let o = (float_of_int(i) -. float_of_int(n - 1) /. 2.) *. gap;
    let c = sqrt(max(0., r *. r -. o *. o));
    switch (side) {
    | Right => {
        x: p.x +. c,
        y: p.y +. o,
      }
    | Left => {
        x: p.x -. c,
        y: p.y +. o,
      }
    | Below => {
        x: p.x +. o,
        y: p.y +. c,
      }
    | Above => {
        x: p.x +. o,
        y: p.y -. c,
      }
    };
  };
  let ends =
    List.map(
      ((name, a, b, pa, ra, pb, rb, level, sa, sb)) => {
        let (ia, na) = Hashtbl.find(slot, (a, sa, name))
        and (ib, nb) = Hashtbl.find(slot, (b, sb, name));
        let s = port(pa, ra, sa, (ia, na))
        and d = port(pb, rb, sb, (ib, nb));
        /* the busier end is where the edge turns */
        let from_src = na >= nb;
        (name, s, d, level, from_src, from_src ? (a, sa, ia) : (b, sb, ib));
      },
      plan,
    );
  /* at a shared side, an edge that turns one way gives way to those that
     go further the same way: they turn first */
  let across = (level, p: point) => level ? p.y : p.x;
  let rise = ((_, s, d, level, from_src, _)) =>
    from_src
      ? across(level, d) -. across(level, s)
      : across(level, s) -. across(level, d);
  let turners: Hashtbl.t((string, end_side), list((int, float))) =
    Hashtbl.create(64);
  List.iter(
    ((_, _, _, _, _, (k, side, i)) as e) =>
      Hashtbl.replace(
        turners,
        (k, side),
        [
          (i, rise(e)),
          ...Option.value(
               ~default=[],
               Hashtbl.find_opt(turners, (k, side)),
             ),
        ],
      ),
    ends,
  );
  List.map(
    ((name, s, d, level, from_src, (k, side, i)) as e) => {
      let rise = rise(e);
      let peers =
        Option.value(~default=[], Hashtbl.find_opt(turners, (k, side)));
      let lane =
        List.length(
          List.filter(
            ((j, r)) =>
              rise > 1.5
              && r > 1.5
              && j > i
              || rise < (-1.5)
              && r < (-1.5)
              && j < i,
            peers,
          ),
        );
      let turning =
        List.length(
          List.filter(((_, r)) => rise > 0. ? r > 1.5 : r < (-1.5), peers),
        );
      /* a frame in which the turning end is at the left and the edge
         runs to the right */
      let (h, o) = from_src ? (s, d) : (d, s);
      let flip = (level ? o.x -. h.x : o.y -. h.y) >= 0. ? 1. : (-1.);
      let into = (p: point): point =>
        level
          ? {
            x: flip *. p.x,
            y: p.y,
          }
          : {
            x: flip *. p.y,
            y: p.x,
          };
      let back = (p: point): point =>
        level
          ? {
            x: flip *. p.x,
            y: p.y,
          }
          : {
            x: p.y,
            y: flip *. p.x,
          };
      let h' = into(h)
      and o' = into(o);
      let span = o'.x -. h'.x;
      let up = abs_float(rise);
      let sg = rise >= 0. ? 1. : (-1.);
      let between =
        min(5., max(1.5, span *. 0.3 /. float_of_int(max(1, turning))));
      let stub =
        min(12. +. float_of_int(lane) *. between, max(6., span *. 0.6));
      let pt = (x, y): point => {
        x,
        y,
      };
      let line =
        if (up < 1.5 || span < 8.) {
          [h', o'];
        } else if (up <= span
                   -. stub
                   -. 12.
                   && (up <= 28. || span -. stub -. up -. 12. >= room(name))) {
          [h', pt(h'.x +. stub, h'.y), pt(h'.x +. stub +. up, o'.y), o'];
        } else {
          let q = min(min(14., (span -. stub -. 8.) /. 2.), up /. 2.);
          let x1 = h'.x +. min(stub, max(4., span -. 4.));
          q < 3.
            ? [h', pt(x1, h'.y), pt(x1, o'.y), o']
            : [
              h',
              pt(x1, h'.y),
              pt(x1 +. q, h'.y +. sg *. q),
              pt(x1 +. q, o'.y -. sg *. q),
              pt(x1 +. 2. *. q, o'.y),
              o',
            ];
        };
      let line = List.map(back, line);
      let points = from_src ? line : List.rev(line);
      /* the label rides the longest level run */
      let (best, len) =
        List.fold_left(
          ((best, len), (a: point, b: point)) =>
            abs_float(a.y -. b.y) < 0.5 && abs_float(a.x -. b.x) > len
              ? (
                Some(
                  Lab.{
                    x: (a.x +. b.x) /. 2.,
                    y: a.y,
                  },
                ),
                abs_float(a.x -. b.x),
              )
              : (best, len),
          (None, 0.),
          Lab.segments(points),
        );
      let label =
        switch (best) {
        | Some(p) when len > 40. => p
        | _ => Lab.along(points, 0.5)
        };
      (
        name,
        {
          points,
          ctrl: None,
          label,
        },
      );
    },
    ends,
  );
};

/* straight: a chord from rim to rim. Edges between the same two nodes (and
   an edge beside a "made of" link) bow apart. Around a busy node the
   labels sit toward the far end, where the chords have spread. */
let straight =
    (
      ~nodes: list((string, point, float)),
      ~links: list((string, string)),
      edges: list((string, string, string)),
    )
    : list((string, wire)) => {
  let at: Hashtbl.t(string, (point, float)) = Hashtbl.create(64);
  List.iter(((k, p, r)) => Hashtbl.replace(at, k, (p, r)), nodes);
  let pair = (a, b) => a <= b ? (a, b) : (b, a);
  let between: Hashtbl.t((string, string), list(string)) =
    Hashtbl.create(64);
  List.iter(
    ((a, b)) =>
      if (!Hashtbl.mem(between, pair(a, b))) {
        Hashtbl.replace(between, pair(a, b), [""]);
      },
    links,
  );
  let degree: Hashtbl.t(string, int) = Hashtbl.create(64);
  List.iter(
    ((name, a, b)) => {
      Hashtbl.replace(
        between,
        pair(a, b),
        Option.value(~default=[], Hashtbl.find_opt(between, pair(a, b)))
        @ [name],
      );
      List.iter(
        k =>
          Hashtbl.replace(
            degree,
            k,
            1 + Option.value(~default=0, Hashtbl.find_opt(degree, k)),
          ),
        [a, b],
      );
    },
    edges,
  );
  let deg = k => Option.value(~default=0, Hashtbl.find_opt(degree, k));
  List.filter_map(
    ((name, a, b)) =>
      switch (Hashtbl.find_opt(at, a), Hashtbl.find_opt(at, b)) {
      | (Some((pa, ra)), Some((pb, rb))) =>
        let unit = (p: point, q: point): point => {
          let l = max(1., Lab.distance(p, q));
          {
            x: (q.x -. p.x) /. l,
            y: (q.y -. p.y) /. l,
          };
        };
        let group = Hashtbl.find(between, pair(a, b));
        let m = List.length(group);
        let rec index = (i, l) =>
          switch (l) {
          | [x, ..._] when x == name => i
          | [_, ...rest] => index(i + 1, rest)
          | [] => 0
          };
        let bow =
          (float_of_int(index(0, group)) -. float_of_int(m - 1) /. 2.)
          *. 22.;
        /* the bow's side is the pair's, not the edge's direction */
        let (ka, _) = pair(a, b);
        let u = ka == a ? unit(pa, pb) : unit(pb, pa);
        let c: point = {
          x: (pa.x +. pb.x) /. 2. -. u.y *. bow *. 2.,
          y: (pa.y +. pb.y) /. 2. +. u.x *. bow *. 2.,
        };
        let ua = unit(pa, c)
        and ub = unit(pb, c);
        let s: point = {
          x: pa.x +. ua.x *. ra,
          y: pa.y +. ua.y *. ra,
        }
        and d: point = {
          x: pb.x +. ub.x *. rb,
          y: pb.y +. ub.y *. rb,
        };
        let third = (p: point, q: point): point => {
          x: p.x +. (q.x -. p.x) *. 2. /. 3.,
          y: p.y +. (q.y -. p.y) *. 2. /. 3.,
        };
        let t =
          deg(a) >= 4 && deg(a) > deg(b)
            ? 0.66 : deg(b) >= 4 && deg(b) > deg(a) ? 0.34 : 0.5;
        let v = 1. -. t;
        Some((
          name,
          {
            points: [],
            ctrl: Some((third(s, c), third(d, c))),
            label: {
              x: v *. v *. s.x +. 2. *. v *. t *. c.x +. t *. t *. d.x,
              y: v *. v *. s.y +. 2. *. v *. t *. c.y +. t *. t *. d.y,
            },
          },
        ));
      | _ => None
      },
    edges,
  );
};

let route = (~nodes, ~links, ~room, edges) =>
  Lab.wires^ == "metro"
    ? metro(~nodes, ~room, edges) : straight(~nodes, ~links, edges);

/* a polyline with its corners rounded (the lab's `path`, wider) */
let path = (points: list(point)): string =>
  if (Lab.wires^ != "metro") {
    Lab.path(points);
  } else {
    switch (points) {
    | [] => ""
    | [first, ...rest] =>
      let fmt = (p: point) => Printf.sprintf("%.1f,%.1f", p.x, p.y);
      let rec draw = (prev, ps) =>
        switch (ps) {
        | [p, next, ...tail] =>
          let back = Lab.distance(prev, p)
          and ahead = Lab.distance(p, next);
          let r = min(10., min(back /. 2., ahead /. 2.));
          let inset = (q: point, d) =>
            d < 0.001
              ? p
              : Lab.{
                  x: p.x +. (q.x -. p.x) *. r /. d,
                  y: p.y +. (q.y -. p.y) *. r /. d,
                };
          " L "
          ++ fmt(inset(prev, back))
          ++ " Q "
          ++ fmt(p)
          ++ " "
          ++ fmt(inset(next, ahead))
          ++ draw(p, [next, ...tail]);
        | [last] => " L " ++ fmt(last)
        | [] => ""
        };
      "M " ++ fmt(first) ++ draw(first, rest);
    };
  };
