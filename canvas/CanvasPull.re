/* CanvasPull — see the interface. Small moves in rounds, in two parts.
   First the pulling: members toward their centres, modules side by side
   apart, and only nodes of one module kept off each other, so a member on
   the far side of another module can pass through it. Then the settling:
   everything moved clear of everything else. */
type strength =
  | Mild
  | Strong;

let strengths = [("none", "none"), ("mild", "mild"), ("strong", "strong")];
let current: ref(option(strength)) = ref(None);
let chosen = (): option(strength) => current^;
let id = (): string =>
  switch (current^) {
  | None => "none"
  | Some(Mild) => "mild"
  | Some(Strong) => "strong"
  };
let set = (k: string): bool =>
  switch (k) {
  | "none" =>
    current := None;
    true;
  | "mild" =>
    current := Some(Mild);
    true;
  | "strong" =>
    current := Some(Strong);
    true;
  | _ => false
  };

type point = (float, float);
type xy = {
  x: float,
  y: float,
};

/* the share of the way to its module's centre a member moves each round */
let step = 0.03;
/* rounds of pulling. With nothing in the way, 14 leave a module at two
   thirds of its spread and 40 at under a third. */
let rounds = (s: strength): int =>
  switch (s) {
  | Mild => 14
  | Strong => 40
  };
/* rounds of only moving clear, after the pulling */
let settle = 60;
/* how much of their packed size two modules side by side keep between
   their centres */
let spread = (s: strength): float =>
  switch (s) {
  | Mild => 0.6
  | Strong => 1.
  };

/* clear space between two nodes of one module: what is asked for, and
   the least there is where the layout gave less */
let within = 44.;
let touching = 16.;
/* the most room an arrow is given for its label and its row of tests */
let longest = 230.;
/* clear space between the region circles of unrelated modules */
let between = 8.;
/* CanvasView's circles: around a member, around a module's own node, and
   what each level of nesting adds */
let member_r = 46.;
let former_r = 62.;
let depth_pad = 14.;
/* a node with members of a module it is not in on every side, this near,
   is inside that module; each round it moves this far toward the way out */
let reach = 240.;
/* the widest opening between them, in radians, that still encloses: past
   a half turn, so a node ends a little beyond the line between the two
   members it left through, not on it */
let enclosed = 3.8;
let nudge = 12.;

let grid = 14.;
let snap = (v: float): float => Float.round(v /. grid) *. grid;

let is_former = (k: string): bool =>
  String.length(k) >= 3 && String.sub(k, 0, 3) == "{}@";

let rec prefixes = (p: list(string)): list(list(string)) =>
  switch (List.rev(p)) {
  | [] => []
  | [_, ...rest] => [p, ...prefixes(List.rev(rest))]
  };

let rec common = (a: list(string), b: list(string)): int =>
  switch (a, b) {
  | ([x, ...at], [y, ...bt]) when x == y => 1 + common(at, bt)
  | _ => 0
  };

/* A node as this pass sees it. Nodes that move as one (a node and the
   satellites docked to it) share a body; `lead` is the one they follow. */
type disc = {
  key: string,
  body: int,
  lead: bool,
  r: float,
  /* its module, as an index into the paths in use, and that path's depth */
  home: int,
  depth: int,
  /* the radius of the circle its own module's region draws around it */
  hull: float,
  /* where the layout put it, and that place relative to its lead */
  at: xy,
  off: xy,
};

let apply =
    (s: strength, g: CanvasGraph.t, nodes: list((string, point, float)))
    : list((string, point)) => {
  let nodes =
    List.map(
      ((k, (x, y), r)) =>
        (
          k,
          {
            x,
            y,
          },
          r,
        ),
      nodes,
    );
  let info: Hashtbl.t(string, CanvasGraph.tynode) = Hashtbl.create(64);
  List.iter(
    (n: CanvasGraph.tynode) => Hashtbl.replace(info, n.key, n),
    g.nodes,
  );
  let placed: Hashtbl.t(string, xy) = Hashtbl.create(64);
  List.iter(((k, p, _)) => Hashtbl.replace(placed, k, p), nodes);
  let path_of = CanvasContour.home(g.nodes, g.edges);
  /* a satellite follows the node it is docked to while both are of one
     module; a terminal docked to another module's type goes its own way */
  let rec lead_of = (seen: list(string), k: string): string =>
    switch (Option.bind(Hashtbl.find_opt(info, k), n => n.sat)) {
    | Some((anchor, _))
        when
          Hashtbl.mem(placed, anchor)
          && !List.mem(anchor, seen)
          && path_of(anchor) == path_of(k) =>
      lead_of([k, ...seen], anchor)
    | _ => k
    };

  /* the module paths in use, numbered; `shared` is how many names two of
     them start with in common, `outer` a path's prefix of a given length */
  let paths: Hashtbl.t(list(string), int) = Hashtbl.create(16);
  let number = (p: list(string)): int =>
    switch (Hashtbl.find_opt(paths, p)) {
    | Some(i) => i
    | None =>
      let i = Hashtbl.length(paths);
      Hashtbl.replace(paths, p, i);
      i;
    };
  ignore(number([]));
  List.iter(
    ((k, _, _)) =>
      List.iter(p => ignore(number(p)), prefixes(path_of(k))),
    nodes,
  );
  let n_paths = Hashtbl.length(paths);
  let deepest =
    Hashtbl.fold((p, _, m) => max(m, List.length(p)), paths, 0);
  let outer = Array.make(n_paths * (deepest + 1), 0);
  Hashtbl.iter(
    (p, i) =>
      List.iter(
        q =>
          outer[i * (deepest + 1) + List.length(q)] = Hashtbl.find(paths, q),
        prefixes(p),
      ),
    paths,
  );
  let shared = Array.make(n_paths * n_paths, 0);
  Hashtbl.iter(
    (p, i) =>
      Hashtbl.iter((q, j) => shared[i * n_paths + j] = common(p, q), paths),
    paths,
  );

  /* a module's own node grows with what the module holds, as the view
     draws it */
  let holds: Hashtbl.t(list(string), int) = Hashtbl.create(16);
  let below: Hashtbl.t(list(string), int) = Hashtbl.create(16);
  let count = (p: list(string)) => {
    Hashtbl.replace(
      holds,
      p,
      1 + Option.value(~default=0, Hashtbl.find_opt(holds, p)),
    );
    List.iter(
      q =>
        Hashtbl.replace(
          below,
          q,
          max(
            List.length(p) - List.length(q),
            Option.value(~default=0, Hashtbl.find_opt(below, q)),
          ),
        ),
      prefixes(p),
    );
  };
  List.iter((n: CanvasGraph.tynode) => count(n.m_path), g.nodes);
  List.iter((e: CanvasGraph.edge) => count(e.m_path), g.edges);
  let hull_of = (k: string, p: list(string)): float =>
    if (p == []) {
      0.;
    } else if (is_former(k)) {
      let members = Option.value(~default=1, Hashtbl.find_opt(holds, p)) - 1;
      let deeper = Option.value(~default=0, Hashtbl.find_opt(below, p));
      former_r
      +. depth_pad
      *. float_of_int(deeper)
      +. min(50., 5. *. float_of_int(members));
    } else {
      member_r;
    };

  /* bodies, in the order their first node comes */
  let bodies: Hashtbl.t(string, int) = Hashtbl.create(64);
  let leads =
    List.map(
      ((k, _, _)) => {
        let l = lead_of([], k);
        if (!Hashtbl.mem(bodies, l)) {
          Hashtbl.replace(bodies, l, Hashtbl.length(bodies));
        };
        l;
      },
      nodes,
    );
  let n_bodies = Hashtbl.length(bodies);
  let bx = Array.make(n_bodies, 0.)
  and by = Array.make(n_bodies, 0.);
  let body_path = Array.make(n_bodies, []);
  Hashtbl.iter(
    (l, b) => {
      let p: xy = Hashtbl.find(placed, l);
      bx[b] = p.x;
      by[b] = p.y;
      body_path[b] = path_of(l);
    },
    bodies,
  );
  let start_x = Array.copy(bx)
  and start_y = Array.copy(by);
  let discs: array(disc) =
    List.map2(
      ((key, at: xy, r), l) => {
        let path = path_of(key);
        let lp: xy = Hashtbl.find(placed, l);
        {
          key,
          body: Hashtbl.find(bodies, l),
          lead: key == l,
          r,
          home: number(path),
          depth: List.length(path),
          hull: hull_of(key, path),
          at,
          off: {
            x: at.x -. lp.x,
            y: at.y -. lp.y,
          },
        };
      },
      nodes,
      leads,
    )
    |> Array.of_list;
  let n = Array.length(discs);
  let x = (i: int): float => bx[discs[i].body] +. discs[i].off.x
  and y = (i: int): float => by[discs[i].body] +. discs[i].off.y;
  let move = (b: int, dx: float, dy: float) => {
    bx[b] = bx[b] +. dx;
    by[b] = by[b] +. dy;
  };

  /* a module's members: the bodies whose lead is in it or in a module
     nested in it. Fewer than two have nothing to be drawn toward. */
  let members: Hashtbl.t(list(string), list(int)) = Hashtbl.create(16);
  Array.iteri(
    (b, path) =>
      List.iter(
        p =>
          Hashtbl.replace(
            members,
            p,
            [b, ...Option.value(~default=[], Hashtbl.find_opt(members, p))],
          ),
        prefixes(path),
      ),
    body_path,
  );
  let named: array((list(string), array(int))) =
    Hashtbl.fold((p, bs, acc) => [(p, bs), ...acc], members, [])
    |> List.sort(compare)
    |> List.filter_map(((p, bs)) =>
         List.length(bs) > 1 ? Some((p, Array.of_list(bs))) : None
       )
    |> Array.of_list;
  let modules: array(array(int)) = Array.map(snd, named);
  let centre = (bs: array(int)): (float, float) => {
    let k = float_of_int(Array.length(bs));
    (
      Array.fold_left((s, b) => s +. bx[b], 0., bs) /. k,
      Array.fold_left((s, b) => s +. by[b], 0., bs) /. k,
    );
  };
  /* the radius of the disc a module's nodes would fill, packed */
  let room_of = Array.make(n_bodies, 0.);
  Array.iter(
    (d: disc) =>
      room_of[d.body] = room_of[d.body] +. (d.r +. 24.) *. (d.r +. 24.),
    discs,
  );
  let packed: array(float) =
    Array.map(
      bs =>
        1.15 *. sqrt(Array.fold_left((s, b) => s +. room_of[b], 0., bs)),
      modules,
    );
  /* modules side by side: nested in the same module, or both in none */
  let parent = (p: list(string)) => List.rev(List.tl(List.rev(p)));
  let siblings: list((int, int)) = {
    let k = Array.length(named);
    List.concat(
      List.init(k, i =>
        List.filter_map(
          j =>
            j > i && parent(fst(named[i])) == parent(fst(named[j]))
              ? Some((i, j)) : None,
          List.init(k, j => j),
        )
      ),
    );
  };

  /* arrows between two bodies, at least one of them in a module, with
     the length each one's label needs: a pull that closed an arrow up
     over its label would cost more than the region gained */
  let index: Hashtbl.t(string, int) = Hashtbl.create(64);
  Array.iteri((i, d: disc) => Hashtbl.replace(index, d.key, i), discs);
  let arrows: array((int, int, float)) =
    List.filter_map(
      (e: CanvasGraph.edge) =>
        switch (
          Hashtbl.find_opt(index, e.e_src),
          Hashtbl.find_opt(index, e.dst),
        ) {
        | (Some(a), Some(b))
            when
              discs[a].body != discs[b].body
              && (discs[a].depth > 0 || discs[b].depth > 0) =>
          let room =
            float_of_int(String.length(e.e_label))
            *. 7.4
            +. 9.
            *. float_of_int(List.length(e.tests))
            +. 44.;
          Some((a, b, discs[a].r +. discs[b].r +. min(room, longest)));
        | _ => None
        },
      g.edges,
    )
    |> Array.of_list;

  /* how far apart two nodes' centres must be */
  let side = (d: disc, level: int): float =>
    d.depth > level
      ? d.hull +. depth_pad *. float_of_int(d.depth - level - 1) : d.r +. 4.;
  /* Nodes of one module keep the space the layout gave them, up to
     `within`, and never less than `touching`. Nodes of different modules
     pass through each other while `crowded` is false: a member on the far
     side of another module could not otherwise get home. Two nodes of no
     module are the layout's business and are left as they are. */
  let clearance = (~crowded: bool, a: disc, b: disc): float =>
    if (a.depth == 0 && b.depth == 0) {
      0.;
    } else if (a.home == b.home) {
      min(
        a.r +. b.r +. within,
        max(
          a.r +. b.r +. touching,
          Float.hypot(a.at.x -. b.at.x, a.at.y -. b.at.y),
        ),
      );
    } else if (crowded) {
      let level = shared[a.home * n_paths + b.home];
      side(a, level) +. side(b, level) +. between;
    } else {
      0.;
    };

  let cell =
    Array.fold_left(
      (m, d: disc) => max(m, 2. *. (d.hull +. depth_pad *. 3.) +. between),
      reach,
      discs,
    );
  let cell_of = (i: int): (int, int) => (
    int_of_float(Float.floor(x(i) /. cell)),
    int_of_float(Float.floor(y(i) /. cell)),
  );
  /* every other node in the nine cells around node i */
  let near =
      (buckets: Hashtbl.t((int, int), list(int)), i: int, f: int => unit) => {
    let (cx, cy) = cell_of(i);
    for (dx in (-1) to 1) {
      for (dy in (-1) to 1) {
        switch (Hashtbl.find_opt(buckets, (cx + dx, cy + dy))) {
        | Some(js) =>
          List.iter(
            j =>
              if (j != i && discs[j].body != discs[i].body) {
                f(j);
              },
            js,
          )
        | None => ()
        };
      };
    };
  };
  let apart = (i: int, j: int, want: float) => {
    let dx = x(i) -. x(j)
    and dy = y(i) -. y(j);
    let dist = Float.hypot(dx, dy);
    if (dist < want) {
      let (ux, uy) = dist > 0.01 ? (dx /. dist, dy /. dist) : (1., 0.);
      let push = (want -. dist) /. 2.;
      move(discs[i].body, ux *. push, uy *. push);
      move(discs[j].body, -. ux *. push, -. uy *. push);
    };
  };

  let pull = () => {
    let centres = Array.map(centre, modules);
    Array.iteri(
      (m, bs) => {
        let (cx, cy) = centres[m];
        Array.iter(
          b => move(b, step *. (cx -. bx[b]), step *. (cy -. by[b])),
          bs,
        );
      },
      modules,
    );
  };
  /* two modules side by side whose packed discs would overlap move
     apart, each as one piece, a fifth of the overlap a round */
  let part = () =>
    List.iter(
      ((i, j)) => {
        let (ax, ay) = centre(modules[i])
        and (bx', by') = centre(modules[j]);
        let dx = ax -. bx'
        and dy = ay -. by';
        let dist = Float.hypot(dx, dy);
        let want = spread(s) *. (packed[i] +. packed[j]);
        if (dist < want) {
          let (ux, uy) =
            dist > 0.01
              ? (dx /. dist, dy /. dist)
              : (cos(float_of_int(i)), sin(float_of_int(i)));
          let push = 0.1 *. (want -. dist);
          Array.iter(b => move(b, ux *. push, uy *. push), modules[i]);
          Array.iter(b => move(b, -. ux *. push, -. uy *. push), modules[j]);
        };
      },
      siblings,
    );
  let clear = (~crowded: bool): Hashtbl.t((int, int), list(int)) => {
    let buckets: Hashtbl.t((int, int), list(int)) = Hashtbl.create(n);
    for (i in 0 to n - 1) {
      let c = cell_of(i);
      Hashtbl.replace(
        buckets,
        c,
        [i, ...Option.value(~default=[], Hashtbl.find_opt(buckets, c))],
      );
    };
    for (i in 0 to n - 1) {
      near(buckets, i, j =>
        if (j > i) {
          apart(i, j, clearance(~crowded, discs[i], discs[j]));
        }
      );
    };
    Array.iter(((a, b, want)) => apart(a, b, want), arrows);
    buckets;
  };
  /* a body with members of a module it is not in on every side moves
     toward the widest opening between them */
  let expel = (buckets: Hashtbl.t((int, int), list(int))) =>
    for (i in 0 to n - 1) {
      let d = discs[i];
      if (d.lead) {
        let around: Hashtbl.t(int, list(float)) = Hashtbl.create(4);
        near(
          buckets,
          i,
          j => {
            let o = discs[j];
            let dx = x(j) -. x(i)
            and dy = y(j) -. y(i);
            let level = shared[d.home * n_paths + o.home];
            if (o.depth > level && Float.hypot(dx, dy) < reach) {
              /* the outermost module of o's that d is not in */
              let m = outer[o.home * (deepest + 1) + level + 1];
              Hashtbl.replace(
                around,
                m,
                [
                  Float.atan2(dy, dx),
                  ...Option.value(~default=[], Hashtbl.find_opt(around, m)),
                ],
              );
            };
          },
        );
        Hashtbl.fold(
          (home, angles, acc) => [(home, angles), ...acc],
          around,
          [],
        )
        |> List.sort(compare)
        |> List.iter(((_, angles)) =>
             if (List.length(angles) >= 3) {
               let a = Array.of_list(List.sort(compare, angles));
               let k = Array.length(a);
               let widest = ref(a[0] +. 2. *. Float.pi -. a[k - 1]);
               let from = ref(a[k - 1]);
               for (t in 0 to k - 2) {
                 if (a[t + 1] -. a[t] > widest^) {
                   widest := a[t + 1] -. a[t];
                   from := a[t];
                 };
               };
               if (widest^ < enclosed) {
                 let out = from^ +. widest^ /. 2.;
                 move(d.body, nudge *. cos(out), nudge *. sin(out));
               };
             }
           );
      };
    };

  for (_ in 1 to rounds(s)) {
    pull();
    part();
    ignore(clear(~crowded=false));
  };
  for (_ in 1 to settle) {
    part();
    expel(clear(~crowded=true));
    ignore(clear(~crowded=true));
  };
  Array.to_list(
    Array.map(
      (d: disc) =>
        (
          d.key,
          (
            d.at.x +. snap(bx[d.body] -. start_x[d.body]),
            d.at.y +. snap(by[d.body] -. start_y[d.body]),
          ),
        ),
      discs,
    ),
  );
};
