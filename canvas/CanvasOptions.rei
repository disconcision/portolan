/* CanvasOptions — the layouts and edge styles a reader can switch between.

   The choice is kept in the lab's two settings
   (`CanvasLayoutExperiments.mode` and `.wires`), which `CanvasLayout`
   already reads and keys its memo on. The lab's own options are listed
   here beside Portolan's: three layouts (compact, ledger, clusters) and two
   edge styles (metro, straight). `CanvasLayout` calls `arrange`, `place`,
   `route` and `path` at places marked PORTOLAN. */

type point = CanvasLayoutExperiments.point;

/* (id, label), in the order a menu shows them. The first is the default. */
let layouts: list((string, string));
let edge_styles: list((string, string));

/* The ids in use. */
let layout: unit => string;
let edges: unit => string;

/* Choose by id. False, and nothing changes, for an id not on offer. */
let set_layout: string => bool;
let set_edges: string => bool;

/* The pane's width over its height: what the packing layouts aim for. */
let aspect: ref(float);

/* How the dotted "made of" lines are drawn: "straight" (chords, as Hazel
   draws them), "routed" (in the edge style, where that is "metro": they take
   lanes beside the arrows), "select" (only those of the selected node) or
   "off". */
let link_styles: list((string, string));
let links: ref(string);
let set_links: string => bool;
/* A "made of" line from one node to another, as an edge for [route]. */
let link_name: (string, string) => string;
let is_link_name: string => bool;

/* What `CanvasLayoutExperiments.place("components", items, links, [])`
   returns (the default layout's packing of separate pieces), computed with
   tables instead of list searches. */
let components:
  (list(CanvasLayoutExperiments.item), list((string, string))) =>
  list(CanvasLayoutExperiments.item);

/* "compact", before the layers are solved: the graph with every unnamed
   type that is used in one place marked as a satellite of that place, so it
   docks there instead of taking a row in a layer of its own. */
let dock_single_use: CanvasGraph.t => CanvasGraph.t;

/* "compact", after the layers are solved (unstretched): the separate
   pieces moved together, in source order, in rows that fill the pane's
   shape. Each piece keeps its inner arrangement. */
let arrange:
  (CanvasGraph.t, list(CanvasLayoutExperiments.item)) =>
  list(CanvasLayoutExperiments.item);

/* True for a layout that places every node itself ("ledger", "clusters"):
   the layered engine is then not run at all. */
let places: string => bool;

/* Where every node goes, by key.
   "ledger": each shared type heads a lane; what makes it sits in rows on
   its left and what is made from it in rows on its right, in source
   order; lanes fill columns, a type after the types it is made from.
   "clusters": the same groups with their rows bent into arcs around the
   type. `radius` is a node's drawn radius. */
let place:
  (~radius: string => float, string, CanvasGraph.t) => list((string, point));

/* What an edge is drawn as: a polyline through `points` (rounded when
   drawn), or, when `points` is empty, a curve with the two control points
   `ctrl`. `label` is where its name would best sit. */
type wire = {
  points: list(point),
  ctrl: option((point, point)),
  label: point,
};

/* True when the edge style in use routes edges here ("metro", "straight"). */
let routes: unit => bool;

/* Routes for the edges (name, source key, target key) between `nodes`
   (key, centre, radius), in the style in use.
   "metro": level runs joined by short diagonals, with edges that share a
   node's side leaving it side by side. `room` is the length an edge's
   label needs on a level run.
   "straight": chords; edges between the same two nodes, or beside one of
   the `links` (a "made of" line), bow apart. */
let route:
  (
    ~nodes: list((string, point, float)),
    ~links: list((string, string)),
    ~room: string => float,
    list((string, string, string))
  ) =>
  list((string, wire));

/* The SVG path of a polyline, its corners rounded as the style in use
   rounds them. */
let path: list(point) => string;
