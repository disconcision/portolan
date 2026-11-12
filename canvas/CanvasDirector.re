open Virtual_dom.Vdom;
open Util.WebUtil;

/* CanvasDirector — the canvas's orchestrator outside Hazel.

   It holds the previous snapshot and layout; on a new snapshot plus a
   cause it queues a beat (CanvasBuffer's pacing), and each render lays the
   paced graph out, diffs it against the previous render into a
   CanvasScore.diff, plans the score, validates it and plays it
   (CanvasEnact), with the avatar, camera, bubbles and ripple field as in
   Hazel.

   This replaces what CanvasSidebar + CanvasPresentation did for the
   canvas itself. The sections marked LIFTED are taken from Hazel's
   CanvasSidebar.re (commit in UPSTREAM.md) with the Hazel model swapped
   for this module's state; they are kept in upstream's order and wording
   so a diff against CanvasSidebar.view_impl stays readable. Left out:
   authoring, probes and value cards, the info panel, the toolbar, the
   replay panel, settings persistence. */

open Js_of_ocaml;

/* ---------- what the host says ---------- */

type cause = {
  label: string, /* the tool or step that produced the snapshot */
  site: option(string), /* the definition it acted on (qualified name) */
  ok: bool,
};

/* the one board: a reset starts a new "slide" so every per-slide memory
   (previous snapshot, frame, camera exposure, avatar site) starts over,
   exactly as a slide switch does upstream */
let slide_no: ref(int) = ref(0);
let current_slide = (): string => "board/" ++ string_of_int(slide_no^);

/* the newest snapshot the host pushed (upstream: the live editor) */
let live: ref(CanvasGraph.t) = ref(CanvasGraph.empty);
/* the last cause: where the avatar rests between scores */
let last_cause: ref(option((cause, float))) = ref(None);

/* settings (upstream: globals.settings.canvas_*) */
let pace: ref(bool) = ref(true);
let follow: ref(bool) = ref(true);
let canvas_zoom: ref(float) = ref(1.);
/* the host's agent is working (upstream: awaiting a reply) */
let busy: ref(bool) = ref(false);
/* streamed reasoning for the thought cloud */
let thought: ref(string) = ref("");

/* selection (upstream: sidebar.canvas_focus / canvas_focus_ty) */
let focused: ref(option(string)) = ref(None);
let focused_ty: ref(option(string)) = ref(None);
let focused_value: ref(option(string)) = ref(None);
let hovered_edge: ref(option(string)) = ref(None);
/* PORTOLAN: a double-click on a node: the host opens it (name, kind) */
let on_open: ref((string, string) => unit) = ref((_, _) => ());
/* (name, kind, where) of what was clicked; kind is "function", "type",
   "module", "value", "test", "builtin", "derived", "ghost" or "product" */
let on_select: ref((string, string, option(CanvasGraph.handle)) => unit) =
  ref((_, _, _) => ());

/* manual arrangement (upstream: settings.canvas_node_offsets), per slide */
let node_offsets: ref(list((string, (float, float)))) = ref([]);

/* ---------- render loop ---------- */

let render_fwd: ref(unit => unit) = ref(() => ());
let render_queued: ref(bool) = ref(false);
/* upstream: globals.inject_global(Set(CanvasTick)) */
let request_render = (): unit =>
  if (! render_queued^) {
    render_queued := true;
    ignore(
      Dom_html.window##requestAnimationFrame(
        Js.wrap_callback(_ => {
          render_queued := false;
          render_fwd^();
        }),
      ),
    );
  };
let tick: Effect.t(unit) = Effect.of_sync_fun(request_render, ());

let schedule_tick = (delay: float): unit => {
  let cb =
    Js.Unsafe.callback(() => {
      CanvasBuffer.tick_fired();
      render_fwd^();
    });
  ignore(Js.Unsafe.global##setTimeout(cb, delay));
};

/* ---------- LIFTED: CanvasSidebar.re top-level state ---------- */

let pan_slack = CanvasRipple.pan_slack;
/* set to force a re-anchor on the next render (fit button) */
let pending_anchor: ref(bool) = ref(false);
/* where anchoring scrolls to; recomputed each render so the content
   sits centered in the pane (infinite canvas: centering can borrow
   the slack when the graph is smaller than the pane) */
let anchor_target: ref((float, float)) = ref((pan_slack, pan_slack));

/* pinch-zoom plumbing: the wheel listener must be non-passive (to
   preventDefault the browser's page zoom on ctrl+wheel), so it is
   installed raw on the scroll element; these refs carry the current
   zoom and dispatcher across renders */
let zoom_now = CanvasCamera.zoom_now;
let zoom_send = CanvasCamera.zoom_send;
let install_zoom_listener = (): unit => {
  switch (Util.JsUtil.get_elem_by_id_opt("canvas-scroll")) {
  | None => ()
  | Some(el) =>
    let el' = Js.Unsafe.coerce(el);
    let installed: bool =
      Js.Optdef.test(Js.Unsafe.get(el', "__zoomInstalled"));
    if (!installed) {
      Js.Unsafe.set(el', "__zoomInstalled", Js.bool(true));
      /* panning and pane resizes must redraw the viewport-fixed dot
         field (a resize otherwise leaves a stale, mis-scaled frame
         until the next scroll) */
      ignore(
        Js.Unsafe.meth_call(
          el',
          "addEventListener",
          [|
            Js.Unsafe.inject(Js.string("scroll")),
            Js.Unsafe.inject(
              Js.Unsafe.callback(() => {
                CanvasCamera.note_scroll();
                CanvasRipple.request_draw();
              }),
            ),
            Js.Unsafe.inject(
              Js.Unsafe.obj([|
                ("passive", Js.Unsafe.inject(Js.bool(true))),
              |]),
            ),
          |],
        ),
      );
      if (Js.Optdef.test(Js.Unsafe.get(Js.Unsafe.global, "ResizeObserver"))) {
        let obs =
          Js.Unsafe.new_obj(
            Js.Unsafe.get(Js.Unsafe.global, "ResizeObserver"),
            [|
              Js.Unsafe.inject(
                Js.Unsafe.callback(() => {
                  CanvasRipple.request_draw();
                  /* PORTOLAN: no app loop re-renders on a resize here */
                  request_render();
                }),
              ),
            |],
          );
        ignore(
          Js.Unsafe.meth_call(obs, "observe", [|Js.Unsafe.inject(el')|]),
        );
      };
      let last = ref(0.);
      let pending_dy = ref(0.);
      let anim_off_until = ref(0.);
      let cb =
        Js.Unsafe.callback((evt: Js.t(Js.Unsafe.any)) => {
          let ctrl: bool = Js.to_bool(Js.Unsafe.coerce(evt)##.ctrlKey);
          if (ctrl) {
            ignore(Js.Unsafe.meth_call(evt, "preventDefault", [||]));
            let dy: float = Js.Unsafe.coerce(evt)##.deltaY;
            let now: float =
              Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now();
            /* accumulate between throttle windows: dropping deltas
               made pinching feel sluggish */
            pending_dy := pending_dy^ +. dy;
            if (now -. last^ > 30.) {
              last := now;
              let z0 = zoom_now^;
              let z = z0 *. exp(-. pending_dy^ *. 0.018);
              pending_dy := 0.;
              /* detent at 1:1 so pinching back to normal lands exactly */
              let z = abs_float(z -. 1.) < 0.05 ? 1. : z;
              let z = CanvasZoom.clamp(z);
              switch (zoom_send^) {
              | Some(send) =>
                /* apply the zoom IMPERATIVELY and correct the scroll
                   in the same frame: the old flow (state update, then
                   a delayed correction) let the anchor point jump and
                   snap back on every step — the pinch judder. The
                   auto-fit zoom transition is suppressed while a
                   pinch is live for the same reason. */
                let root =
                  Js.Unsafe.meth_call(
                    Js.Unsafe.global##.document,
                    "querySelector",
                    [|Js.Unsafe.inject(Js.string(".canvas-root"))|],
                  );
                switch (Js.Opt.to_option(root)) {
                | None => send(z)
                | Some(root) =>
                  let cl =
                    Js.Unsafe.get(Js.Unsafe.coerce(root), "classList");
                  if (now > anim_off_until^) {
                    ignore(
                      Js.Unsafe.meth_call(
                        cl,
                        "add",
                        [|Js.Unsafe.inject(Js.string("no-zoom-anim"))|],
                      ),
                    );
                  };
                  anim_off_until := now +. 250.;
                  ignore(
                    Js.Unsafe.global##setTimeout(
                      Js.Unsafe.callback(() =>
                        if (Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now()
                            >= anim_off_until^) {
                          ignore(
                            Js.Unsafe.meth_call(
                              cl,
                              "remove",
                              [|
                                Js.Unsafe.inject(Js.string("no-zoom-anim")),
                              |],
                            ),
                          );
                        }
                      ),
                      260,
                    ),
                  );
                  Js.Unsafe.coerce(root)##.style##.zoom :=
                    Js.string(Printf.sprintf("%.4f", z));
                  let rect =
                    Js.Unsafe.meth_call(el', "getBoundingClientRect", [||]);
                  /* measure from the scroll ORIGIN (inside the
                     border), not the border box */
                  let mx: float =
                    Js.Unsafe.coerce(evt)##.clientX
                    -.
                    Js.Unsafe.coerce(rect)##.left
                    -.
                    Js.Unsafe.coerce(el')##.clientLeft;
                  let my: float =
                    Js.Unsafe.coerce(evt)##.clientY
                    -.
                    Js.Unsafe.coerce(rect)##.top
                    -.
                    Js.Unsafe.coerce(el')##.clientTop;
                  let sl: float = Js.Unsafe.coerce(el')##.scrollLeft
                  and st: float = Js.Unsafe.coerce(el')##.scrollTop;
                  let r = z /. z0;
                  /* the pan slack doesn't scale with zoom, so the
                     fixed-point math runs in board coordinates */
                  Js.Unsafe.coerce(el')##.scrollLeft :=
                    (sl +. mx -. pan_slack) *. r +. pan_slack -. mx;
                  Js.Unsafe.coerce(el')##.scrollTop :=
                    (st +. my -. pan_slack) *. r +. pan_slack -. my;
                  zoom_now := z;
                  CanvasRipple.zoom := z;
                  CanvasCamera.note_user_zoom();
                  CanvasRipple.request_draw();
                  send(z);
                };
              | None => ()
              };
            };
          };
        });
      ignore(
        Js.Unsafe.meth_call(
          el',
          "addEventListener",
          [|
            Js.Unsafe.inject(Js.string("wheel")),
            Js.Unsafe.inject(cb),
            Js.Unsafe.inject(
              Js.Unsafe.obj([|
                ("passive", Js.Unsafe.inject(Js.bool(false))),
              |]),
            ),
          |],
        ),
      );
    };
  };
};

/* fit: one camera animation moving zoom AND scroll together so the
   graph center stays pinned at the pane center throughout */
let animate_fit =
    (~z_to: float, ~lw: float, ~lh: float, ~aw: float, ~ah: float): unit =>
  CanvasCamera.animate(
    ~aw,
    ~ah,
    ~zoom=Some(z_to),
    ~dur=320.,
    ~easing=CanvasCamera.EaseOut,
    (lw /. 2., lh /. 2.),
  );
let last_anchor_slide: ref(string) = ref("");
/* anchor the scroll to the board (skipping the slack margin) whenever
   the pane is fresh, the slide changed, or an anchor was requested */
let ensure_scroll_anchor = (slide: string): unit =>
  ignore(
    Dom_html.window##requestAnimationFrame(
      Js.wrap_callback(_ => {
        switch (Util.JsUtil.get_elem_by_id_opt("canvas-scroll")) {
        | None => ()
        | Some(el) =>
          let el' = Js.Unsafe.coerce(el);
          let fresh = !Js.Optdef.test(Js.Unsafe.get(el', "__panAnchored"));
          if (fresh || last_anchor_slide^ != slide || pending_anchor^) {
            Js.Unsafe.set(el', "__panAnchored", Js.bool(true));
            last_anchor_slide := slide;
            pending_anchor := false;
            CanvasCamera.mark_driving();
            let (tx, ty) = anchor_target^;
            el'##.scrollLeft := tx;
            el'##.scrollTop := ty;
          };
        }
      }),
    ),
  );

/* avatar continuity: hold the last successfully resolved site so the
   avatar persists through interstitials (turn start, pathless tools,
   paths gone stale mid-burst) instead of blinking out */
let last_avatar_id: ref(option(string)) = ref(None: option(string));

/* While a node drag is in progress the pane measurements are FROZEN:
   any mid-drag change (scrollbar appearing, overlay quirks, content
   growth) would otherwise re-derive the fit/centering frame and bounce
   every node between adjacent grid cells under the user's cursor. */
let drag_active: ref(bool) = ref(false);
let cached_avail_w: ref(option(float)) = ref(None: option(float));
let cached_avail_h: ref(option(float)) = ref(None: option(float));

/* Sticky frame for manually-arranged slides: pins/offsets are stored in
   the pre-normalization frame, so re-deriving the frame from a changed
   program re-anchors EVERY manual position (placement pins landed away
   from the click; pinned nodes flew off-pane on unrelated edits). Once a
   slide has any manual position, its frame (origin + scales) is frozen
   here — re-derived only on pane resize, slide switch, or reset layout. */
type frame_cache = {
  fc_slide: string,
  fc_burst: int, /* the burst it was derived in (CanvasLog turn) */
  fc_nodes: int, /* nodes in the graph it was derived from */
  fc_origin: CanvasLayout.pos,
  fc_x_scale: float,
  fc_y_scale: float,
  fc_avail_w: float,
};
let cached_frame: ref(option(frame_cache)) =
  ref(None: option(frame_cache));

/* the previous render's layout, and the elements a score is still erasing:
   a removed node or arrow stays on the board as a ghost (rendered from
   this layout, at its old place) until its act's cue — A2 for removals */
let last_layout: ref(option(CanvasLayout.t)) = ref(Option.none);
type leaving = {
  l_nodes: list(CanvasLayout.node_layout),
  l_edges: list(CanvasLayout.edge_layout),
  l_until: float /* ms; dropped from the render after this */
};
let leaving: ref(option(leaving)) = ref(Option.none);

/* an element's LOOK: what a viewer would notice changing without an
   actor (A2 for modifications). Tests/values from evaluation are not
   the agent's edit and are exempt. */
let node_look = (n: CanvasGraph.tynode) => (
  n.label,
  n.ctrs,
  n.n_ty,
  n.n_err,
  n.deps,
);
let edge_look = (e: CanvasGraph.edge) => (
  e.e_label,
  e.e_ty,
  e.e_err,
  e.e_hole,
  e.e_src,
  e.dst,
  e.e_deps,
);
/* a changed element renders with its OLD record until its act's cue
   (absolute ms), then swaps with a pulse */
type staged = {
  s_nodes: list((string, float, CanvasGraph.tynode)),
  s_edges: list((string, float, CanvasGraph.edge)),
};
let staged: ref(staged) =
  ref({
    s_nodes: [],
    s_edges: [],
  });

/* value badges present at the previous render, for the score's diff */
let last_value_snapshot: ref((string, list(string))) = ref(("", []));
/* values share the score's node namespace under a prefix */
let value_score_key = (v_name: string): string => "val:" ++ v_name;
let value_of_score_key = (k: string): option(string) =>
  String.length(k) > 4 && String.sub(k, 0, 4) == "val:"
    ? Some(String.sub(k, 4, String.length(k) - 4)) : None;

let last_placed: ref(list((string, float))) = ref([]);
let note_placed = (key: string): unit =>
  last_placed :=
    [(key, CanvasBuffer.now())]
    @ List.filter(((k, _)) => k != key, last_placed^);

/* previous render's nodes, for removal detection: a node that vanishes
   gets a suction ripple (negative amplitude) at its last position.
   Slide switches and wholesale rewrites are gated by the removal count. */
let last_node_snapshot: ref((string, list((string, (float, float))))) =
  ref(("", []));
/* previous render's function edges (by name), for the arrival
   choreography: new edges during an agent beat are drawn by the avatar */
let last_edge_snapshot: ref((string, list(string))) = ref(("", []));

/* collapsed module hulls (root module names); toggled from the hull
   label */
let collapsed_modules: ref(list(string)) = ref([]);
/* collapse/expand transitions must not read as node deletions: the
   removal differ skips its suctions inside this window */
let collapse_fx_until: ref(float) = ref(0.);

/* hide a collapsed module's internals pre-layout: members, their edges
   and orbit values, and the satellite terminals their functions grew.
   The former node and the module's own value stay. Returns the graph +
   (root, hidden-member count) pairs for the hull label. */
let collapse_filter =
    (g: CanvasGraph.t): (CanvasGraph.t, list((string, int))) =>
  if (collapsed_modules^ == []) {
    (g, []);
  } else {
    let hidden = r => List.mem(r, collapsed_modules^);
    let member_edges =
      List.filter(
        (e: CanvasGraph.edge) =>
          switch (e.m_path) {
          | [r, ..._] => hidden(r)
          | [] => false
          },
        g.edges,
      );
    let member_values =
      List.filter(
        (v: CanvasGraph.value) =>
          switch (v.m_path) {
          | [r, ..._] => hidden(r)
          | [] => false
          },
        g.values,
      );
    let member_qnames =
      List.map((e: CanvasGraph.edge) => e.e_name, member_edges)
      @ List.map((v: CanvasGraph.value) => v.v_name, member_values);
    let is_member_sat = (n: CanvasGraph.tynode): bool =>
      switch (n.sat) {
      | Some(_) =>
        List.exists(
          qn => {
            let suffix = "@" ++ qn;
            let ln = String.length(n.key)
            and ls = String.length(suffix);
            ln >= ls && String.sub(n.key, ln - ls, ls) == suffix;
          },
          member_qnames,
        )
      | None => false
      };
    let drop_node = (n: CanvasGraph.tynode): bool =>
      switch (n.m_path) {
      | [r, ..._] => hidden(r) && n.key != "{}@" ++ r
      | [] => is_member_sat(n)
      };
    let counts =
      List.map(
        r => {
          let c =
            List.length(
              List.filter(
                (n: CanvasGraph.tynode) =>
                  switch (n.m_path) {
                  | [r', ..._] => r' == r && n.key != "{}@" ++ r
                  | [] => false
                  },
                g.nodes,
              ),
            )
            + List.length(
                List.filter(
                  (e: CanvasGraph.edge) =>
                    switch (e.m_path) {
                    | [r', ..._] => r' == r
                    | [] => false
                    },
                  g.edges,
                ),
              )
            + List.length(
                List.filter(
                  (v: CanvasGraph.value) =>
                    switch (v.m_path) {
                    | [r', ..._] => r' == r
                    | [] => false
                    },
                  g.values,
                ),
              );
          (r, c);
        },
        collapsed_modules^,
      );
    (
      {
        ...g,
        nodes: List.filter(n => !drop_node(n), g.nodes),
        edges:
          List.filter(
            (e: CanvasGraph.edge) => !List.mem(e, member_edges),
            g.edges,
          ),
        values:
          List.filter(
            (v: CanvasGraph.value) => !List.mem(v, member_values),
            g.values,
          ),
      },
      counts,
    );
  };

let last_avatar_pos: ref(option(CanvasLayout.pos)) = ref(None);
let last_followed_id: ref(option(string)) = ref(None);
let last_followed: ref(option(CanvasLayout.pos)) = ref(None);
let last_followed_busy: ref(bool) = ref(false);
let last_seen_slide: ref(string) = ref("");
let last_pacing_live: ref(bool) = ref(false);
let last_logged_avatar: ref((option(string), string)) = ref((None, "off"));
let last_logged_busy: ref(bool) = ref(false);

/* collapse/uncollapse memo: the layout memo keys on the graph's physical
   identity, so the filtered graph must be the same value while its
   inputs are */
let collapse_cache:
  ref(
    option(
      (CanvasGraph.t, list(string), (CanvasGraph.t, list((string, int)))),
    ),
  ) =
  ref(None);
let collapse_cached = (g: CanvasGraph.t) =>
  switch (collapse_cache^) {
  | Some((g0, c0, r)) when g0 === g && c0 == collapsed_modules^ => r
  | _ =>
    let r = collapse_filter(g);
    collapse_cache := Some((g, collapsed_modules^, r));
    r;
  };

/* beat weight for pacing: graph elements (by stable name) that differ
   between two states — nodes by key, edges by qualified name */
let graph_delta = (a: CanvasGraph.t, b: CanvasGraph.t): int => {
  let keys = (g: CanvasGraph.t) =>
    List.map((n: CanvasGraph.tynode) => n.key, g.nodes)
    @ List.map((e: CanvasGraph.edge) => "e:" ++ e.e_name, g.edges);
  let ka = keys(a)
  and kb = keys(b);
  List.length(List.filter(k => !List.mem(k, kb), ka))
  + List.length(List.filter(k => !List.mem(k, ka), kb));
};

/* a card just opened / closed / resized: re-fit the camera after the
   render that lays it out */
let fit_pending: ref(bool) = ref(false);
let overview_pending: ref(bool) = ref(false);
/* PORTOLAN: nothing has been framed since the board was cleared: the
   first framing is where the camera starts, not a move from where the
   last board left it */
let fresh_board: ref(bool) = ref(true);
/* PORTOLAN: a glide the reader asked for (a layout chosen, a module
   opened in place) is slower than an agent's, to be followed by eye */
let reader_ms = 800;
/* the render right after a burst ends re-frames the graph: that one big
   move is the tidy act. A big move the USER caused is not scored: it
   plays on the staged beat at once, not after an actor's drift. */
let burst_end_reframe: ref(bool) = ref(false);

/* ---- imperative layout follow (node drag): apply a layout's geometry
   straight to the DOM — no vdom, no render ---- */
module LayoutDom = {
  let set_attr = (el, name: string, v: string) =>
    ignore(
      Js.Unsafe.meth_call(
        el,
        "setAttribute",
        [|
          Js.Unsafe.inject(Js.string(name)),
          Js.Unsafe.inject(Js.string(v)),
        |],
      ),
    );
  let set_pos = (id: string, x: float, y: float) =>
    switch (Util.JsUtil.get_elem_by_id_opt(id)) {
    | Some(el) =>
      let st = Js.Unsafe.coerce(el)##.style;
      st##.left := Js.string(Printf.sprintf("%.1fpx", x));
      st##.top := Js.string(Printf.sprintf("%.1fpx", y));
    | None => ()
    };
  let fmt' = (v: float) => Printf.sprintf("%f", v);
  let pull_back = (p: CanvasLayout.pos, c: CanvasLayout.pos, d: float) => {
    let vx = p.x -. c.x
    and vy = p.y -. c.y;
    let len = max(1., Float.hypot(vx, vy));
    CanvasLayout.{
      x: p.x -. vx /. len *. d,
      y: p.y -. vy /. len *. d,
    };
  };
  let apply_layout = (l: CanvasLayout.t): unit => {
    List.iter(
      (nl: CanvasLayout.node_layout) =>
        set_pos(CanvasView.node_dom_id(nl.node.key), nl.p.x, nl.p.y),
      l.nodes,
    );
    List.iter(
      (el: CanvasLayout.edge_layout) => {
        let e = el.edge;
        if (el.endo) {
          switch (
            Util.JsUtil.get_elem_by_id_opt(
              "corbit-" ++ CanvasView.sanitize(e.e_name),
            )
          ) {
          | Some(c) =>
            set_attr(c, "cx", fmt'(el.dst_p.x));
            set_attr(c, "cy", fmt'(el.dst_p.y));
          | None => ()
          };
        } else {
          switch (
            Util.JsUtil.get_elem_by_id_opt(
              "cpath-" ++ CanvasView.sanitize(e.e_name),
            )
          ) {
          | Some(path_el) =>
            set_attr(path_el, "d", CanvasLayout.edge_path(~pull=9., el))
          | None => ()
          };
        };
        set_pos(
          CanvasView.edge_dom_id(e.e_name),
          el.label_p.x,
          el.label_p.y,
        );
        switch (
          Util.JsUtil.get_elem_by_id_opt(
            "clead-" ++ CanvasView.sanitize(e.e_name),
          )
        ) {
        | Some(lel) =>
          set_attr(lel, "x1", fmt'(el.label_p.x));
          set_attr(lel, "y1", fmt'(el.label_p.y -. 8.));
          set_attr(lel, "x2", fmt'(el.label_anchor.x));
          set_attr(lel, "y2", fmt'(el.label_anchor.y));
        | None => ()
        };
      },
      l.edges,
    );
    List.iteri(
      (
        _i,
        (a, b, cp, pp): (string, string, CanvasLayout.pos, CanvasLayout.pos),
      ) => {
        let pp' = pull_back(pp, cp, 5.);
        switch (
          Util.JsUtil.get_elem_by_id_opt(CanvasView.formation_dom_id(a, b))
        ) {
        | Some(el) =>
          set_attr(
            el,
            "d",
            CanvasLayout.relation_path(
              ~nodes=l.nodes,
              ~from_key=a,
              ~to_key=b,
              cp,
              pp',
            ),
          )
        | None => ()
        };
      },
      l.formations,
    );
    List.iteri(
      (
        _i,
        (a, b, dp, tp): (string, string, CanvasLayout.pos, CanvasLayout.pos),
      ) => {
        let tp' = pull_back(tp, dp, 5.);
        switch (Util.JsUtil.get_elem_by_id_opt(CanvasView.dep_dom_id(a, b))) {
        | Some(el) =>
          set_attr(
            el,
            "d",
            CanvasLayout.relation_path(
              ~nodes=l.nodes,
              ~from_key=a,
              ~to_key=b,
              dp,
              tp',
            ),
          )
        | None => ()
        };
      },
      l.dep_links,
    );
    List.iter(
      (vl: CanvasLayout.value_layout) =>
        set_pos(CanvasView.value_dom_id(vl.value.v_name), vl.p.x, vl.p.y),
      l.values,
    );
    /* hull sausages track exactly (no springs for paths) */
    List.iter(
      path =>
        List.iter(
          ((id, d, _)) =>
            switch (Util.JsUtil.get_elem_by_id_opt(id)) {
            | Some(el) => set_attr(el, "d", d)
            | None => ()
            },
          CanvasView.hull_sausages_at(l, path),
        ),
      CanvasView.hull_paths_of(l),
    );
    CanvasJelly.set_targets(CanvasView.hull_targets(l));
  };
};

let px_float = (s: string): float =>
  try(float_of_string(String.sub(s, 0, String.length(s) - 2))) {
  | _ => 0.
  };

/* ---------- PORTOLAN: the avatar's site, by name ---------- */

/* The agent's current work site: the last cause's definition (upstream
   reads the last tool result's jump path). */
let avatar_target = (): option((string, string)) =>
  switch (last_cause^) {
  | Some(({site: Some(site), ok, _}, landed_at)) =>
    let agent_busy = busy^ || CanvasBuffer.fake_busy();
    /* "edit" only around a landed edit (a score playing it, or one
       just landed); a busy agent is otherwise THINKING */
    let editing =
      CanvasBuffer.score_playing() || CanvasBuffer.now() -. landed_at < 1500.;
    let state =
      if (!ok) {
        "err";
      } else if (agent_busy && editing) {
        "edit";
      } else {
        "";
      };
    Some((site, state));
  | _ => None
  };

/* Map a definition's name to a canvas position: an exact hit on an
   edge / node / value, then looser readings of the name (a path written
   with '/', a module, a bare unqualified name). Upstream climbs statics
   ancestors instead. */
let locate = (lay: CanvasLayout.t, id: string): option(CanvasLayout.pos) => {
  let dots = String.map(c => c == '/' ? '.' : c);
  let id' = dots(id);
  let edge = (pred: CanvasGraph.edge => bool, ()) =>
    List.find_opt(
      (el: CanvasLayout.edge_layout) => pred(el.edge),
      lay.edges,
    )
    |> Option.map((el: CanvasLayout.edge_layout) => el.label_p);
  let node = (pred: CanvasGraph.tynode => bool, ()) =>
    List.find_opt(
      (nl: CanvasLayout.node_layout) => pred(nl.node),
      lay.nodes,
    )
    |> Option.map((nl: CanvasLayout.node_layout) => nl.p);
  let value = (pred: CanvasGraph.value => bool, ()) =>
    List.find_opt(
      (vl: CanvasLayout.value_layout) => pred(vl.value),
      lay.values,
    )
    |> Option.map((vl: CanvasLayout.value_layout) => vl.p);
  List.find_map(
    f => f(),
    [
      edge(e => e.e_id == id),
      node(n => n.n_id == Some(id)),
      value(v => v.v_id == id),
      edge(e => dots(e.e_name) == id'),
      node(n => dots(n.key) == id'),
      value(v => dots(v.v_name) == id'),
      node(n => n.key == "{}@" ++ id'),
      edge(e => e.e_label == id),
      node(n => n.sat == None && n.label == id),
      value(v => v.v_label == id),
    ],
  );
};

let kind_name = (n: CanvasGraph.tynode): string =>
  String.length(n.key) >= 3 && String.sub(n.key, 0, 3) == "{}@"
    ? "module"
    : (
      switch (n.kind) {
      | Alias => "type"
      | Builtin => "builtin"
      | Derived => "derived"
      | Ghost => "ghost"
      | Product => "product"
      }
    );

/* a module's node is named for its module */
let node_name = (n: CanvasGraph.tynode): string =>
  kind_name(n) == "module"
    ? String.sub(n.key, 3, String.length(n.key) - 3) : n.key;

/* ---------- the view (LIFTED from CanvasSidebar.view_impl) ---------- */

let view = (graph0: CanvasGraph.t): Node.t => {
  let slide = current_slide();
  CanvasLayoutExperiments.scene := slide;
  {
    /* the layout frame is frozen while beats play and re-derives when
       they stop: that re-frame must glide, not snap */
    let pl = CanvasBuffer.pacing_live();
    if (last_pacing_live^ && !pl) {
      CanvasBuffer.stage_beat(~slow=true, ());
      burst_end_reframe := true;
    };
    last_pacing_live := pl;
  };
  if (last_seen_slide^ != slide) {
    /* per-slide state: a site remembered from the previous slide put the
       avatar off the new board */
    last_seen_slide := slide;
    last_avatar_pos := None;
    last_avatar_id := None;
    last_followed := None;
    last_followed_id := None;
    CanvasCamera.reset_exposure();
    CanvasBuffer.avatar_site := None;
    last_followed_busy := false;
    CanvasBuffer.beat_avatar := None;
    CanvasCamera.roi := [];
  };
  /* the graph MUST derive from the PACED snapshot */
  let (graph, collapsed_counts) = collapse_cached(graph0);
  CanvasEnact.label_of :=
    (
      k =>
        switch (value_of_score_key(k)) {
        | Some(v) => v
        | None =>
          switch (
            List.find_opt((n: CanvasGraph.tynode) => n.key == k, graph.nodes)
          ) {
          | Some(n) =>
            /* a module's former: its name; a terminal: its type */
            String.length(k) >= 3 && String.sub(k, 0, 3) == "{}@"
              ? String.sub(k, 3, String.length(k) - 3) : n.label
          | None => CanvasGraph.display_label(k)
          }
        }
    );
  let zoom = canvas_zoom^;
  zoom_now := zoom;
  CanvasBuffer.canvas_zoom := zoom;
  CanvasRipple.zoom := zoom;
  zoom_send :=
    Some(
      z => {
        canvas_zoom := z;
        request_render();
      },
    );
  install_zoom_listener();
  CanvasCamera.install_testers();
  CanvasEnact.install_testers();
  let offsets = node_offsets^;
  let pins = [];
  /* stretch columns to fill the panel when the graph is narrower than it;
     panel width is read from the (pre-patch) DOM, so the first render
     after a resize uses the previous width */
  let avail_width =
    switch (Util.JsUtil.get_elem_by_id_opt("canvas-scroll")) {
    | Some(el) =>
      let w = Js.Unsafe.coerce(el)##.clientWidth;
      w > 50 ? Some(float_of_int(w)) : None;
    | None => None
    };
  let (avail_height, layout_height) =
    switch (Util.JsUtil.get_elem_by_id_opt("canvas-scroll")) {
    | Some(el) =>
      let h: int = Js.Unsafe.coerce(el)##.clientHeight;
      (
        h > 100 ? Some(float_of_int(h)) : None,
        h > 100 ? Some(float_of_int(h)) : None,
      );
    | None => (None, None)
    };
  /* frame freeze during drags (see drag_active) */
  let avail_width =
    if (drag_active^) {
      switch (cached_avail_w^) {
      | Some(_) as c => c
      | None => avail_width
      };
    } else {
      cached_avail_w := avail_width;
      avail_width;
    };
  let avail_height =
    if (drag_active^) {
      switch (cached_avail_h^) {
      | Some(_) as c => c
      | None => avail_height
      };
    } else {
      cached_avail_h := avail_height;
      avail_height;
    };
  CanvasLayout.card_extents := [];
  /* PORTOLAN: the layouts that pack aim for the pane's shape (in coarse
     steps, so a small resize is not a new arrangement) */
  switch (avail_width, avail_height) {
  | (Some(w), Some(h)) =>
    CanvasOptions.aspect :=
      max(0.5, min(4., Float.round(w /. h *. 4.) /. 4.))
  | _ => ()
  };
  let lay = {
    /* pins/offsets for nodes that no longer exist must not keep a stale
       frame alive */
    let node_keys = List.map((n: CanvasGraph.tynode) => n.key, graph.nodes);
    let live = ((k, _)) => List.mem(k, node_keys);
    let manual = List.exists(live, offsets) || List.exists(live, pins);
    let aw = Option.value(~default=0., avail_width);
    /* the frame stays frozen for the whole burst (B2: a node keeps its
       place unless an act moves it); the burst-end re-frame is one tidy */
    let outgrown = CanvasBuffer.pacing_live() && !manual;
    /* a frozen frame that would push the new content past the pane is
       re-fit at this beat instead (the drift makes room first, so the
       whole graph glides to the new fit before anything blooms) */
    let fits_cached =
      switch (cached_frame^, avail_height) {
      | (Some(fc), Some(ah)) when !manual =>
        let l =
          CanvasLayout.layout(
            ~x_scale=fc.fc_x_scale,
            ~y_scale=fc.fc_y_scale,
            ~origin_override=Some(fc.fc_origin),
            ~cards=[],
            graph,
          );
        l.height <= ah -. 8. && l.width <= aw +. 40.;
      | _ => true
      };
    let runtime_frame =
      switch (cached_frame^) {
      | Some(fc)
          when
            fits_cached
            && (
              manual
              || outgrown
              /* a new burst keeps the frozen frame until it adds nodes;
                 re-deriving on the bare tool beat that opens a burst moved
                 everything mid-score */
              && (
                fc.fc_burst == CanvasLog.turn_no()
                || List.length(graph.nodes) <= fc.fc_nodes
              )
            )
            /* a frame derived from an empty graph (the bare tool beat
               before the first insertion) would spread the real one */
            && fc.fc_nodes > 0
            && fc.fc_slide == slide
            && Float.abs(fc.fc_avail_w -. aw) < 2. =>
        Some((fc.fc_origin, fc.fc_x_scale, fc.fc_y_scale))
      | _ => None
      };
    switch (runtime_frame) {
    | Some((origin, xs, ys)) =>
      /* sticky frame: manual positions never re-anchor (see cached_frame) */
      cached_frame :=
        Some({
          fc_slide: slide,
          fc_burst: CanvasLog.turn_no(),
          fc_nodes: List.length(graph.nodes),
          fc_origin: origin,
          fc_x_scale: xs,
          fc_y_scale: ys,
          fc_avail_w: aw,
        });
      CanvasLayout.layout(
        ~x_scale=xs,
        ~y_scale=ys,
        ~origin_override=Some(origin),
        ~offsets,
        ~pins,
        graph,
      );
    | None =>
      /* ALL frame decisions (fit scales, normalization origin) derive
         from the VIRGIN layout — no user offsets/pins — so dragging a
         node can never rescale or re-anchor the rest of the graph. The
         final layout applies the frozen frame plus the user's edits. */
      let virgin = CanvasLayout.layout(~cards=[], graph);
      let y_scale =
        switch (layout_height) {
        | Some(h) => min(2.1, max(1., (h -. 40.) /. virgin.height))
        | None => 1.
        };
      let x_scale =
        switch (avail_width) {
        | Some(avail) =>
          /* fill the pane, less a margin the camera can see as "all in" */
          let target = avail -. 56.;
          /* never tighter than the layout's natural size: the layout
             keeps its spacing and the camera does the fitting */
          let s1 = min(1.8, max(1., target /. virgin.width));
          if (s1 >= 1.8 || s1 <= 1.) {
            s1;
          } else {
            let v1 =
              CanvasLayout.layout(~x_scale=s1, ~y_scale, ~cards=[], graph);
            v1.width >= target -. 30. && v1.width <= target +. 30.
              ? s1 : min(1.8, max(1., s1 *. target /. v1.width));
          };
        | None => 1.
        };
      let framed =
        CanvasLayout.layout(
          ~x_scale,
          ~y_scale,
          ~center_within=avail_width,
          ~cards=[],
          graph,
        );
      cached_frame :=
        Some({
          fc_slide: slide,
          fc_burst: CanvasLog.turn_no(),
          fc_nodes: List.length(graph.nodes),
          fc_origin: framed.origin,
          fc_x_scale: x_scale,
          fc_y_scale: y_scale,
          fc_avail_w: aw,
        });
      /* the rendered layout: the frozen frame plus the user's edits */
      CanvasLayout.layout(
        ~x_scale,
        ~y_scale,
        ~origin_override=Some(framed.origin),
        ~offsets,
        ~pins,
        graph,
      );
    };
  };

  /* ---- selection (PORTOLAN: the host is told; no info panel) ---- */
  let select = (name: string, kind: string, id: option(string)) =>
    Effect.of_sync_fun(
      () =>
        on_select^(
          name,
          kind,
          Option.bind(id, CanvasGraph.handle_of(graph)),
        ),
      (),
    );
  let focus_dom = dom_id =>
    Effect.of_sync_fun(CanvasView.focus_selector, "#" ++ dom_id);
  let inject_jump = (id: string) =>
    Effect.Many([select(id, "test", Some(id)), Effect.Stop_propagation]);
  let on_edge_click = (e: CanvasGraph.edge) => {
    focused_value := None;
    focused := Some(e.e_name);
    focused_ty := None;
    Effect.Many([
      select(e.e_name, "function", Some(e.e_id)),
      tick,
      Effect.Stop_propagation,
      focus_dom(CanvasView.edge_dom_id(e.e_name)),
    ]);
  };
  let click_effect = (n: CanvasGraph.tynode) => {
    focused_value := None;
    focused_ty := Some(n.key);
    focused := None;
    Effect.Many([
      select(node_name(n), kind_name(n), n.n_id),
      tick,
      focus_dom(CanvasView.node_dom_id(n.key)),
    ]);
  };
  /* drag-vs-click on a node: document listeners move the div imperatively;
     release either commits a layout delta or fires the click */
  let start_node_drag =
      (n: CanvasGraph.tynode, evt: Js.t(Dom_html.mouseEvent))
      : Effect.t(unit) => {
    open LayoutDom;
    let sx: int = Js.Unsafe.coerce(evt)##.clientX;
    let sy: int = Js.Unsafe.coerce(evt)##.clientY;
    /* dragging while the actor works pauses the workload; it resumes on
       release, re-planned around the moved node */
    let during_score = CanvasBuffer.pacing_live();
    if (during_score) {
      CanvasEnact.pause_score();
    } else {
      /* a beat may still be morphing or revealing geometry (dash arrays
         left on paths would truncate them as they stretch): the drag takes
         over cleanly */
      List.iter(
        id =>
          switch (Util.JsUtil.get_elem_by_id_opt(id)) {
          | Some(el) =>
            let anims = Js.Unsafe.meth_call(el, "getAnimations", [||]);
            let len: int = Js.Unsafe.get(anims, "length");
            for (i in 0 to len - 1) {
              ignore(
                Js.Unsafe.meth_call(Js.Unsafe.get(anims, i), "cancel", [||]),
              );
            };
            ignore(
              Js.Unsafe.meth_call(
                el,
                "removeAttribute",
                [|Js.Unsafe.inject(Js.string("stroke-dasharray"))|],
              ),
            );
            let st = Js.Unsafe.get(el, "style");
            Js.Unsafe.set(st, "strokeDasharray", Js.string(""));
            Js.Unsafe.set(st, "strokeDashoffset", Js.string(""));
          | None => ()
          },
        Util.JsUtil.ids_with_prefix("cpath-")
        @ Util.JsUtil.ids_with_prefix("cform-")
        @ Util.JsUtil.ids_with_prefix("corbit-"),
      );
    };
    let base =
      Option.value(~default=(0., 0.), List.assoc_opt(n.key, offsets));
    let commit = ((dx, dy)) =>
      node_offsets :=
        [
          (n.key, (fst(base) +. dx, snd(base) +. dy)),
          ...List.remove_assoc(n.key, node_offsets^),
        ];
    let orig =
      switch (Util.JsUtil.get_elem_by_id_opt(CanvasView.node_dom_id(n.key))) {
      | Some(el) =>
        let st = Js.Unsafe.coerce(el)##.style;
        Some((
          el,
          px_float(Js.to_string(st##.left)),
          px_float(Js.to_string(st##.top)),
        ));
      | None => None
      };
    drag_active := true;
    let moved = ref(false);
    let delta = ref((0., 0.));
    /* EXACT drag follow: recompute the REAL layout with the pending
       delta (same frame + offsets the commit will use) and apply every
       piece of geometry imperatively — no vdom, no render, and nothing
       left to jump on drop. Hull circles go through the jelly springs as
       targets. */
    let lay_with = ((dx, dy)): option(CanvasLayout.t) => {
      let offsets' = [
        (n.key, (fst(base) +. dx, snd(base) +. dy)),
        ...List.remove_assoc(n.key, offsets),
      ];
      switch (cached_frame^) {
      | Some(fc) =>
        Some(
          CanvasLayout.layout(
            ~x_scale=fc.fc_x_scale,
            ~y_scale=fc.fc_y_scale,
            ~origin_override=Some(fc.fc_origin),
            ~offsets=offsets',
            ~pins,
            graph,
          ),
        )
      | None => None
      };
    };
    let raf_busy = ref(false);
    let pending: ref(option((float, float))) =
      ref(None: option((float, float)));
    let follow_edges = (dx: float, dy: float) => {
      pending := Some((dx, dy));
      if (! raf_busy^) {
        raf_busy := true;
        let _ =
          Js.Unsafe.meth_call(
            Js.Unsafe.global##.window,
            "requestAnimationFrame",
            [|
              Js.Unsafe.inject(
                Js.Unsafe.callback(() => {
                  raf_busy := false;
                  switch (pending^) {
                  | Some(d) =>
                    switch (lay_with(d)) {
                    | Some(l) => apply_layout(l)
                    | None => ()
                    }
                  | None => ()
                  };
                }),
              ),
            |],
          );
        ();
      };
    };
    let doc = Js.Unsafe.coerce(Dom_html.document);
    let move_cb = ref(Js.Unsafe.inject(Js.null));
    let up_cb = ref(Js.Unsafe.inject(Js.null));
    let on_move = e => {
      let x: int = Js.Unsafe.coerce(e)##.clientX;
      let y: int = Js.Unsafe.coerce(e)##.clientY;
      /* CSS zoom scales screen deltas; convert to layout px */
      let z = CanvasZoom.clamp(zoom_now^);
      let dx = float_of_int(x - sx) /. z
      and dy = float_of_int(y - sy) /. z;
      if (abs_float(dx) +. abs_float(dy) > 4.) {
        moved := true;
      };
      if (moved^) {
        /* snap the drag itself to the dot lattice: the visual position
           is always exactly what the committed layout will produce, so
           the imperative style can never disagree with the re-render */
        let dx = CanvasLayout.snap(dx)
        and dy = CanvasLayout.snap(dy);
        delta := (dx, dy);
        switch (orig) {
        | Some((el, l, t)) =>
          let st = Js.Unsafe.coerce(el)##.style;
          st##.left := Js.string(Printf.sprintf("%.1fpx", l +. dx));
          st##.top := Js.string(Printf.sprintf("%.1fpx", t +. dy));
          /* bow wave: the unsnapped position, so the field glides */
          CanvasRipple.set_field(
            Some((
              l +. float_of_int(x - sx) /. z,
              t +. float_of_int(y - sy) /. z,
            )),
          );
        | None => ()
        };
        /* incident edges, labels, orbits and orbit rings follow by
           DIRECT DOM mutation */
        follow_edges(dx, dy);
      };
    };
    let on_up = _ => {
      drag_active := false;
      if (moved^) {
        /* settle: a soft splash where the node lands */
        switch (orig) {
        | Some((_, l, t)) =>
          let (dx, dy) = delta^;
          CanvasRipple.splash(~amp=4.5, (l +. dx, t +. dy));
        | None => ()
        };
      };
      CanvasRipple.set_field(None);
      let _ = doc##removeEventListener(Js.string("mousemove"), move_cb^);
      let _ = doc##removeEventListener(Js.string("mouseup"), up_cb^);
      if (during_score) {
        CanvasEnact.resume_score();
        if (moved^) {
          switch (CanvasEnact.visible_site()) {
          | Some(pos) => CanvasEnact.replan_score(~pos, ())
          | None => ()
          };
        };
      };
      if (moved^) {
        commit(delta^);
      };
      Effect.Expert.handle_non_dom_event_exn(click_effect(n));
    };
    move_cb := Js.Unsafe.inject(Js.wrap_callback(on_move));
    up_cb := Js.Unsafe.inject(Js.wrap_callback(on_up));
    let _ = doc##addEventListener(Js.string("mousemove"), move_cb^);
    let _ = doc##addEventListener(Js.string("mouseup"), up_cb^);
    Effect.Prevent_default;
  };
  /* Explicit Fit is an overview, independent of Follow's readability
     floor. Include the rendered labels and wires, not just node radii:
     any of them can extend beyond the layout's nominal box. */
  let fit_view = (~cap: option(float)=None, ~dur: float=320., ()) => {
    let capped = z =>
      switch (cap) {
      | Some(c) => min(c, z)
      | None => z
      };
    let aw = Option.value(~default=lay.width, avail_width)
    and ah = Option.value(~default=lay.height, avail_height);
    let top = min(max(0., ah -. 40.), CanvasCamera.overview_top_inset());
    CanvasCamera.note_user_zoom();
    let bounds =
      switch (CanvasCamera.rendered_graph_bbox()) {
      | Some(b) => Some(b)
      | None => CanvasCamera.graph_bbox^
      };
    switch (bounds) {
    | Some((x0, y0, x1, y1)) =>
      let z =
        capped(
          CanvasZoom.fit(
            ~width=x1 -. x0,
            ~height=y1 -. y0,
            ~aw,
            ~ah=ah -. top,
          ),
        );
      CanvasCamera.animate(
        ~aw,
        ~ah,
        ~zoom=Some(z),
        ~dur,
        ~easing=CanvasCamera.EaseOut,
        ((x0 +. x1) /. 2., (y0 +. y1) /. 2. -. top /. (2. *. z)),
      );
    | None =>
      animate_fit(
        ~z_to=
          capped(
            CanvasZoom.fit(~width=lay.width, ~height=lay.height, ~aw, ~ah),
          ),
        ~lw=lay.width,
        ~lh=lay.height,
        ~aw,
        ~ah,
      )
    };
  };
  if (fit_pending^) {
    fit_pending := false;
    CanvasEnact.after_render(fit_view);
  };
  /* PORTOLAN: a snapshot shown without a story is framed whole, but
     never magnified past 1:1 */
  if (overview_pending^) {
    overview_pending := false;
    let dur = fresh_board^ ? 1. : 320.;
    fresh_board := false;
    CanvasEnact.after_render(fit_view(~cap=Some(1.), ~dur));
  };

  /* ---- the avatar ---- */
  let agent_busy = busy^ || CanvasBuffer.fake_busy();
  let avatar = {
    /* while beats are on screen, the avatar target rides them (captured
       per cause at push time) instead of reading the live state and
       arriving ahead of the paced graph */
    let target =
      switch (CanvasBuffer.pacing_live() ? CanvasBuffer.beat_avatar^ : None) {
      | Some(_) as beat_site => beat_site
      | None => avatar_target()
      };
    let resolved_from_target =
      target
      |> Util.OptUtil.and_then(((id, state)) =>
           switch (locate(lay, id)) {
           | Some(p) =>
             last_avatar_id := Some(id);
             last_avatar_pos := Some(p);
             Some((p, state));
           | None =>
             /* target no longer resolves (edits moved on): try the last
                good id against the current layout, else hold position */
             switch (last_avatar_id^ |> Util.OptUtil.and_then(locate(lay))) {
             | Some(p) =>
               last_avatar_pos := Some(p);
               Some((p, state));
             | None => last_avatar_pos^ |> Option.map(p => (p, state))
             }
           }
         );
    let resolved =
      switch (CanvasBuffer.avatar_site^) {
      | Some((x, y)) =>
        let p =
          CanvasLayout.{
            x,
            y,
          };
        last_avatar_pos := Some(p);
        /* the state comes from the last cause (edit only around a
           landed edit, thinking otherwise) — busy alone is not editing */
        let st =
          switch (resolved_from_target) {
          | Some((_, st)) => st
          | None => ""
          };
        Some((p, agent_busy ? st : ""));
      | None => resolved_from_target
      };
    switch (resolved) {
    | Some((p, state)) =>
      /* busy with no fresh edit landing = thinking */
      Some((p, agent_busy && state == "" ? "think" : state))
    | None when agent_busy =>
      /* nothing resolved yet: embody the agent from the first thinking
         token anyway — at its last known spot, else at the viewport
         center (where the user is looking); its first hop then travels
         from there. */
      let on_board = (p: CanvasLayout.pos) =>
        p.x >= (-60.)
        && p.x <= lay.width
        +. 60.
        && p.y >= (-60.)
        && p.y <= lay.height
        +. 60.;
      let p =
        switch (last_avatar_pos^) {
        | Some(p) when on_board(p) => Some(p)
        | _ =>
          switch (avail_width, avail_height) {
          | (Some(aw), Some(ah)) =>
            CanvasCamera.center(~aw, ~ah)
            |> Option.map(((x, y)) =>
                 CanvasLayout.{
                   x,
                   y,
                 }
               )
          | _ =>
            Some(
              CanvasLayout.{
                x: lay.width /. 2.,
                y: lay.height /. 2.,
              },
            )
          }
        };
      switch (p) {
      | Some(p) =>
        last_avatar_pos := Some(p);
        Some((p, "think"));
      | None => None
      };
    | None => None
    };
  };
  /* the camera judges "fits"/"all visible" against the nodes themselves,
     not the pane-sized layout box (which never fits with a margin) */
  CanvasCamera.graph_bbox :=
    (
      switch (lay.nodes) {
      | [] => None
      | ns =>
        Some(
          List.fold_left(
            ((x0, y0, x1, y1), nl: CanvasLayout.node_layout) =>
              (
                min(x0, nl.p.x -. nl.r),
                min(y0, nl.p.y -. nl.r),
                max(x1, nl.p.x +. nl.r),
                max(y1, nl.p.y +. nl.r +. 18.),
              ),
            (infinity, infinity, neg_infinity, neg_infinity),
            ns,
          ),
        )
      }
    );
  switch (avail_width, avail_height) {
  | (Some(aw), Some(ah)) =>
    CanvasCamera.note_exposure(
      ~aw,
      ~ah,
      List.map(
        (nl: CanvasLayout.node_layout) => (nl.node.key, (nl.p.x, nl.p.y)),
        lay.nodes,
      ),
    )
  | _ => ()
  };
  /* camera follow: a hop (or a resting avatar waking) hands the site to
     the camera; the dead zone decides whether it actually moves */
  switch (avatar, avail_width, avail_height) {
  | (Some((p, _)), Some(aw), Some(ah))
      when
        follow^
        /* while beats are paced the score owns the camera (its frames
           are planned per act); the generic follow is for the rest */
        && !CanvasBuffer.pacing_live()
        && CanvasBuffer.now() >= CanvasCamera.scored_until^ =>
    let hopped = last_avatar_id^ != last_followed_id^;
    let woke = agent_busy && ! last_followed_busy^;
    if (hopped || woke) {
      last_followed := Some(p);
      last_followed_id := last_avatar_id^;
      CanvasCamera.follow(~aw, ~ah, (p.x, p.y));
    };
    last_followed_busy := agent_busy;
  | _ => ()
  };
  if (agent_busy != last_logged_busy^) {
    last_logged_busy := agent_busy;
    CanvasBuffer.note_agent_busy(agent_busy);
    CanvasLog.log(agent_busy ? "agent: busy" : "agent: idle");
  };
  {
    let (pid, pstate) = last_logged_avatar^;
    let cur_state =
      switch (avatar) {
      | Some((_, st)) => st == "" ? "at rest" : st
      | None => "off"
      };
    let cur_id = last_avatar_id^;
    /* name the node the avatar landed nearest, so the journal reads
       as a story ("hop -> Recipe") */
    let site = () =>
      switch (avatar) {
      | Some((p, _)) =>
        lay.nodes
        |> List.fold_left(
             (best, nl: CanvasLayout.node_layout) => {
               let d = abs_float(nl.p.x -. p.x) +. abs_float(nl.p.y -. p.y);
               switch (best) {
               | Some((bd, _)) when bd <= d => best
               | _ => Some((d, nl.node.key))
               };
             },
             None,
           )
        |> Option.map(((_, k)) => " @ " ++ k)
        |> Option.value(~default="")
      | None => ""
      };
    if (cur_state != pstate) {
      CanvasLog.log("avatar: " ++ cur_state ++ site());
      last_logged_avatar := (cur_id, cur_state);
    } else if (cur_id != pid) {
      CanvasLog.log("avatar: hop (" ++ cur_state ++ ")" ++ site());
      last_logged_avatar := (cur_id, cur_state);
    };
  };
  /* the thought bubble shows while the model streams; its text is
     driven outside the vdom (CanvasBubble) from the full streamed text.
     Actions are called out by the speech bubble, fed by the player. */
  if (! CanvasBubble.force_cloud^) {
    CanvasBubble.source := thought^;
  };
  CanvasBubble.thought_available :=
    CanvasBubble.force_cloud^
    || agent_busy
    && pace^
    && String.length(thought^) > 0;
  CanvasBubble.ensure_loop();

  CanvasRipple.request_draw();
  switch (avail_width, avail_height) {
  | (Some(aw), Some(ah)) =>
    /* PORTOLAN: an empty board is anchored as if it were as wide as the
       pane (a layout with content is, through ~center_within), so the
       first content lands centred without a scroll jump (B1). Upstream
       slides rarely start empty; a pushed code base does. */
    let lw = lay.nodes == [] ? aw : lay.width *. zoom;
    anchor_target :=
      (
        max(0., pan_slack +. (lw -. aw) /. 2.),
        max(0., pan_slack +. (lay.height *. zoom -. ah) /. 2.),
      );
  | _ => ()
  };
  ensure_scroll_anchor(slide);
  /* hull circles follow these targets (CanvasJelly): on a scored render
     they glide on the nodes' schedule (set below, once the score is
     planned); otherwise they spring — drags and collapses wobble */
  let hull_targets = CanvasView.hull_targets(lay);
  {
    let (prev_slide, prev_nodes) = last_node_snapshot^;
    if (prev_slide != slide) {
      CanvasLog.log("slide: " ++ slide);
    };
    let cur_keys =
      List.map((nl: CanvasLayout.node_layout) => nl.node.key, lay.nodes);
    let removed =
      List.filter(((k, _)) => !List.mem(k, cur_keys), prev_nodes);
    let cur_edge_names =
      List.map((el: CanvasLayout.edge_layout) => el.edge.e_name, lay.edges);
    let removed_edge_layouts =
      switch (last_layout^) {
      | Some(prev) when prev_slide == slide =>
        List.filter(
          (el: CanvasLayout.edge_layout) =>
            !List.mem(el.edge.e_name, cur_edge_names),
          prev.edges,
        )
      | _ => []
      };
    let removed_node_layouts =
      switch (last_layout^) {
      | Some(prev) when prev_slide == slide =>
        List.filter(
          (nl: CanvasLayout.node_layout) =>
            List.mem_assoc(nl.node.key, removed),
          prev.nodes,
        )
      | _ => []
      };
    /* elements present in both renders whose look differs */
    let (changed_nodes, changed_edges) =
      switch (last_layout^) {
      | Some(prev) when prev_slide == slide => (
          List.filter_map(
            (nl: CanvasLayout.node_layout) =>
              switch (
                List.find_opt(
                  (pl: CanvasLayout.node_layout) =>
                    pl.node.key == nl.node.key,
                  prev.nodes,
                )
              ) {
              | Some(pl) when node_look(pl.node) != node_look(nl.node) =>
                Some((nl, pl.node))
              | _ => None
              },
            lay.nodes,
          ),
          List.filter_map(
            (el: CanvasLayout.edge_layout) =>
              switch (
                List.find_opt(
                  (pe: CanvasLayout.edge_layout) =>
                    pe.edge.e_name == el.edge.e_name,
                  prev.edges,
                )
              ) {
              | Some(pe) when edge_look(pe.edge) != edge_look(el.edge) =>
                Some((el, pe.edge))
              | _ => None
              },
            lay.edges,
          ),
        )
      | _ => ([], [])
      };
    if (prev_slide == slide
        && removed != []
        && CanvasBuffer.now() > collapse_fx_until^) {
      if (List.length(removed) <= 4) {
        List.iter(((_, (x, y))) => CanvasRipple.suction((x, y)), removed);
        CanvasLog.log(
          Printf.sprintf(
            "-%d node(s): %s (suction ripple)",
            List.length(removed),
            String.concat(", ", List.map(fst, removed)),
          ),
        );
      } else {
        CanvasLog.log(
          Printf.sprintf(
            "-%d nodes at once (removal effects skipped)",
            List.length(removed),
          ),
        );
      };
    };
    /* agent rainfall: nodes that appear during a paced burst arrive with
       a small splash + grow-in */
    let added =
      List.filter(
        (nl: CanvasLayout.node_layout) =>
          !List.mem_assoc(nl.node.key, prev_nodes)
          && !List.mem_assoc(nl.node.key, last_placed^),
        lay.nodes,
      );
    /* value badges (constants, module values) are canvas content too: a
       new one is a terminal of its type's definition act (A2) */
    let (_, prev_values) = last_value_snapshot^;
    let added_values =
      List.filter(
        (vl: CanvasLayout.value_layout) =>
          !List.mem(vl.value.v_name, prev_values),
        lay.values,
      );
    last_value_snapshot :=
      (
        slide,
        List.map(
          (vl: CanvasLayout.value_layout) => vl.value.v_name,
          lay.values,
        ),
      );
    /* ---- the beat's score (A1): ONE plan from this render's diff. The
       actor visits each new definition, pulls each new arrow, and the
       rest of the graph drifts last. Arrival times feed Animation before
       the render lands; the player runs one frame after it. ---- */
    let (_, prev_edges) = last_edge_snapshot^;
    let new_edges =
      lay.edges
      |> List.filter_map((el: CanvasLayout.edge_layout) =>
           if (List.mem(el.edge.e_name, prev_edges)) {
             None;
           } else {
             let product =
               lay.nodes
               |> List.find_opt((nl: CanvasLayout.node_layout) =>
                    nl.node.key == el.edge.e_src
                  )
               |> Util.OptUtil.and_then((nl: CanvasLayout.node_layout) =>
                    nl.node.kind == CanvasGraph.Product
                    && nl.node.parts != []
                    && !List.mem_assoc(nl.node.key, prev_nodes)
                      ? Some((nl.node.key, nl.node.parts)) : None
                  );
             Some(
               CanvasScore.{
                 name: el.edge.e_name,
                 src: el.edge.e_src,
                 dst: el.edge.dst,
                 product,
               },
             );
           }
         );
    if (added != [] || new_edges != []) {
      let nodes =
        List.map((nl: CanvasLayout.node_layout) => nl.node.key, added)
      and edges = List.map(e => e.CanvasScore.name, new_edges);
      CanvasEnact.after_render(() => CanvasEnact.revive(~nodes, ~edges));
    };
    let moved =
      lay.nodes
      |> List.filter_map((nl: CanvasLayout.node_layout) =>
           switch (List.assoc_opt(nl.node.key, prev_nodes)) {
           | Some((px, py))
               when
                 abs_float(px -. nl.p.x) > 1. || abs_float(py -. nl.p.y) > 1. =>
             Some((
               nl.node.key,
               CanvasLayout.{
                 x: px,
                 y: py,
               },
               nl.p,
             ))
           | _ => None
           }
         );
    let big_move =
      List.length(
        List.filter(
          ((_, f: CanvasLayout.pos, t: CanvasLayout.pos)) =>
            abs_float(f.x -. t.x) > 40. || abs_float(f.y -. t.y) > 40.,
          moved,
        ),
      )
      >= 3;
    let scored =
      prev_slide == slide
      && pace^
      && (
        CanvasBuffer.pacing_live()
        && (
          added != []
          || added_values != []
          || new_edges != []
          || removed != []
          || removed_edge_layouts != []
          || changed_nodes != []
          || changed_edges != []
        )
        || big_move
        && burst_end_reframe^
      );
    burst_end_reframe := false;
    if (scored) {
      /* a render nobody staged (the burst-end re-frame) still needs its
         movers recorded before the patch, or the drift is a jump */
      if (!Animation.staged()) {
        CanvasBuffer.stage_beat(~slow=true, ());
      };
      List.iter(
        (nl: CanvasLayout.node_layout) => note_placed(nl.node.key),
        added,
      );
      let pos_of = k =>
        lay.nodes
        |> List.find_opt((nl: CanvasLayout.node_layout) => nl.node.key == k)
        |> Option.map((nl: CanvasLayout.node_layout) => nl.p);
      /* the story's order (CanvasScore.new_node.order): graph nodes and
         edges are extracted in program order; a glyph nobody names takes
         the place of the first edge that touches it */
      let order_of = (k: string): int => {
        let rec idx = (i, ks) =>
          switch (ks) {
          | [] => Option.none
          | [x, ..._] when x == k => Option.some(i)
          | [_, ...rest] => idx(i + 1, rest)
          };
        let is_module = key =>
          String.length(key) >= 3 && String.sub(key, 0, 3) == "{}@";
        let named =
          List.filter_map(
            (n: CanvasGraph.tynode) =>
              switch (n.kind) {
              | CanvasGraph.Builtin => None
              | CanvasGraph.Product => is_module(n.key) ? Some(n.key) : None
              | _ => Some(n.key)
              },
            graph.nodes,
          );
        /* a module leads its members: the agent wrote `module M = {`
           before anything inside it */
        let module_first = (k: string): int =>
          if (is_module(k)) {
            let root = String.sub(k, 3, String.length(k) - 3);
            let first_member =
              List.mapi((i, key) => (i, key), named)
              |> List.find_opt(((_, key)) =>
                   List.exists(
                     (n: CanvasGraph.tynode) =>
                       n.key == key
                       && (
                         switch (n.m_path) {
                         | [r, ..._] => r == root
                         | [] => false
                         }
                       ),
                     graph.nodes,
                   )
                 );
            switch (first_member) {
            | Some((i, _)) => 2 * i - 1 /* just before that member */
            | None => max_int
            };
          } else {
            max_int;
          };
        switch (idx(0, named)) {
        | Some(i) => min(2 * i, module_first(k))
        | None =>
          let n = List.length(named);
          let touching =
            List.mapi((i, e: CanvasGraph.edge) => (i, e), graph.edges)
            |> List.find_opt(((_, e: CanvasGraph.edge)) =>
                 e.e_src == k
                 || e.dst == k
                 || List.mem(k, e.e_deps)
                 || (
                   switch (
                     List.find_opt(
                       (pn: CanvasGraph.tynode) => pn.key == e.e_src,
                       graph.nodes,
                     )
                   ) {
                   | Some(pn) => List.mem(k, pn.parts)
                   | None => false
                   }
                 )
               );
          switch (touching) {
          | Some((i, _)) => 2 * (n + i)
          | None => max_int
          };
        };
      };
      let diff =
        CanvasScore.{
          d_cause: Option.value(~default="edit", CanvasBuffer.shown_label^),
          added:
            List.map(
              (nl: CanvasLayout.node_layout) =>
                CanvasScore.{
                  key: nl.node.key,
                  anchor: Option.map(fst, nl.node.sat),
                  p: nl.p,
                  order: order_of(nl.node.key),
                },
              added,
            )
            @ List.map(
                (vl: CanvasLayout.value_layout) =>
                  CanvasScore.{
                    key: value_score_key(vl.value.v_name),
                    anchor: Some(vl.value.v_key),
                    p: vl.p,
                    order: order_of(vl.value.v_key),
                  },
                added_values,
              ),
          edges: new_edges,
          removed:
            List.map(
              ((k, (x, y))) =>
                (
                  k,
                  CanvasLayout.{
                    x,
                    y,
                  },
                ),
              removed,
            ),
          removed_edges:
            List.map(
              (el: CanvasLayout.edge_layout) => (el.edge.e_name, el.label_p),
              removed_edge_layouts,
            ),
          changed:
            List.map(
              ((nl: CanvasLayout.node_layout, _)) => (nl.node.key, nl.p),
              changed_nodes,
            )
            @ List.map(
                ((el: CanvasLayout.edge_layout, _)) =>
                  (el.edge.e_name, el.label_p),
                changed_edges,
              ),
          moved,
          actor: last_avatar_pos^,
        };
      let score = CanvasScore.plan(~pos_of, diff);
      let score =
        switch (avail_width, avail_height) {
        | (Some(aw), Some(ah)) when follow^ =>
          switch (CanvasCamera.center(~aw, ~ah)) {
          | Some((cx, cy)) =>
            CanvasScore.with_frames(
              ~pane=(aw, ah),
              ~cur=
                CanvasScore.{
                  center:
                    CanvasLayout.{
                      x: cx,
                      y: cy,
                    },
                  zoom: CanvasCamera.zoom_now^,
                },
              ~pos_of=
                CanvasScore.with_edge_positions(
                  ~pos_of,
                  ~edges=
                    List.map(
                      (el: CanvasLayout.edge_layout) =>
                        (el.edge.e_name, el.src_p, el.dst_p),
                      lay.edges @ removed_edge_layouts,
                    ),
                ),
              ~all_keys=cur_keys,
              ~exposed=CanvasCamera.exposed_keys(),
              score,
            )
          | None => score
          }
        | _ => score
        };
      /* Appear/Pill times drive Animation's arrivals; movers wait for
         the drift act */
      let pills =
        CanvasScore.effects_abs(score)
        |> List.filter_map(((t, _, e: CanvasScore.timed_effect)) =>
             switch (e.effect) {
             | Pill(name) => Some((CanvasView.edge_dom_id(name), t))
             | Reveal(name) =>
               Some((CanvasView.edge_dom_id(name), t + e.dur))
             | _ => None
             }
           );
      /* module hulls grow with their members: every hull circle (a
         node's or a label's, ancestor copies included) arrives with the
         node or pill it stands for, and the member-edge sausages draw on
         with their pills. Times are looked up from the hull ids
         themselves, so the two enumerations cannot disagree. */
      let (hull_arrivals, hull_geoms) = {
        let node_t =
          List.map(
            ((k, t)) => (CanvasView.sanitize(k), t),
            CanvasScore.appear_times(score),
          );
        let edge_t =
          CanvasScore.effects_abs(score)
          |> List.filter_map(((t, _, e: CanvasScore.timed_effect)) =>
               switch (e.effect) {
               | Pill(name) => Some((CanvasView.sanitize(name), t))
               | Reveal(name) =>
                 Some((CanvasView.sanitize(name), t + e.dur))
               | _ => None
               }
             );
        let after = (pre: string, id: string): option(string) =>
          String.length(id) > String.length(pre)
          && String.sub(id, 0, String.length(pre)) == pre
            ? Some(
                String.sub(
                  id,
                  String.length(pre),
                  String.length(id) - String.length(pre),
                ),
              )
            : None;
        /* an ancestor copy carries a "--v<path>" suffix */
        let base = (s: string): string => {
          let n = String.length(s);
          let rec go = i =>
            i + 3 > n
              ? s
              : String.sub(s, i, 3) == "--v"
                  ? String.sub(s, 0, i) : go(i + 1);
          go(0);
        };
        let time_of = (id: string): option(int) =>
          switch (
            after("hullc-n-", id),
            after("hullc-l-", id),
            after("hulls-e-", id),
          ) {
          | (Some(s), _, _) => List.assoc_opt(base(s), node_t)
          | (_, Some(s), _)
          | (_, _, Some(s)) => List.assoc_opt(base(s), edge_t)
          | _ => None
          };
        let ids =
          List.map(((id, _, _)) => id, CanvasView.hull_targets(lay));
        let sausage_ids =
          CanvasView.hull_paths_of(lay)
          |> List.concat_map(path =>
               List.map(
                 ((id, _, _)) => id,
                 CanvasView.hull_sausages_at(lay, path),
               )
             );
        (
          List.filter_map(id => Option.map(t => (id, t), time_of(id)), ids),
          List.filter_map(
            id => Option.map(t => (id, Option.some(t)), time_of(id)),
            sausage_ids,
          ),
        );
      };
      Animation.set_arrival_schedule(
        List.map(
          ((k, t)) =>
            switch (value_of_score_key(k)) {
            | Some(v) => (CanvasView.value_dom_id(v), t)
            | None => (CanvasView.node_dom_id(k), t)
            },
          CanvasScore.appear_times(score),
        )
        @ pills
        @ hull_arrivals,
      );
      Animation.set_movers_at(CanvasScore.drift_at(score));
      /* the hulls move with the nodes: same start (the drift act, or the
         beat's lead), same duration, same curve */
      CanvasJelly.set_targets(
        ~glide=
          Some((
            CanvasBuffer.now()
            +. float_of_int(
                 max(
                   CanvasBuffer.lead_ms,
                   Option.value(CanvasScore.drift_at(score), ~default=0),
                 ),
               ),
            float_of_int(CanvasBuffer.relayout_ms),
          )),
        hull_targets,
      );
      {
        /* new geometry follows its act: formation/dep lines draw on right
           after the node they attach to blooms; new function paths stay
           hidden until the actor rides them; label leaders come with pills */

        let appear = CanvasScore.appear_times(score);
        let at_key = k => List.assoc_opt(k, appear);
        let pair = (a, b, id) =>
          switch (at_key(a), at_key(b)) {
          | (Some(ta), Some(tb)) => [(id, Some(max(ta, tb) + 80))]
          | (Some(t), None)
          | (None, Some(t)) => [(id, Some(t + 80))]
          | (None, None) => []
          };
        /* lines into a product an act FORMS are drawn by that act: read
           it off the score's effects, so ownership can never disagree
           with what the score will actually do */
        let formed =
          CanvasScore.effects_abs(score)
          |> List.filter_map(((_, _, e: CanvasScore.timed_effect)) =>
               switch (e.effect) {
               | Form(pk, _) => Some(pk)
               | _ => None
               }
             );
        let forms =
          List.concat_map(
            ((a, b, _, _)) =>
              List.mem(b, formed)
                ? [(CanvasView.formation_dom_id(a, b), Option.none)]
                : pair(a, b, CanvasView.formation_dom_id(a, b)),
            lay.formations,
          );
        let deps =
          List.concat_map(
            ((a, b, _, _)) => pair(a, b, CanvasView.dep_dom_id(a, b)),
            lay.dep_links,
          );
        let (draws, leaders) =
          CanvasScore.effects_abs(score)
          |> List.fold_left(
               ((ds, ls), (t, _, e: CanvasScore.timed_effect)) =>
                 switch (e.effect) {
                 | Draw(name) => (
                     /* an endofunction's orbit ring cannot be ridden: it
                        draws on when the ride would have ended */
                     [
                       (CanvasView.path_dom_id(name), Option.none),
                       /* drawn on over the ride's span, so it is whole
                          before the pill lands */
                       (CanvasView.orbit_dom_id(name), Option.some(t)),
                       ...ds,
                     ],
                     ls,
                   )
                 | Reveal(name) => (
                     /* a path or, for an endofunction, an orbit ring:
                        schedule both ids (one exists); the label's leader
                        comes with the pill */
                     [
                       (CanvasView.path_dom_id(name), Option.some(t)),
                       (CanvasView.orbit_dom_id(name), Option.some(t)),
                       ...ds,
                     ],
                     [
                       (
                         "clead-" ++ CanvasView.sanitize(name),
                         Option.some(t + e.dur),
                       ),
                       ...ls,
                     ],
                   )
                 | Pill(name) => (
                     ds,
                     [
                       ("clead-" ++ CanvasView.sanitize(name), Some(t)),
                       ...ls,
                     ],
                   )
                 | _ => (ds, ls)
                 },
               ([], []),
             );
        /* a label leader that appears on an EXISTING edge (its pill was
           pushed by the relayout) belongs to the drift, not to a beat's
           default stagger */
        let drift_t = Option.value(CanvasScore.drift_at(score), ~default=0);
        let old_leaders =
          lay.edges
          |> List.filter_map((el: CanvasLayout.edge_layout) =>
               List.exists(
                 (e: CanvasScore.new_edge) => e.name == el.edge.e_name,
                 new_edges,
               )
                 ? None
                 : Some((
                     "clead-" ++ CanvasView.sanitize(el.edge.e_name),
                     Option.some(drift_t),
                   ))
             );
        Animation.set_geom_schedule(
          forms @ deps @ draws @ leaders @ old_leaders @ hull_geoms,
        );
      };
      CanvasBuffer.extend_dwell(float_of_int(score.total_ms) +. 600.);
      {
        let t0 = CanvasBuffer.now();
        let cue = k =>
          switch (List.assoc_opt(k, CanvasScore.change_times(score))) {
          | Some(t) => t0 +. float_of_int(t)
          | None => t0
          };
        staged :=
          {
            s_nodes:
              List.map(
                ((nl: CanvasLayout.node_layout, old)) =>
                  (nl.node.key, cue(nl.node.key), old),
                changed_nodes,
              ),
            s_edges:
              List.map(
                ((el: CanvasLayout.edge_layout, old)) =>
                  (el.edge.e_name, cue(el.edge.e_name), old),
                changed_edges,
              ),
          };
      };
      if (removed_node_layouts != [] || removed_edge_layouts != []) {
        leaving :=
          Option.some({
            l_nodes: removed_node_layouts,
            l_edges: removed_edge_layouts,
            l_until:
              CanvasBuffer.now() +. float_of_int(score.total_ms) +. 400.,
          });
      };
      CanvasLog.log(CanvasScore.to_string(score));
      List.iter(
        v => CanvasLog.log("SCORE: " ++ v),
        CanvasScore.validate(score) @ CanvasScore.coverage(diff, score),
      );
      let zoom = CanvasCamera.zoom_now^;
      CanvasEnact.jump_snapshot(~zoom);
      CanvasEnact.after_render(() => {
        CanvasEnact.check_jumps(~zoom);
        CanvasEnact.play(~zoom, score);
      });
    } else {
      if (prev_slide == slide && CanvasBuffer.in_burst() && added != []) {
        CanvasLog.log(
          Printf.sprintf(
            "+%d node(s) (unscored render)",
            List.length(added),
          ),
        );
      };
      CanvasJelly.set_targets(hull_targets);
    };
    /* layout churn: how much did EXISTING nodes move this render?
       (the metric behind "the layout suddenly reshuffled") */
    let moved =
      List.filter_map(
        (nl: CanvasLayout.node_layout) =>
          switch (List.assoc_opt(nl.node.key, prev_nodes)) {
          | Some((px, py)) =>
            let d = max(abs_float(nl.p.x -. px), abs_float(nl.p.y -. py));
            d > 21. ? Some(d) : None;
          | None => None
          },
        lay.nodes,
      );
    if (prev_slide == slide && moved != [] && ! drag_active^) {
      CanvasLog.log(
        Printf.sprintf(
          "layout: %d/%d nodes moved (max %.0fpx)",
          List.length(moved),
          List.length(lay.nodes),
          List.fold_left(max, 0., moved),
        ),
      );
    };
    /* record positions (grid cells) whenever the layout meaningfully
       changed, for post-hoc before/after diffing */
    if (! drag_active^
        && (prev_slide != slide || moved != [] || added != [] || removed != [])) {
      let pos_line =
        lay.nodes
        |> List.map((nl: CanvasLayout.node_layout) =>
             Printf.sprintf(
               "%s(%.0f,%.0f)",
               nl.node.key,
               nl.p.x /. 14.,
               nl.p.y /. 14.,
             )
           )
        |> String.concat(" ");
      CanvasLog.record_layout(
        CanvasLog.stamp() ++ " " ++ slide ++ " | " ++ pos_line,
      );
    };
    last_node_snapshot :=
      (
        slide,
        List.map(
          (nl: CanvasLayout.node_layout) => (nl.node.key, (nl.p.x, nl.p.y)),
          lay.nodes,
        ),
      );
    last_layout := Option.some(lay);

    /* the edge set for the next render's diff (edges are enacted by the
       score planned above) */
    let cur_edges =
      List.map((el: CanvasLayout.edge_layout) => el.edge.e_name, lay.edges);
    last_edge_snapshot := (slide, cur_edges);
  };
  /* dependency fan: subdued under-layer curves from the hovered or
     focused function's pill to what it references */
  let dep_fan = {
    /* PORTOLAN: where a name is on the board: a function's pill, a value,
       or a node (a module on the rim stands for what is called in it) */
    let at = (name: string): option(CanvasLayout.pos) =>
      switch (
        List.find_opt(
          (dl: CanvasLayout.edge_layout) => dl.edge.e_name == name,
          lay.edges,
        )
      ) {
      | Some(dl) => Some(dl.label_p)
      | None =>
        switch (
          List.find_opt(
            (vl: CanvasLayout.value_layout) => vl.value.v_name == name,
            lay.values,
          )
        ) {
        | Some(vl) => Some(vl.p)
        | None =>
          List.find_opt(
            (nl: CanvasLayout.node_layout) => nl.node.key == name,
            lay.nodes,
          )
          |> Option.map((nl: CanvasLayout.node_layout) => nl.p)
        }
      };
    /* PORTOLAN: a module on the rim fans to what it uses on the board, and
       from what calls into it */
    let fan_for_node = (key: string) =>
      switch (at(key)) {
      | None => []
      | Some(p) =>
        let uses =
          switch (
            List.find_opt((r: CanvasGraph.rim) => r.r_key == key, graph0.rim)
          ) {
          | Some(r) =>
            List.filter_map(d => at(d) |> Option.map(t => (p, t)), r.r_uses)
          | None => []
          };
        let used_by =
          lay.edges
          |> List.filter_map((dl: CanvasLayout.edge_layout) =>
               List.mem(key, dl.edge.e_deps) ? Some((dl.label_p, p)) : None
             );
        uses @ used_by;
      };
    let fan_for = (name: string) =>
      switch (
        List.find_opt(
          (el: CanvasLayout.edge_layout) => el.edge.e_name == name,
          lay.edges,
        )
      ) {
      | None => []
      | Some(el) =>
        let uses =
          el.edge.e_deps
          |> List.filter_map(dep =>
               at(dep) |> Option.map(t => (el.label_p, t))
             );
        /* and the reverse: who uses THIS function */
        let used_by =
          lay.edges
          |> List.filter_map((dl: CanvasLayout.edge_layout) =>
               dl.edge.e_name != name && List.mem(name, dl.edge.e_deps)
                 ? Some((dl.label_p, el.label_p)) : None
             );
        let rim_users =
          graph0.rim
          |> List.filter_map((r: CanvasGraph.rim) =>
               List.mem(name, r.r_uses)
                 ? at(r.r_key) |> Option.map(p => (p, el.label_p)) : None
             );
        uses @ used_by @ rim_users;
      };
    switch (hovered_edge^, focused^, focused_ty^) {
    | (Some(n), _, _)
    | (None, Some(n), _) => fan_for(n)
    | (None, None, Some(k)) => fan_for_node(k)
    | (None, None, None) => []
    };
  };
  let on_hull_toggle = (root: string) => {
    let collapsing = !List.mem(root, collapsed_modules^);
    collapsed_modules :=
      collapsing
        ? [root, ...collapsed_modules^]
        : List.filter(r => r != root, collapsed_modules^);
    collapse_fx_until := CanvasBuffer.now() +. 900.;
    /* the effect emanates from the module node itself */
    switch (
      Util.JsUtil.get_elem_by_id_opt(CanvasView.node_dom_id("{}@" ++ root))
    ) {
    | Some(el) =>
      let st = Js.Unsafe.coerce(el)##.style;
      let px = (v: Js.t(Js.js_string)): float => {
        let str = Js.to_string(v);
        switch (String.index_opt(str, 'p')) {
        | Some(i) =>
          Option.value(
            ~default=0.,
            float_of_string_opt(String.sub(str, 0, i)),
          )
        | None => 0.
        };
      };
      let l = px(st##.left)
      and tp = px(st##.top);
      let r = float_of_int(Js.Unsafe.coerce(el)##.offsetWidth) /. 2.;
      collapsing
        ? CanvasRipple.suction((l +. r, tp +. r))
        : CanvasRipple.splash(~amp=8., (l +. r, tp +. r));
    | None => ()
    };
    tick;
  };
  let on_edge_hover = (h: option(string)) => {
    hovered_edge := h;
    tick;
  };
  /* clicking empty canvas clears the selection */
  let on_canvas_plain_click = () =>
    if (focused^ != None || focused_ty^ != None || focused_value^ != None) {
      focused_value := None;
      focused := None;
      focused_ty := None;
      tick;
    } else {
      Effect.Ignore;
    };
  let on_value_click = (v: CanvasGraph.value) => {
    focused_value := Some(v.v_name);
    focused := None;
    focused_ty := None;
    Effect.Many([
      select(v.v_name, "value", Some(v.v_id)),
      tick,
      Effect.Stop_propagation,
    ]);
  };
  let just_placed = {
    let now = CanvasBuffer.now();
    last_placed := List.filter(((_, t)) => now -. t < 2500., last_placed^);
    List.map(fst, last_placed^);
  };
  Node.div(
    ~attrs=[Attr.id("canvas-sidebar"), clss(["portolan-canvas"])],
    [
      Node.div(
        ~attrs=[
          Attr.id("canvas-scroll"),
          Attr.tabindex(-1),
          clss(["canvas-scroll"]),
        ],
        [
          /* the dot field: a viewport-fixed canvas UNDER the board
             (sticky 0x0 holder), redrawn on scroll/zoom with the
             lattice window + edge fade (CanvasRipple) */
          Node.div(
            ~attrs=[clss(["canvas-dots-holder"])],
            [
              Node.create(
                "canvas",
                ~attrs=[Attr.id("canvas-dots"), clss(["canvas-dots"])],
                [],
              ),
            ],
          ),
          CanvasView.view(
            ~inject_jump,
            ~collapsed_counts,
            ~on_hull_toggle,
            ~dep_fan,
            ~on_edge_hover,
            ~on_value_click=Some(on_value_click),
            ~on_canvas_plain_click,
            ~just_placed,
            ~on_node_select=n => click_effect(n),
            ~on_node_dblclick=
              n =>
                Effect.of_sync_fun(
                  () => on_open^(node_name(n), kind_name(n)),
                  (),
                ),
            ~on_delete_definition=_ => Effect.Ignore,
            ~on_edge_contextmenu=(_, _) => Effect.Ignore,
            ~focused_ty=focused_ty^,
            ~on_edge_click,
            ~on_node_mousedown=start_node_drag,
            ~zoom,
            ~min_size=(
              (Option.value(~default=0., avail_width) -. 2.) /. zoom,
              (Option.value(~default=0., avail_height) -. 2.) /. zoom,
            ),
            ~focused=focused^,
            ~avatar,
            ~loose_tests=graph.loose_tests,
            {
              /* ghosts of what a running score is erasing ride along, at
                 their old places, until the score is done */

              let now = CanvasBuffer.now();
              /* changed elements keep their old look until their cue */
              let lay = {
                let st = staged^;
                if (st.s_nodes == [] && st.s_edges == []) {
                  lay;
                } else {
                  let live =
                    List.exists(((_, t, _)) => now < t, st.s_nodes)
                    || List.exists(((_, t, _)) => now < t, st.s_edges);
                  if (!live) {
                    staged :=
                      {
                        s_nodes: [],
                        s_edges: [],
                      };
                    lay;
                  } else {
                    CanvasLayout.{
                      ...lay,
                      nodes:
                        List.map(
                          (nl: CanvasLayout.node_layout) =>
                            switch (
                              List.find_opt(
                                ((k, t, _)) => k == nl.node.key && now < t,
                                st.s_nodes,
                              )
                            ) {
                            | Some((_, _, old)) => {
                                ...nl,
                                node: old,
                              }
                            | None => nl
                            },
                          lay.nodes,
                        ),
                      edges:
                        List.map(
                          (el: CanvasLayout.edge_layout) =>
                            switch (
                              List.find_opt(
                                ((n, t, _)) =>
                                  n == el.edge.e_name && now < t,
                                st.s_edges,
                              )
                            ) {
                            | Some((_, _, old)) => {
                                ...el,
                                edge: old,
                              }
                            | None => el
                            },
                          lay.edges,
                        ),
                    };
                  };
                };
              };
              switch (leaving^) {
              | Some(l) when now < l.l_until =>
                let cur =
                  List.map(
                    (nl: CanvasLayout.node_layout) => nl.node.key,
                    lay.nodes,
                  );
                let cur_e =
                  List.map(
                    (el: CanvasLayout.edge_layout) => el.edge.e_name,
                    lay.edges,
                  );
                CanvasLayout.{
                  ...lay,
                  nodes:
                    lay.nodes
                    @ List.filter(
                        (nl: CanvasLayout.node_layout) =>
                          !List.mem(nl.node.key, cur),
                        l.l_nodes,
                      ),
                  edges:
                    lay.edges
                    @ List.filter(
                        (el: CanvasLayout.edge_layout) =>
                          !List.mem(el.edge.e_name, cur_e),
                        l.l_edges,
                      ),
                };
              | Some(_) =>
                leaving := Option.none;
                lay;
              | None => lay
              };
            },
          ),
        ],
      ),
    ],
  );
};

/* ---------- mounting and the render pass ---------- */

let host: ref(option(Js.t(Dom_html.element))) = ref(None);
let rendered: ref(option((Node.t, Js.t(Dom_html.element)))) = ref(None);

/* upstream: CanvasSidebar.present_editor + Page's view + Main's
   Animation.go after display */
let render = (): unit =>
  switch (host^) {
  | None => ()
  | Some(host_el) =>
    CanvasEnact.request_tick := schedule_tick;
    let graph =
      CanvasBuffer.observe(
        ~enabled=pace^,
        ~weight=graph_delta,
        ~schedule_tick,
        live^,
      );
    let node = view(graph);
    switch (rendered^) {
    | None =>
      let el = Node.to_dom(node);
      Dom.appendChild(host_el, el);
      rendered := Some((node, el));
    | Some((prev, el)) =>
      let patch = Node.Patch.create(~previous=prev, ~current=node);
      let el = Node.Patch.apply(patch, el);
      rendered := Some((node, el));
    };
    Animation.go();
  };
render_fwd := render;

let mount = (el: Js.t(Dom_html.element)): unit => {
  switch (rendered^) {
  | Some((_, old)) =>
    Js.Opt.iter(old##.parentNode, p =>
      ignore(p##removeChild((old :> Js.t(Dom.node))))
    )
  | None => ()
  };
  rendered := None;
  host := Some(el);
  Animation.slow_hook := CanvasLog.log;
  /* twice: the first pass creates the pane, the second measures it */
  render();
  render();
};

/* a new snapshot and what caused it: one beat (upstream:
   CanvasPresentation.push — seed with the state before, queue the state
   after, labelled, with the avatar's site) */
let push = (g: CanvasGraph.t, c: cause): unit => {
  CanvasBuffer.seed(live^);
  CanvasBuffer.note_tool();
  CanvasBuffer.push_lazy(
    ~label=c.label,
    ~avatar=Option.map(s => (s, c.ok ? "edit" : "err"), c.site),
    lazy(g),
  );
  last_cause := Some((c, CanvasBuffer.now()));
  live := g;
  request_render();
};

/* a snapshot with no story: shown at once when nothing is playing (an
   already-existing code base, a reload) */
let show = (g: CanvasGraph.t): unit => {
  /* PORTOLAN: on a board already showing something, what stays glides
     to its new place */
  if (! fresh_board^) {
    CanvasBuffer.stage_beat(~slow=true, ~ms=Some(reader_ms), ());
  };
  live := g;
  overview_pending := true;
  request_render();
};

/* PORTOLAN: frame the whole graph as [show] does (never past 1:1), for a
   host that sees the board has settled */
let frame = (): unit => {
  overview_pending := true;
  request_render();
};

/* a clean board */
let reset = (): unit => {
  CanvasEnact.cancel_score();
  CanvasBuffer.reset();
  CanvasBuffer.last_agent_action := 0.;
  CanvasCamera.gen := CanvasCamera.gen^ + 1;
  CanvasCamera.inflight := Option.none;
  CanvasCamera.scored_until := 0.;
  slide_no := slide_no^ + 1;
  live := CanvasGraph.empty;
  last_cause := None;
  cached_frame := None;
  last_layout := None;
  leaving := None;
  staged :=
    {
      s_nodes: [],
      s_edges: [],
    };
  last_placed := [];
  node_offsets := [];
  collapsed_modules := [];
  focused := None;
  focused_ty := None;
  focused_value := None;
  hovered_edge := None;
  thought := "";
  fresh_board := true;
  pending_anchor := true;
  /* synchronously, so a push right after a reset is diffed against the
     empty board */
  render();
  /* and owes no dwell to whatever was showing before */
  CanvasBuffer.cur_dwell := CanvasBuffer.dwell_base_ms;
  CanvasBuffer.last_beat := 0.;
};

let fit = (): unit => {
  fit_pending := true;
  request_render();
};

/* PORTOLAN: the host selects a definition by its qualified name (a search
   result), as a click on it would. */
let select = (~is_type: bool, name: string): unit => {
  focused_value := None;
  focused := is_type ? None : Some(name);
  focused_ty := is_type ? Some(name) : None;
  request_render();
};

/* PORTOLAN: the layout and the edge style (CanvasOptions). The graph on
   the board is laid out again and glides to its new places; positions
   set by dragging belonged to the old arrangement and are dropped. */
let relayout = (): unit => {
  cached_frame := None;
  node_offsets := [];
  CanvasBuffer.stage_beat(~slow=true, ~ms=Some(reader_ms), ());
  fit_pending := true;
  request_render();
};
let set_layout = (k: string): bool => {
  let ok = CanvasOptions.set_layout(k);
  if (ok) {
    CanvasLog.log("layout: " ++ k);
    relayout();
  };
  ok;
};
let set_edges = (k: string): bool => {
  let ok = CanvasOptions.set_edges(k);
  if (ok) {
    CanvasLog.log("edges: " ++ k);
    relayout();
  };
  ok;
};

/* PORTOLAN: what a module's region is drawn around (CanvasContour): the
   board is drawn again, and nothing moves. And the pull of a module's
   members toward each other (CanvasPull): a new arrangement, as a change
   of layout is. */
let set_contour = (k: string): bool => {
  let ok = CanvasContour.set(k);
  if (ok) {
    CanvasLog.log("contour: " ++ k);
    request_render();
  };
  ok;
};
let set_pull = (k: string): bool => {
  let ok = CanvasPull.set(k);
  if (ok) {
    CanvasLog.log("pull: " ++ k);
    relayout();
  };
  ok;
};

let set_follow = (b: bool): unit => {
  follow := b;
  request_render();
};
let set_pace = (b: bool): unit => {
  pace := b;
  request_render();
};
let set_busy = (b: bool): unit => {
  busy := b;
  request_render();
};
let set_thought = (s: string): unit => {
  thought := s;
  request_render();
};
let pending = (): int => List.length(CanvasBuffer.queue^);
