let m = Mutex.create();
let last: ref(option(Yojson.Safe.t)) = ref(None);
/* How many bytes of changes were sent since the last whole graph. */
let since_whole = ref(0);

let size = json => String.length(Yojson.Safe.to_string(json));

/* The field a graph event carries the graph in, and the bytes of changes
   since the last whole graph once it is sent. */
let encode = (~previous, ~since, graph) => {
  let whole = size(graph);
  switch (Option.bind(previous, p => Delta.diff(p, graph))) {
  | Some(change)
      when size(change) * 2 < whole && since + size(change) < whole => (
      ("delta", change),
      since + size(change),
    )
  | _ => (("graph", graph), 0)
  };
};

/* Events are written with "t", "type" and then the graph or the change,
   so which it is can be read off the front of a long line. */
let kind = line =>
  try(
    Scanf.sscanf(line, "{\"t\":%d,\"type\":\"graph\",\"%[^\"]\"", (_, field) =>
      field == "delta" ? `Change : `Whole
    )
  ) {
  | _ => `Other
  };

/* The graph after the last graph event in [lines], that event's fields,
   and the bytes of changes since the last whole graph. */
let final = lines => {
  let rec tail = (acc, older) =>
    switch (older) {
    | [] => None
    | [line, ...older] =>
      switch (kind(line)) {
      | `Whole => Some([line, ...acc])
      | `Change => tail([line, ...acc], older)
      | `Other => tail(acc, older)
      }
    };
  let step = (state, line) =>
    switch (Yojson.Safe.from_string(line), state) {
    | (`Assoc(fields), _) when List.mem_assoc("graph", fields) =>
      Some((List.assoc("graph", fields), fields, 0))
    | (`Assoc(fields), Some((graph, _, since)))
        when List.mem_assoc("delta", fields) =>
      Some((
        Delta.apply(graph, List.assoc("delta", fields)),
        fields,
        since + String.length(line),
      ))
    | _
    | exception _ => state
    };
  Option.bind(tail([], List.rev(lines)), List.fold_left(step, None));
};

/* An event's fields with the graph whole. */
let whole = (fields, graph) =>
  List.concat_map(
    ((name, value)) =>
      switch (name) {
      | "graph"
      | "delta" => [("graph", graph)]
      | _ => [(name, value)]
      },
    fields,
  );

let emit = (~cause, ~partial, graph) => {
  Mutex.lock(m);
  Fun.protect(
    ~finally=() => Mutex.unlock(m),
    () => {
      let (body, since) =
        encode(~previous=last^, ~since=since_whole^, graph);
      Bus.emit(
        [("type", `String("graph")), body, ("cause", cause)]
        @ (partial ? [("partial", `Bool(true))] : []),
      );
      last := Some(graph);
      since_whole := since;
    },
  );
};

let resume = lines => {
  let found = final(lines);
  Mutex.lock(m);
  last := Option.map(((graph, _, _)) => graph, found);
  since_whole := Option.fold(~none=0, ~some=((_, _, since)) => since, found);
  Mutex.unlock(m);
  last^;
};

let collapse = lines =>
  switch (final(lines)) {
  | None => List.filter(line => kind(line) == `Other, lines)
  | Some((graph, fields, _)) =>
    let event = Yojson.Safe.to_string(`Assoc(whole(fields, graph)));
    snd(
      List.fold_right(
        (line, (placed, kept)) =>
          kind(line) == `Other
            ? (placed, [line, ...kept])
            : (true, placed ? kept : [event, ...kept]),
        lines,
        (false, []),
      ),
    );
  };

let compact = lines => {
  let previous = ref(None)
  and since = ref(0);
  List.map(
    line =>
      switch (kind(line), Yojson.Safe.from_string(line)) {
      | (`Other, _) => line
      | (_, `Assoc(fields)) =>
        let graph =
          switch (
            List.assoc_opt("graph", fields),
            List.assoc_opt("delta", fields),
            previous^,
          ) {
          | (Some(graph), _, _) => Some(graph)
          | (None, Some(change), Some(p)) => Some(Delta.apply(p, change))
          | _ => None
          };
        switch (graph) {
        | None => line
        | Some(graph) =>
          let (body, bytes) =
            encode(~previous=previous^, ~since=since^, graph);
          previous := Some(graph);
          since := bytes;
          Yojson.Safe.to_string(
            `Assoc(
              List.concat_map(
                ((name, value)) =>
                  switch (name) {
                  | "graph"
                  | "delta" => [body]
                  | _ => [(name, value)]
                  },
                fields,
              ),
            ),
          );
        };
      | _
      | exception _ => line
      },
    lines,
  );
};

let starting_over = f => {
  Mutex.lock(m);
  Fun.protect(
    ~finally=() => Mutex.unlock(m),
    () => {
      since_whole := 0;
      f(
        Option.map(
          graph =>
            [
              ("type", `String("graph")),
              ("graph", graph),
              ("cause", `Assoc([("label", `String("carried over"))])),
            ],
          last^,
        ),
      );
    },
  );
};
