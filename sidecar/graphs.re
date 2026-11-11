module Line = Event.Line;

let m = Mutex.create();
let last: ref(option(Yojson.Safe.t)) = ref(None);
/* How many bytes of changes were sent since the last whole graph. */
let since_whole = ref(0);

let size = json => String.length(Yojson.Safe.to_string(json));

let encode = (~previous, ~since, graph) => {
  let whole = size(graph);
  switch (Option.bind(previous, p => Delta.diff(p, graph))) {
  | Some(change)
      when size(change) * 2 < whole && since + size(change) < whole => (
      Event.Change(change),
      since + size(change),
    )
  | _ => (Event.Whole(graph), 0)
  };
};

/* The graph after the last graph event in [lines], that event's line,
   and the bytes of changes since the last whole graph. */
let final = lines => {
  let rec tail = (acc, older) =>
    switch (older) {
    | [] => None
    | [line, ...older] =>
      switch (Line.draws(line)) {
      | Some(`Whole) => Some([line, ...acc])
      | Some(`Change) => tail([line, ...acc], older)
      | None => tail(acc, older)
      }
    };
  let step = (state, line) =>
    switch (Line.drawing(line), state) {
    | (Some(Event.Whole(graph)), _) => Some((graph, line, 0))
    | (Some(Change(change)), Some((graph, _, since))) =>
      Some((
        Delta.apply(graph, change),
        line,
        since + String.length(Line.to_string(line)),
      ))
    | _ => state
    };
  Option.bind(tail([], List.rev(lines)), List.fold_left(step, None));
};

let emit = (~cause, ~partial, graph) => {
  Mutex.lock(m);
  Fun.protect(
    ~finally=() => Mutex.unlock(m),
    () => {
      let (drawing, since) =
        encode(~previous=last^, ~since=since_whole^, graph);
      Bus.emit(
        Graph({
          drawing,
          cause,
          partial,
        }),
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
  | None => List.filter(line => Line.draws(line) == None, lines)
  | Some((graph, line, _)) =>
    let event = Line.redraw(line, Whole(graph));
    snd(
      List.fold_right(
        (line, (placed, kept)) =>
          Line.draws(line) == None
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
      switch (Line.draws(line)) {
      | None => line
      | Some(_) =>
        let graph =
          switch (Line.drawing(line), previous^) {
          | (Some(Event.Whole(graph)), _) => Some(graph)
          | (Some(Change(change)), Some(p)) =>
            Some(Delta.apply(p, change))
          | _ => None
          };
        switch (graph) {
        | None => line
        | Some(graph) =>
          let (drawing, bytes) =
            encode(~previous=previous^, ~since=since^, graph);
          previous := Some(graph);
          since := bytes;
          Line.redraw(line, drawing);
        };
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
            Event.Graph({
              drawing: Whole(graph),
              cause: Carried_over,
              partial: false,
            }),
          last^,
        ),
      );
    },
  );
};
