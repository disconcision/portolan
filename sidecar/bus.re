let m = Mutex.create();
let c = Condition.create();
let events: Dynarray.t(Event.Line.t) = Dynarray.create();
/* The trace file being written, with its path. */
let trace: ref(option((string, out_channel))) = ref(None);
/* How many times the trace has been started over. */
let traces = ref(0);

/* The clock: milliseconds since this process started, on top of where the
   trace left off. */
let started = Unix.gettimeofday();
let base = ref(0);
let elapsed = () => int_of_float((Unix.gettimeofday() -. started) *. 1000.);
let now_ms = () => base^ + elapsed();

let locked = f => {
  Mutex.lock(m);
  Fun.protect(~finally=() => Mutex.unlock(m), f);
};

/* Call with [m] held. */
let add = event => {
  let line = Event.Line.make(~time=now_ms(), event);
  Dynarray.add_last(events, line);
  switch (trace^) {
  | Some((_, oc)) =>
    output_string(oc, Event.Line.to_string(line));
    output_char(oc, '\n');
    flush(oc);
  | None => ()
  };
  Condition.broadcast(c);
};

let emit = event => locked(() => add(event));

let append = path =>
  open_out_gen([Open_append, Open_creat, Open_binary], 0o644, path);

let record = path => {
  let text =
    if (Sys.file_exists(path)) {
      Files.read(path);
    } else {
      "";
    };
  /* A sidecar killed mid-write leaves a cut line: leave those out. */
  let earlier =
    Files.lines(text)
    |> List.map(Event.Line.of_string)
    |> List.filter(Event.Line.is_whole);
  let last_t =
    switch (List.rev(earlier)) {
    | [last, ..._] => Option.value(Event.Line.time(last), ~default=0)
    | [] => 0
    };
  locked(() => {
    List.iter(Dynarray.add_last(events), earlier);
    base := last_t;
    let oc = append(path);
    if (text != "" && !String.ends_with(~suffix="\n", text)) {
      output_char(oc, '\n');
    };
    trace := Some((path, oc));
    Condition.broadcast(c);
  });
  List.length(earlier);
};

let start_over = (~archive, opening) =>
  locked(() => {
    switch (trace^) {
    | Some((path, oc)) =>
      close_out_noerr(oc);
      Files.mkdir_p(Filename.dirname(archive));
      Sys.rename(path, archive);
      trace := Some((path, append(path)));
    | None => ()
    };
    Dynarray.clear(events);
    incr(traces);
    base := - elapsed();
    List.iter(add, opening);
  });

type cursor = {
  trace: int,
  index: int,
};

let origin = {
  trace: (-1),
  index: 0,
};

let wait = cursor => {
  Mutex.lock(m);
  let from = () => cursor.trace == traces^ ? cursor.index : 0;
  while (Dynarray.length(events) <= from()) {
    Condition.wait(c, m);
  };
  let from = from()
  and n = Dynarray.length(events);
  let batch = List.init(n - from, i => Dynarray.get(events, from + i));
  let next = {
    trace: traces^,
    index: n,
  };
  Mutex.unlock(m);
  (batch, next, cursor.trace != next.trace);
};

let all = () => locked(() => Dynarray.to_list(events));

let last = kind => {
  let line =
    locked(() => {
      let rec back = i =>
        if (i < 0) {
          None;
        } else {
          let line = Dynarray.get(events, i);
          if (Event.Line.kind(line) == Some(kind)) {
            Some(line);
          } else {
            back(i - 1);
          };
        };
      back(Dynarray.length(events) - 1);
    });
  Option.bind(line, Event.Line.event);
};
