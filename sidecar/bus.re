let m = Mutex.create();
let c = Condition.create();
let events: Dynarray.t(string) = Dynarray.create();
let trace: ref(option(out_channel)) = ref(None);

/* The clock: milliseconds since this process started, on top of where the
   trace left off. */
let started = Unix.gettimeofday();
let base = ref(0);
let now_ms = () =>
  base^ + int_of_float((Unix.gettimeofday() -. started) *. 1000.);

let locked = f => {
  Mutex.lock(m);
  Fun.protect(~finally=() => Mutex.unlock(m), f);
};

let emit = fields =>
  locked(() => {
    let s =
      Yojson.Safe.to_string(`Assoc([("t", `Int(now_ms())), ...fields]));
    Dynarray.add_last(events, s);
    switch (trace^) {
    | Some(oc) =>
      output_string(oc, s);
      output_char(oc, '\n');
      flush(oc);
    | None => ()
    };
    Condition.broadcast(c);
  });

let time_of = line =>
  switch (Yojson.Safe.from_string(line)) {
  | `Assoc(fields) =>
    switch (List.assoc_opt("t", fields)) {
    | Some(`Int(t)) => Some(t)
    | _ => None
    }
  | _
  | exception _ => None
  };

let record = path => {
  let text =
    if (Sys.file_exists(path)) {
      Files.read(path);
    } else {
      "";
    };
  /* A sidecar killed mid-write leaves a cut line: leave those out. */
  let earlier =
    List.filter(
      l =>
        String.starts_with(~prefix="{", l)
        && String.ends_with(~suffix="}", l),
      Files.lines(text),
    );
  let last_t =
    switch (List.rev(earlier)) {
    | [last, ..._] => Option.value(time_of(last), ~default=0)
    | [] => 0
    };
  locked(() => {
    List.iter(Dynarray.add_last(events), earlier);
    base := last_t;
    let oc =
      open_out_gen([Open_append, Open_creat, Open_binary], 0o644, path);
    if (text != "" && !String.ends_with(~suffix="\n", text)) {
      output_char(oc, '\n');
    };
    trace := Some(oc);
    Condition.broadcast(c);
  });
  List.length(earlier);
};

let wait_from = from => {
  Mutex.lock(m);
  while (Dynarray.length(events) <= from) {
    Condition.wait(c, m);
  };
  let n = Dynarray.length(events);
  let batch = List.init(n - from, i => Dynarray.get(events, from + i));
  Mutex.unlock(m);
  batch;
};

let all = () => locked(() => Dynarray.to_list(events));

/* Events are written with "t" first and "type" second, so the type can be
   read off the front without parsing a whole (possibly large) event. */
let type_of = line =>
  try(
    Scanf.sscanf(line, "{\"t\":%d,\"type\":\"%[^\"]\"", (_, ty) => Some(ty))
  ) {
  | _ => None
  };

let last = ty => {
  let line =
    locked(() => {
      let rec back = i =>
        if (i < 0) {
          None;
        } else {
          let l = Dynarray.get(events, i);
          if (type_of(l) == Some(ty)) {
            Some(l);
          } else {
            back(i - 1);
          };
        };
      back(Dynarray.length(events) - 1);
    });
  switch (line) {
  | Some(l) =>
    try(Some(Yojson.Safe.from_string(l))) {
    | _ => None
    }
  | None => None
  };
};

let clip = (~n=6000, s) =>
  if (String.length(s) <= n) {
    s;
  } else {
    String.sub(s, 0, n) ++ "\n…";
  };
