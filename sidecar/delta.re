let key = Yojson.Safe.to_string;

/* The new list as runs of the old one and the items it did not have. */
let list_ops = (old, now) => {
  let olds = Array.of_list(List.map(key, old));
  let where = Hashtbl.create(Array.length(olds));
  Array.iteri(
    (i, k) =>
      if (!Hashtbl.mem(where, k)) {
        Hashtbl.add(where, k, i);
      },
    olds,
  );
  let step = (ops, item) => {
    let k = key(item);
    switch (ops) {
    | [`Copy(start, n), ...rest]
        when start + n < Array.length(olds) && olds[start + n] == k => [
        `Copy((start, n + 1)),
        ...rest,
      ]
    | _ =>
      switch (Hashtbl.find_opt(where, k), ops) {
      | (Some(i), _) => [`Copy((i, 1)), ...ops]
      | (None, [`Add(items), ...rest]) => [
          `Add([item, ...items]),
          ...rest,
        ]
      | (None, _) => [`Add([item]), ...ops]
      }
    };
  };
  List.fold_left(step, [], now)
  |> List.rev_map(
       fun
       | `Copy(i, n) => `List([`Int(i), `Int(n)])
       | `Add(items) => `Assoc([("add", `List(List.rev(items)))]),
     );
};

let diff = (old, now) =>
  switch (old, now) {
  | (`Assoc(was), `Assoc(is)) =>
    let (set, lists) =
      List.fold_left(
        ((set, lists), (name, value)) =>
          switch (List.assoc_opt(name, was), value) {
          | (Some(before), _) when before == value => (set, lists)
          | (Some(`List(before)), `List(items)) => (
              set,
              [(name, `List(list_ops(before, items))), ...lists],
            )
          | _ => ([(name, value), ...set], lists)
          },
        ([], []),
        is,
      );
    let drop =
      List.filter_map(
        ((name, _)) =>
          List.mem_assoc(name, is) ? None : Some(`String(name)),
        was,
      );
    let part = (name, fields) =>
      fields == [] ? [] : [(name, `Assoc(List.rev(fields)))];
    Some(
      `Assoc(
        part("set", set)
        @ part("lists", lists)
        @ (drop == [] ? [] : [("drop", `List(drop))]),
      ),
    );
  | _ => None
  };

let replay = (old, ops) =>
  List.concat_map(
    fun
    | `List([`Int(i), `Int(n)]) => Array.to_list(Array.sub(old, i, n))
    | `Assoc([("add", `List(items))]) => items
    | _ => [],
    ops,
  );

let apply = (old, change) =>
  switch (old, change) {
  | (`Assoc(fields), `Assoc(parts)) =>
    let part = name =>
      switch (List.assoc_opt(name, parts)) {
      | Some(`Assoc(fields)) => fields
      | _ => []
      };
    let set = part("set")
    and lists = part("lists");
    let dropped = name =>
      switch (List.assoc_opt("drop", parts)) {
      | Some(`List(names)) => List.mem(`String(name), names)
      | _ => false
      };
    let patch = (name, value) =>
      switch (List.assoc_opt(name, set), List.assoc_opt(name, lists), value) {
      | (Some(v), _, _) => v
      | (None, Some(`List(ops)), `List(items)) =>
        `List(replay(Array.of_list(items), ops))
      | _ => value
      };
    `Assoc(
      (
        fields
        |> List.filter(((name, _)) => !dropped(name))
        |> List.map(((name, value)) => (name, patch(name, value)))
      )
      @ List.filter(((name, _)) => !List.mem_assoc(name, fields), set),
    );
  | _ => old
  };
