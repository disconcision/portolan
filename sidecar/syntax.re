type t =
  | Ml
  | Re;

let name =
  fun
  | Ml => "ml"
  | Re => "re";

let of_string =
  fun
  | "ml" => Some(Ml)
  | "re" => Some(Re)
  | _ => None;

/* The syntax a file is written in, and whether it is an interface. */
let of_path = path =>
  switch (Filename.extension(path)) {
  | ".ml" => Some((Ml, false))
  | ".mli" => Some((Ml, true))
  | ".re" => Some((Re, false))
  | ".rei" => Some((Re, true))
  | _ => None
  };

let slice = (~first, ~last, text) => {
  let lines = Array.of_list(String.split_on_char('\n', text));
  let n = Array.length(lines);
  let first = max(1, first);
  let last = min(n, Option.value(last, ~default=n));
  if (first > last) {
    "";
  } else {
    String.concat(
      "\n",
      Array.to_list(Array.sub(lines, first - 1, last - first + 1)),
    );
  };
};

let convert = (~path, ~first, ~last, ~target) => {
  let original = slice(~first, ~last, Files.read(path));
  switch (of_path(path)) {
  | None => (original, false)
  | Some((source, _)) when source == target => (original, true)
  | Some((source, interface)) =>
    let tmp = Filename.temp_file("portolan-", Filename.extension(path));
    Fun.protect(
      ~finally=
        () =>
          try(Sys.remove(tmp)) {
          | Sys_error(_) => ()
          },
      () => {
        Files.write(tmp, original);
        let command =
          Printf.sprintf(
            "refmt --parse %s --print %s --interface %b %s 2>/dev/null",
            name(source),
            name(target),
            interface,
            Filename.quote(tmp),
          );
        switch (Run.cmd(~timeout=20., ~cwd=Filename.dirname(tmp), command)) {
        | (0, converted) when String.trim(converted) != "" => (
            converted,
            true,
          )
        | _ => (original, false)
        };
      },
    );
  };
};

/* ---------- types ---------- */

/* Printed types already converted, by their OCaml text. */
let converted: Hashtbl.t(string, option(string)) = Hashtbl.create(1024);
let lock = Mutex.create();

/* refmt breaks long types over lines and leaves trailing commas; a type
   on the canvas is one line. */
let one_line = text => {
  let sub = (pattern, by, s) =>
    Str.global_replace(Str.regexp(pattern), by, s);
  let s = String.trim(sub("[ \n\t]+", " ", text));
  let s =
    String.ends_with(~suffix=";", s)
      ? String.sub(s, 0, String.length(s) - 1) : s;
  let s = sub(",? \\([})]\\)", "\\1", s) |> sub("\\([{(]\\) ", "\\1");
  String.starts_with(~prefix="| ", s)
    ? String.sub(s, 2, String.length(s) - 2) : s;
};

let split_types = (~count, converted) => {
  let bodies =
    Str.full_split(Str.regexp("^type p[0-9]+ ="), converted)
    |> List.filter_map(
         fun
         | Str.Text(body) when String.trim(body) != "" =>
           Some(one_line(body))
         | _ => None,
       );
  List.length(bodies) == count ? Some(bodies) : None;
};

/* One `refmt` run over all the texts, each as the body of a type
   definition; [None] if `refmt` rejects any of them. */
let batch = texts => {
  let tmp = Filename.temp_file("portolan-types-", ".ml");
  Fun.protect(
    ~finally=
      () =>
        try(Sys.remove(tmp)) {
        | Sys_error(_) => ()
        },
    () => {
      Files.write(
        tmp,
        String.concat(
          "\n",
          List.mapi((i, t) => Printf.sprintf("type p%d = %s", i, t), texts),
        ),
      );
      let command =
        Printf.sprintf(
          "refmt --parse ml --print re --print-width 400 %s 2>/dev/null",
          Filename.quote(tmp),
        );
      switch (Run.cmd(~timeout=30., ~cwd=Filename.dirname(tmp), command)) {
      | (0, out) => split_types(~count=List.length(texts), out)
      | _ => None
      };
    },
  );
};

/* Halves a rejected batch until the texts `refmt` cannot read are alone. */
let rec convert_all = texts =>
  switch (texts) {
  | [] => []
  | _ =>
    switch (batch(texts)) {
    | Some(bodies) => List.map(Option.some, bodies)
    | None =>
      switch (texts) {
      | [_] => [None]
      | _ =>
        let half = List.length(texts) / 2;
        convert_all(List.filteri((i, _) => i < half, texts))
        @ convert_all(List.filteri((i, _) => i >= half, texts));
      }
    }
  };

let types = texts => {
  Mutex.lock(lock);
  Fun.protect(
    ~finally=() => Mutex.unlock(lock),
    () => {
      let fresh =
        List.sort_uniq(compare, texts)
        |> List.filter(t =>
             String.trim(t) != "" && !Hashtbl.mem(converted, t)
           );
      List.iter2(Hashtbl.replace(converted), fresh, convert_all(fresh));
      List.map(t => Option.join(Hashtbl.find_opt(converted, t)), texts);
    },
  );
};
