/* portolan-extract <root> [--build-dir DIR] [--failing file:line,file:line]

   Prints the graph of the dune project at <root> as JSON on stdout.
   The project's annotations must have been built first:
     dune build --root . --build-dir _build_portolan @check */

let usage = "usage: portolan-extract <root> [--build-dir DIR] [--failing file:line,file:line]";

let die = message => {
  prerr_endline(message);
  exit(1);
};

/* "lib/scene.ml:20,lib/scene.ml:31" -> [("lib/scene.ml", 20); ...] */
let parse_failing = (s: string): list((string, int)) =>
  String.split_on_char(',', s)
  |> List.filter(entry => String.trim(entry) != "")
  |> List.map(entry => {
       let entry = String.trim(entry);
       switch (String.rindex_opt(entry, ':')) {
       | None => die("--failing: expected file:line, got " ++ entry)
       | Some(i) =>
         let file = String.sub(entry, 0, i);
         let line = String.sub(entry, i + 1, String.length(entry) - i - 1);
         switch (int_of_string_opt(line)) {
         | Some(line) => (file, line)
         | None => die("--failing: expected file:line, got " ++ entry)
         };
       };
     });

let () = {
  let root = ref(None);
  let build_dir = ref("_build_portolan");
  let failing = ref(None);
  let rec parse =
    fun
    | [] => ()
    | ["--build-dir", dir, ...rest] => {
        build_dir := dir;
        parse(rest);
      }
    | ["--failing", list, ...rest] => {
        failing := Some(parse_failing(list));
        parse(rest);
      }
    | ["-h" | "--help", ..._] => {
        print_endline(usage);
        exit(0);
      }
    | [arg, ...rest]
        when root^ == None && !String.starts_with(~prefix="-", arg) => {
        root := Some(arg);
        parse(rest);
      }
    | [arg, ..._] => die("unexpected argument: " ++ arg ++ "\n" ++ usage);

  parse(List.tl(Array.to_list(Sys.argv)));
  switch (root^) {
  | None => die(usage)
  | Some(root) =>
    switch (
      Portolan_extract.Extract.extract(
        ~root,
        ~build_dir=build_dir^,
        ~failing=failing^,
      )
    ) {
    | Ok(graph) => print_endline(Portolan_graph.Graph.to_string(graph))
    | Error(message) => die("portolan-extract: " ++ message)
    }
  };
};
