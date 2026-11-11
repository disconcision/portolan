/* portolan-extract <root> [--build-dir DIR] [--failing file:line,file:line]
                           [--previous FILE] [--broken file:line,file:line]

   Prints the graph of the dune project at <root> as JSON on stdout.
   The project's annotations must have been built first:
     dune build --root . --build-dir _build_portolan @check

   When that build fails, the modules that did not compile have no
   annotations. Pass the graph printed by an earlier run (--previous) to
   keep them in the picture as they were, and the compiler's error
   locations (--broken) to mark the definitions they fall in. */

let usage = "usage: portolan-extract <root> [--build-dir DIR] [--failing file:line,file:line] [--previous FILE] [--broken file:line,file:line]";

let die = message => {
  prerr_endline(message);
  exit(1);
};

/* "lib/scene.ml:20,lib/scene.ml:31" -> [("lib/scene.ml", 20); ...] */
let parse_lines = (~flag: string, s: string): list((string, int)) =>
  String.split_on_char(',', s)
  |> List.filter(entry => String.trim(entry) != "")
  |> List.map(entry => {
       let entry = String.trim(entry);
       switch (String.rindex_opt(entry, ':')) {
       | None => die(flag ++ ": expected file:line, got " ++ entry)
       | Some(i) =>
         let file = String.sub(entry, 0, i);
         let line = String.sub(entry, i + 1, String.length(entry) - i - 1);
         switch (int_of_string_opt(line)) {
         | Some(line) => (file, line)
         | None => die(flag ++ ": expected file:line, got " ++ entry)
         };
       };
     });

let () = {
  let root = ref(None);
  let build_dir = ref("_build_portolan");
  let failing = ref(None);
  let previous = ref(None);
  let broken = ref([]);
  let rec parse =
    fun
    | [] => ()
    | ["--build-dir", dir, ...rest] => {
        build_dir := dir;
        parse(rest);
      }
    | ["--failing", list, ...rest] => {
        failing := Some(parse_lines(~flag="--failing", list));
        parse(rest);
      }
    | ["--broken", list, ...rest] => {
        broken := parse_lines(~flag="--broken", list);
        parse(rest);
      }
    | ["--previous", file, ...rest] => {
        let text =
          switch (In_channel.with_open_bin(file, In_channel.input_all)) {
          | text => text
          | exception (Sys_error(message)) => die("--previous: " ++ message)
          };
        switch (Portolan_graph.Graph.of_string(text)) {
        | Ok(graph) => previous := Some(graph)
        | Error(message) =>
          die("--previous: " ++ file ++ " is not a graph: " ++ message)
        };
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
        ~previous=previous^,
        ~broken=broken^,
      )
    ) {
    | Ok(graph) => print_endline(Portolan_graph.Graph.to_string(graph))
    | Error(message) => die("portolan-extract: " ++ message)
    }
  };
};
