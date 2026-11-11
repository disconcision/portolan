(* The Portolan sidecar.

   One process, started for one project directory. It
   - watches the project's source files; on a change it builds the
     annotations into a build directory of its own, runs the extractor and
     the tests, and emits a new graph snapshot when the graph changed;
   - runs a coding agent (Claude Code in its headless streaming mode, or a
     scripted stand-in) and relays its stream;
   - serves the page and a server-sent event stream carrying both;
   - appends every event to a trace file, which `replay` plays back.

   Files on disk are the truth: the model is re-derived from them after
   every change, whoever made it. A build that fails emits no new graph,
   so the canvas keeps its last good picture.

   Dependencies: unix, threads, yojson. The graph is treated as opaque
   JSON here; only the extractor and the canvas know its shape. *)

let now () = Unix.gettimeofday ()
let t0 = now ()
let ms () = int_of_float ((now () -. t0) *. 1000.)
let ( / ) = Filename.concat

let read_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic (in_channel_length ic))

let write_file path s =
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> output_string oc s)

let rec mkdir_p d =
  if d <> "" && d <> "/" && not (Sys.file_exists d) then begin
    mkdir_p (Filename.dirname d);
    try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let truncate ?(n = 6000) s = if String.length s <= n then s else String.sub s 0 n ^ "\n…"

(* ---------- the event bus ---------- *)

module Bus = struct
  let m = Mutex.create ()
  let c = Condition.create ()
  let events : string Dynarray.t = Dynarray.create ()
  let trace : out_channel option ref = ref None

  let emit (fields : (string * Yojson.Safe.t) list) =
    let s = Yojson.Safe.to_string (`Assoc (("t", `Int (ms ())) :: fields)) in
    Mutex.lock m;
    Dynarray.add_last events s;
    (match !trace with
     | Some oc -> output_string oc s; output_char oc '\n'; flush oc
     | None -> ());
    Condition.broadcast c;
    Mutex.unlock m

  (* Blocks until there are events at or past [from]; returns them. *)
  let wait_from from =
    Mutex.lock m;
    while Dynarray.length events <= from do Condition.wait c m done;
    let n = Dynarray.length events in
    let batch = List.init (n - from) (fun i -> Dynarray.get events (from + i)) in
    Mutex.unlock m;
    batch

  let all () =
    Mutex.lock m;
    let l = Dynarray.to_list events in
    Mutex.unlock m;
    l
end

(* ---------- running commands ---------- *)

(* Runs a shell command in [cwd], returns (exit code, combined output).
   Killed after [timeout] seconds, reported as code 124. *)
let run_cmd ?(timeout = 120.) ~cwd cmd =
  let r, w = Unix.pipe ~cloexec:true () in
  let sh = Printf.sprintf "cd %s && %s" (Filename.quote cwd) cmd in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let pid = Unix.create_process "/bin/sh" [| "/bin/sh"; "-c"; sh |] devnull w w in
  Unix.close devnull;
  Unix.close w;
  let buf = Buffer.create 4096 and chunk = Bytes.create 4096 in
  let deadline = now () +. timeout in
  let timed_out = ref false in
  let rec loop () =
    let left = deadline -. now () in
    if left <= 0. then begin
      timed_out := true;
      (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ())
    end
    else
      match Unix.select [ r ] [] [] (Float.min left 1.0) with
      | [], _, _ -> loop ()
      | _ ->
        let n = try Unix.read r chunk 0 4096 with Unix.Unix_error _ -> 0 in
        if n > 0 then (Buffer.add_subbytes buf chunk 0 n; loop ())
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  loop ();
  Unix.close r;
  let code =
    match snd (Unix.waitpid [] pid) with
    | Unix.WEXITED n -> n
    | _ -> 125
    | exception Unix.Unix_error _ -> 125
  in
  ((if !timed_out then 124 else code), Buffer.contents buf)

(* ---------- configuration ---------- *)

let project = ref ""  (* absolute path of the watched project *)
let home = ref ""  (* the portolan repository: web/, built extractor, canvas bundle *)
let port = ref 8791
let build_dir = "_build_portolan"
let agent_kind = ref "claude"  (* "claude" | "script:<file>" *)
let model = ref ""
let extract_exe = ref ""

(* ---------- watcher: files -> build -> extract -> tests ---------- *)

let is_source name =
  List.exists (fun ext -> Filename.check_suffix name ext) [ ".ml"; ".mli"; ".re"; ".rei"; ".mll"; ".mly" ]
  || List.mem name [ "dune"; "dune-project"; "dune-workspace" ]

let signature () =
  let rec scan acc dir =
    Array.fold_left
      (fun acc name ->
        if name = "" || name.[0] = '.' || name.[0] = '_' || name = "node_modules" then acc
        else
          let p = dir / name in
          match (Unix.lstat p).Unix.st_kind with
          | Unix.S_DIR -> scan acc p
          | Unix.S_REG when is_source name ->
            let st = Unix.lstat p in
            (p, st.Unix.st_mtime, st.Unix.st_size) :: acc
          | _ -> acc
          | exception Unix.Unix_error _ -> acc)
      acc
      (try Sys.readdir dir with Sys_error _ -> [||])
  in
  List.sort compare (scan [] !project)

let last_graph = ref ""
let last_failing : (string * int) list option ref = ref None

(* What the agent last did to the files, to label the next graph change. *)
let cause_m = Mutex.create ()
let pending_cause : Yojson.Safe.t option ref = ref None

let take_cause () =
  Mutex.lock cause_m;
  let c = !pending_cause in
  pending_cause := None;
  Mutex.unlock cause_m;
  match c with Some c -> c | None -> `Assoc [ ("label", `String "edit"); ("by", `String "hand") ]

let set_cause c =
  Mutex.lock cause_m;
  pending_cause := Some c;
  Mutex.unlock cause_m

let extract failing =
  let failing_arg =
    match failing with
    | None -> ""
    | Some l ->
      " --failing "
      ^ Filename.quote (String.concat "," (List.map (fun (f, n) -> Printf.sprintf "%s:%d" f n) l))
  in
  run_cmd ~timeout:60. ~cwd:!project
    (Printf.sprintf "%s . --build-dir %s%s 2>/dev/null" (Filename.quote !extract_exe) build_dir failing_arg)

let emit_graph ~why failing =
  match extract failing with
  | 0, json when json <> "" ->
    if json <> !last_graph then begin
      match Yojson.Safe.from_string json with
      | g ->
        last_graph := json;
        let cause = match why with Some w -> w | None -> take_cause () in
        Bus.emit [ ("type", `String "graph"); ("graph", g); ("cause", cause) ]
      | exception _ -> Bus.emit [ ("type", `String "extract"); ("ok", `Bool false); ("output", `String (truncate json)) ]
    end
  | _, out -> Bus.emit [ ("type", `String "extract"); ("ok", `Bool false); ("output", `String (truncate out)) ]

(* "File "lib/scene.ml", line 21, characters 0-58: ... is false." *)
let parse_failing out =
  let acc = ref [] in
  let key = "File \"" in
  let rec go i =
    match Str.search_forward (Str.regexp_string key) out i with
    | exception Not_found -> ()
    | j ->
      let s = j + String.length key in
      (match String.index_from_opt out s '"' with
       | Some e ->
         let file = String.sub out s (e - s) in
         let rest = String.sub out e (min 40 (String.length out - e)) in
         (try
            Scanf.sscanf rest "\", line %d" (fun n -> if not (List.mem (file, n) !acc) then acc := (file, n) :: !acc)
          with _ -> ())
       | None -> ());
      go (s + 1)
  in
  go 0;
  List.rev !acc

let cycle () =
  if Sys.file_exists (!project / "dune-project") then begin
    let t = now () in
    let code, out =
      run_cmd ~cwd:!project (Printf.sprintf "dune build --root . --build-dir %s @check 2>&1" build_dir)
    in
    Bus.emit
      [ ("type", `String "build"); ("ok", `Bool (code = 0)); ("output", `String (truncate out));
        ("ms", `Int (int_of_float ((now () -. t) *. 1000.))) ];
    if code = 0 then begin
      (* First with the test results we already had, so pips do not blink. *)
      emit_graph ~why:None !last_failing;
      let tcode, tout =
        run_cmd ~timeout:90. ~cwd:!project (Printf.sprintf "dune test --root . --build-dir %s 2>&1" build_dir)
      in
      let failing = parse_failing tout in
      last_failing := Some failing;
      Bus.emit
        [ ("type", `String "tests"); ("ok", `Bool (tcode = 0)); ("output", `String (truncate tout));
          ("failing", `List (List.map (fun (f, n) -> `Assoc [ ("file", `String f); ("line", `Int n) ]) failing)) ];
      emit_graph ~why:(Some (`Assoc [ ("label", `String "tests") ])) (Some failing)
    end
  end

let watcher () =
  let last = ref [] and settled = ref [] in
  while true do
    (try
       let s = signature () in
       (* Act once the tree has been the same for two looks in a row. *)
       if s <> !last && s = !settled then (last := s; cycle ());
       settled := s
     with e -> Bus.emit [ ("type", `String "error"); ("where", `String "watcher"); ("message", `String (Printexc.to_string e)) ]);
    Thread.delay 0.25
  done

(* ---------- the agent ---------- *)

let agent_m = Mutex.create ()
let agent_in : out_channel option ref = ref None
let agent_pid : int option ref = ref None
let started_once = ref false

let agent_state s = Bus.emit [ ("type", `String "agent_state"); ("state", `String s) ]

let rel path =
  let p = !project ^ "/" in
  let n = String.length p in
  if String.length path > n && String.sub path 0 n = p then String.sub path n (String.length path - n) else path

(* Note which file a tool call is about to change, to label the next graph. *)
let track (ev : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  try
    if ev |> member "type" |> to_string = "assistant" then
      ev |> member "message" |> member "content" |> to_list
      |> List.iter (fun b ->
           if b |> member "type" |> to_string = "tool_use" then begin
             let name = b |> member "name" |> to_string in
             let input = b |> member "input" in
             match name with
             | "Write" | "Edit" | "MultiEdit" ->
               let f = try input |> member "file_path" |> to_string with _ -> "" in
               set_cause (`Assoc [ ("label", `String name); ("tool", `String name); ("file", `String (rel f)); ("by", `String "agent") ])
             | "Bash" ->
               let c = try input |> member "command" |> to_string with _ -> "" in
               set_cause (`Assoc [ ("label", `String "Bash"); ("tool", `String "Bash"); ("command", `String (truncate ~n:120 c)); ("by", `String "agent") ])
             | _ -> ()
           end)
  with _ -> ()

let relay_line line =
  match Yojson.Safe.from_string line with
  | ev ->
    track ev;
    Bus.emit [ ("type", `String "agent"); ("ev", ev) ];
    (try if Yojson.Safe.Util.(ev |> member "type" |> to_string) = "result" then agent_state "idle" with _ -> ())
  | exception _ -> if String.trim line <> "" then Bus.emit [ ("type", `String "agent_raw"); ("line", `String (truncate line)) ]

let prompt_extra = ref ""  (* a file whose text is added to the agent's instructions *)

let agent_prompt () =
  let f = !home / "sidecar" / "agent-prompt.md" in
  let base = if Sys.file_exists f then read_file f else "" in
  if !prompt_extra <> "" && Sys.file_exists !prompt_extra then base ^ "\n" ^ read_file !prompt_extra else base

let start_claude () =
  let in_r, in_w = Unix.pipe ~cloexec:true () in
  let out_r, out_w = Unix.pipe ~cloexec:true () in
  mkdir_p (!project / ".portolan");
  let err = Unix.openfile (!project / ".portolan" / "agent.err") [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ] 0o644 in
  let q = Filename.quote in
  let allowed =
    [ "Bash(dune build:*)"; "Bash(dune test:*)"; "Bash(dune runtest:*)"; "Bash(dune exec:*)";
      "Bash(dune clean:*)"; "Bash(dune fmt:*)"; "Bash(ls:*)"; "Bash(mkdir:*)"; "Bash(cat:*)" ]
  in
  let cmd =
    String.concat " "
      ([ "cd"; q !project; "&&"; "exec"; "claude"; "-p"; "--input-format"; "stream-json"; "--output-format";
         "stream-json"; "--verbose"; "--include-partial-messages"; "--permission-mode"; "acceptEdits" ]
       @ (if !started_once then [ "--continue" ] else [])
       @ (if !model <> "" then [ "--model"; q !model ] else [])
       @ (match agent_prompt () with "" -> [] | p -> [ "--append-system-prompt"; q p ])
       @ [ "--allowedTools" ] @ List.map q allowed)
  in
  let pid = Unix.create_process "/bin/sh" [| "/bin/sh"; "-c"; cmd |] in_r out_w err in
  Unix.close in_r; Unix.close out_w; Unix.close err;
  started_once := true;
  agent_in := Some (Unix.out_channel_of_descr in_w);
  agent_pid := Some pid;
  agent_state "running";
  let ic = Unix.in_channel_of_descr out_r in
  ignore
    (Thread.create
       (fun () ->
         (try while true do relay_line (input_line ic) done with End_of_file | Sys_error _ -> ());
         close_in_noerr ic;
         (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
         Mutex.lock agent_m;
         if !agent_pid = Some pid then (agent_pid := None; agent_in := None);
         Mutex.unlock agent_m;
         agent_state "exited")
       ())

(* A scripted stand-in: plays steps from a JSON file, doing the file
   operations itself and emitting events shaped like Claude Code's. *)
let run_script file =
  let open Yojson.Safe.Util in
  let assistant blocks =
    relay_line (Yojson.Safe.to_string (`Assoc [ ("type", `String "assistant"); ("message", `Assoc [ ("role", `String "assistant"); ("content", `List blocks) ]) ]))
  in
  let n = ref 0 in
  let tool name input =
    incr n;
    let id = Printf.sprintf "script_%d" !n in
    assistant [ `Assoc [ ("type", `String "tool_use"); ("id", `String id); ("name", `String name); ("input", input) ] ];
    id
  in
  let result id text =
    relay_line (Yojson.Safe.to_string (`Assoc [ ("type", `String "user"); ("message", `Assoc [ ("role", `String "user"); ("content", `List [ `Assoc [ ("type", `String "tool_result"); ("tool_use_id", `String id); ("content", `String text) ] ]) ]) ]))
  in
  let abs f = if Filename.is_relative f then !project / f else f in
  agent_state "running";
  (try
     Yojson.Safe.from_file file |> member "steps" |> to_list
     |> List.iter (fun step ->
          match step with
          | `Assoc [ ("say", `String s) ] -> assistant [ `Assoc [ ("type", `String "text"); ("text", `String s) ] ]
          | `Assoc [ ("wait", `Int w) ] -> Thread.delay (float_of_int w /. 1000.)
          | `Assoc [ ("write", w) ] ->
            let f = abs (w |> member "file" |> to_string) and content = w |> member "content" |> to_string in
            let id = tool "Write" (`Assoc [ ("file_path", `String f); ("content", `String content) ]) in
            mkdir_p (Filename.dirname f);
            write_file f content;
            result id "File written."
          | `Assoc [ ("edit", e) ] ->
            let f = abs (e |> member "file" |> to_string) in
            let o = e |> member "old" |> to_string and nw = e |> member "new" |> to_string in
            let id = tool "Edit" (`Assoc [ ("file_path", `String f); ("old_string", `String o); ("new_string", `String nw) ]) in
            let s = read_file f in
            (match Str.search_forward (Str.regexp_string o) s 0 with
             | i ->
               write_file f (String.sub s 0 i ^ nw ^ String.sub s (i + String.length o) (String.length s - i - String.length o));
               result id "Edited."
             | exception Not_found -> result id "Error: old text not found.")
          | _ -> ())
   with e -> Bus.emit [ ("type", `String "error"); ("where", `String "script"); ("message", `String (Printexc.to_string e)) ]);
  relay_line "{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false}"

let send_prompt text =
  Bus.emit [ ("type", `String "prompt"); ("text", `String text) ];
  let script =
    let k = "script:" in
    let n = String.length k in
    if String.length !agent_kind > n && String.sub !agent_kind 0 n = k then
      Some (String.sub !agent_kind n (String.length !agent_kind - n))
    else None
  in
  match script with
  | Some file -> ignore (Thread.create run_script file)
  | None ->
    Mutex.lock agent_m;
    if !agent_in = None then start_claude ();
    (match !agent_in with
     | Some oc ->
       let msg = `Assoc [ ("type", `String "user"); ("message", `Assoc [ ("role", `String "user"); ("content", `String text) ]) ] in
       (try output_string oc (Yojson.Safe.to_string msg); output_char oc '\n'; flush oc; agent_state "running"
        with Sys_error _ -> agent_state "exited")
     | None -> ());
    Mutex.unlock agent_m

let stop_agent () =
  Mutex.lock agent_m;
  (match !agent_pid with Some pid -> (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ()) | None -> ());
  Mutex.unlock agent_m

(* ---------- http ---------- *)

let url_decode s =
  let b = Buffer.create (String.length s) in
  let i = ref 0 in
  while !i < String.length s do
    (match s.[!i] with
     | '%' when !i + 2 < String.length s ->
       (try Buffer.add_char b (Char.chr (int_of_string ("0x" ^ String.sub s (!i + 1) 2))); i := !i + 2
        with _ -> Buffer.add_char b '%')
     | '+' -> Buffer.add_char b ' '
     | c -> Buffer.add_char b c);
    incr i
  done;
  Buffer.contents b

let parse_target target =
  match String.index_opt target '?' with
  | None -> (url_decode target, [])
  | Some i ->
    let q = String.sub target (i + 1) (String.length target - i - 1) in
    ( url_decode (String.sub target 0 i),
      List.filter_map
        (fun kv ->
          match String.index_opt kv '=' with
          | Some j -> Some (url_decode (String.sub kv 0 j), url_decode (String.sub kv (j + 1) (String.length kv - j - 1)))
          | None -> None)
        (String.split_on_char '&' q) )

let ctype path =
  let has = Filename.check_suffix path in
  if has ".html" then "text/html; charset=utf-8"
  else if has ".js" then "text/javascript; charset=utf-8"
  else if has ".css" then "text/css; charset=utf-8"
  else if has ".json" || has ".jsonl" then "application/json; charset=utf-8"
  else if has ".svg" then "image/svg+xml"
  else if has ".woff2" then "font/woff2"
  else "text/plain; charset=utf-8"

let respond oc ?(status = "200 OK") ?(ctype = "text/plain; charset=utf-8") body =
  Printf.fprintf oc "HTTP/1.1 %s\r\nContent-Type: %s\r\nContent-Length: %d\r\nCache-Control: no-store\r\nConnection: close\r\n\r\n%s"
    status ctype (String.length body) body;
  flush oc

let safe p = not (List.mem ".." (String.split_on_char '/' p))

let serve_file oc path =
  if safe path && Sys.file_exists path && not (Sys.is_directory path) then respond oc ~ctype:(ctype path) (read_file path)
  else respond oc ~status:"404 Not Found" "not found"

let sse_headers oc =
  output_string oc "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nCache-Control: no-cache\r\nConnection: keep-alive\r\n\r\n";
  flush oc

let sse oc s = output_string oc "data: "; output_string oc s; output_string oc "\n\n"

(* Live: everything so far at once, then a marker, then events as they come. *)
let events_live oc =
  sse_headers oc;
  let cursor = ref 0 and first = ref true in
  while true do
    let batch = Bus.wait_from !cursor in
    List.iter (sse oc) batch;
    cursor := !cursor + List.length batch;
    if !first then (sse oc "{\"type\":\"caught_up\"}"; first := false);
    flush oc
  done

(* Replay: a recorded trace, with its own timing, from the start for each viewer. *)
let replay_lines : string list ref = ref []
let replay_speed = ref 1.0

let events_replay oc =
  sse_headers oc;
  sse oc "{\"type\":\"caught_up\"}";
  flush oc;
  let prev = ref 0 in
  List.iter
    (fun line ->
      let t = try Yojson.Safe.Util.(Yojson.Safe.from_string line |> member "t" |> to_int) with _ -> !prev in
      let gap = Float.min 2.0 (float_of_int (t - !prev) /. 1000. /. !replay_speed) in
      if gap > 0.005 then Thread.delay gap;
      prev := t;
      sse oc line;
      flush oc)
    !replay_lines;
  sse oc "{\"type\":\"replay_end\"}";
  flush oc;
  while true do Thread.delay 30.; output_string oc ": keep\n\n"; flush oc done

let replaying = ref false

let handle fd =
  let ic = Unix.in_channel_of_descr fd and oc = Unix.out_channel_of_descr fd in
  (try
     let line = String.trim (input_line ic) in
     let meth, target = match String.split_on_char ' ' line with m :: t :: _ -> (m, t) | _ -> ("", "/") in
     let rec headers acc =
       match String.trim (input_line ic) with
       | "" -> acc
       | l ->
         (match String.index_opt l ':' with
          | Some i ->
            headers ((String.lowercase_ascii (String.sub l 0 i), String.trim (String.sub l (i + 1) (String.length l - i - 1))) :: acc)
          | None -> headers acc)
     in
     let hs = headers [] in
     let body = match List.assoc_opt "content-length" hs with Some n -> really_input_string ic (int_of_string n) | None -> "" in
     let path, query = parse_target target in
     match (meth, path) with
     | "GET", "/events" -> if !replaying then events_replay oc else events_live oc
     | "GET", ("/" | "/index.html") -> serve_file oc (!home / "web" / "index.html")
     | "GET", "/trace" -> respond oc ~ctype:"application/json; charset=utf-8" (String.concat "\n" (if !replaying then !replay_lines else Bus.all ()))
     | "GET", "/source" ->
       let file = try List.assoc "file" query with Not_found -> "" in
       let p = !project / file in
       if !project <> "" && safe file && Sys.file_exists p then respond oc (read_file p) else respond oc ~status:"404 Not Found" "not found"
     | "POST", "/prompt" when not !replaying ->
       let text = try Yojson.Safe.Util.(Yojson.Safe.from_string body |> member "text" |> to_string) with _ -> body in
       if String.trim text <> "" then send_prompt text;
       respond oc ~ctype:"application/json" "{\"ok\":true}"
     | "POST", "/stop" when not !replaying -> stop_agent (); respond oc ~ctype:"application/json" "{\"ok\":true}"
     | "GET", p when String.length p > 8 && String.sub p 0 8 = "/canvas/" ->
       serve_file oc (!home / "canvas" / "dist" / String.sub p 8 (String.length p - 8))
     | "GET", p -> serve_file oc (!home / "web" / String.sub p 1 (String.length p - 1))
     | _ -> respond oc ~status:"405 Method Not Allowed" "no"
   with _ -> ());
  (try flush oc with _ -> ());
  try Unix.close fd with _ -> ()

let serve () =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, !port));
  Unix.listen sock 64;
  Printf.printf "Portolan: http://127.0.0.1:%d/\n%!" !port;
  while true do
    match Unix.accept sock with
    | fd, _ -> ignore (Thread.create handle fd)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
  done

(* ---------- entry ---------- *)

let find_home () =
  let rec up d n =
    if Sys.file_exists (d / "web" / "index.html") then d
    else if n = 0 || d = "/" then Sys.getcwd ()
    else up (Filename.dirname d) (n - 1)
  in
  up (Filename.dirname (Unix.realpath Sys.executable_name)) 6

let absolute p = if Filename.is_relative p then Sys.getcwd () / p else p

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let args = Array.to_list Sys.argv |> List.tl in
  let rec flags = function
    | "--port" :: n :: r -> port := int_of_string n; flags r
    | "--home" :: h :: r -> home := absolute h; flags r
    | "--agent" :: a :: r -> agent_kind := a; flags r
    | "--model" :: m :: r -> model := m; flags r
    | "--nudge" :: f :: r -> prompt_extra := absolute f; flags r
    | "--extract" :: e :: r -> extract_exe := absolute e; flags r
    | "--speed" :: s :: r -> replay_speed := float_of_string s; flags r
    | x :: r -> x :: flags r
    | [] -> []
  in
  let positional = flags args in
  if !home = "" then home := find_home ();
  if !extract_exe = "" then extract_exe := !home / "_build" / "default" / "extract" / "main.exe";
  match positional with
  | [ "serve"; dir ] ->
    project := absolute dir;
    if not (Sys.file_exists !project) then begin
      (* A new project gets a workspace file, so dune treats it as its own
         root even when it sits inside another dune project. *)
      mkdir_p !project;
      write_file (!project / "dune-workspace") "(lang dune 3.16)\n"
    end;
    project := Unix.realpath !project;
    mkdir_p (!project / ".portolan");
    let stamp = let tm = Unix.localtime (now ()) in Printf.sprintf "%04d%02d%02d-%02d%02d%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec in
    Bus.trace := Some (open_out (!project / ".portolan" / ("trace-" ^ stamp ^ ".jsonl")));
    Bus.emit [ ("type", `String "hello"); ("project", `String (Filename.basename !project)); ("agent", `String !agent_kind); ("mode", `String "live") ];
    ignore (Thread.create watcher ());
    serve ()
  | [ "replay"; file ] ->
    replaying := true;
    replay_lines := List.filter (fun l -> String.trim l <> "") (String.split_on_char '\n' (read_file (absolute file)));
    serve ()
  | _ ->
    prerr_endline
      "usage:\n  portolan serve <project-dir> [--port N] [--agent claude|script:FILE] [--model M] [--nudge FILE]\n  portolan replay <trace.jsonl> [--port N] [--speed X]";
    exit 2
