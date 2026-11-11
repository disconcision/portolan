open Portolan_sidecar;
open Testing;

let said = (line, expected) =>
  Alcotest.(check(bool))(line, true, Protocol.read(line) == expected);

let written = (expected, message) =>
  Alcotest.(check(string))(expected, expected, Protocol.to_string(message));

/* Paths under /project are the project's. */
let relative = path => {
  let root = "/project/";
  String.starts_with(~prefix=root, path)
    ? Some(
        String.sub(
          path,
          String.length(root),
          String.length(path) - String.length(root),
        ),
      )
    : None;
};

let tool_use = (name, input) =>
  Printf.sprintf(
    {|{"type":"tool_use","id":"toolu_1","name":"%s","input":%s}|},
    name,
    input,
  );

let assistant = blocks =>
  parse(
    Printf.sprintf(
      {|{"type":"assistant","message":{"role":"assistant","content":[%s]}}|},
      String.concat(",", blocks),
    ),
  );

let cause = (name, blocks, expected) =>
  Alcotest.(check(bool))(
    name,
    true,
    Protocol.cause(~relative, assistant(blocks)) == expected,
  );

let questions = {|{"questions":[{"question":"Which?","options":[{"label":"A"},{"label":"B"}]}]}|};

let tests = [
  case("the start of a turn names Claude Code's session", () => {
    let init = {|{"type":"system","subtype":"init","session_id":"abc","model":"haiku"}|};
    said(init, Began(parse(init), Some("abc")));
    let bare = {|{"type":"system","subtype":"init"}|};
    said(bare, Began(parse(bare), None));
  }),
  case("a result ends the turn", () => {
    let result = {|{"type":"result","subtype":"success","is_error":false}|};
    said(result, Ended(parse(result)));
  }),
  case("any other event of the stream is passed on", () =>
    List.iter(
      line => said(line, Told(parse(line))),
      [
        {|{"type":"assistant","message":{"role":"assistant","content":[]}}|},
        {|{"type":"system","subtype":"status"}|},
        {|{"type":"stream_event","event":{"type":"content_block_delta"}}|},
        {|{"no type":true}|},
      ],
    )
  ),
  case("a request to use a tool is read whole", () =>
    said(
      {|{"type":"control_request","request_id":"req_a","request":{"subtype":"can_use_tool","tool_name":"Bash","input":{"command":"make"},"permission_suggestions":[{"type":"setMode"}],"tool_use_id":"toolu_a"}}|},
      Asked({
        id: "req_a",
        tool: "Bash",
        input: parse({|{"command":"make"}|}),
        suggestions: parse({|[{"type":"setMode"}]|}),
        tool_use: Some("toolu_a"),
      }),
    )
  ),
  case("a request may come without suggestions or a tool use", () =>
    said(
      {|{"type":"control_request","request_id":"req_c","request":{"subtype":"can_use_tool","tool_name":"Read","input":{"file_path":"/etc/hosts"}}}|},
      Asked({
        id: "req_c",
        tool: "Read",
        input: parse({|{"file_path":"/etc/hosts"}|}),
        suggestions: `Null,
        tool_use: None,
      }),
    )
  ),
  case("a request of another kind is owed a refusal", () =>
    said(
      {|{"type":"control_request","request_id":"req_u","request":{"subtype":"mcp_message"}}|},
      Asked_other("req_u"),
    )
  ),
  case("a request too malformed to answer is dropped", () =>
    List.iter(
      line => said(line, Nothing),
      [
        {|{"type":"control_request","request":{"subtype":"can_use_tool"}}|},
        {|{"type":"control_request","request_id":"r"}|},
        {|{"type":"control_request","request_id":"r","request":{"subtype":"can_use_tool","input":{}}}|},
      ],
    )
  ),
  case("a request can be withdrawn", () => {
    said(
      {|{"type":"control_cancel_request","request_id":"req_x"}|},
      Withdrew("req_x"),
    );
    said({|{"type":"control_cancel_request"}|}, Nothing);
  }),
  case("the acknowledgement of a request of ours says nothing", () =>
    said(
      {|{"type":"control_response","response":{"subtype":"success","request_id":"stop-1"}}|},
      Nothing,
    )
  ),
  case("output that is not a JSON object is raw, unless it is blank", () => {
    said("this is not json", Raw("this is not json"));
    said("{\"type\":\"assis", Raw("{\"type\":\"assis"));
    said("[1,2]", Raw("[1,2]"));
    said("12", Raw("12"));
    said("", Nothing);
    said("  \t", Nothing);
  }),
  case("a write or an edit is the cause, by its path in the project", () => {
    cause(
      "write",
      [tool_use("Write", {|{"file_path":"/project/lib/scene.ml"}|})],
      Some(
        Wrote({
          tool: "Write",
          file: "lib/scene.ml",
        }),
      ),
    );
    cause(
      "edit outside",
      [tool_use("MultiEdit", {|{"file_path":"/etc/hosts"}|})],
      Some(
        Wrote({
          tool: "MultiEdit",
          file: "/etc/hosts",
        }),
      ),
    );
  }),
  case("a shell command is the cause, cut if it is long", () => {
    cause(
      "short",
      [tool_use("Bash", {|{"command":"dune build"}|})],
      Some(Ran("dune build")),
    );
    cause(
      "long",
      [
        tool_use("Bash", "{\"command\":\"" ++ String.make(200, 'x') ++ "\"}"),
      ],
      Some(Ran(String.make(120, 'x') ++ "\n…")),
    );
  }),
  case("of several tool uses the last one that changes files is the cause", () =>
    cause(
      "three",
      [
        tool_use("Bash", {|{"command":"ls"}|}),
        tool_use("Edit", {|{"file_path":"/project/a.ml"}|}),
        tool_use("Read", {|{"file_path":"/project/b.ml"}|}),
      ],
      Some(
        Wrote({
          tool: "Edit",
          file: "a.ml",
        }),
      ),
    )
  ),
  case("words, reads and results change nothing", () => {
    cause("text", [{|{"type":"text","text":"Write Bash Edit"}|}], None);
    cause(
      "read",
      [tool_use("Read", {|{"file_path":"/project/a.ml"}|})],
      None,
    );
    Alcotest.(check(bool))(
      "a result",
      true,
      Protocol.cause(
        ~relative,
        parse(
          {|{"type":"user","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"toolu_1","content":"Edit"}]}}|},
        ),
      )
      == None,
    );
    Alcotest.(check(bool))(
      "not a message",
      true,
      Protocol.cause(~relative, parse({|{"type":"result"}|})) == None
      && Protocol.cause(~relative, parse("[]")) == None,
    );
  }),
  case("a prompt goes in as the user's message", () =>
    written(
      {|{"type":"user","message":{"role":"user","content":"Add a perimeter."}}|},
      Protocol.prompt("Add a perimeter."),
    )
  ),
  case("allowing once gives the tool its input back", () =>
    written(
      {|{"type":"control_response","response":{"subtype":"success","request_id":"req_b","response":{"behavior":"allow","updatedInput":{"command":"rm -rf old"}}}}|},
      Protocol.answer(
        ~id="req_b",
        ~input=parse({|{"command":"rm -rf old"}|}),
        Once,
      ),
    )
  ),
  case("allowing always adds the patterns as rules for the session", () =>
    written(
      {|{"type":"control_response","response":{"subtype":"success","request_id":"req_a","response":{"behavior":"allow","updatedInput":{"command":"git push"},"updatedPermissions":[{"type":"addRules","rules":[{"toolName":"Bash","ruleContent":"git push:*"},{"toolName":"WebSearch"}],"behavior":"allow","destination":"session"}]}}}|},
      Protocol.answer(
        ~id="req_a",
        ~input=parse({|{"command":"git push"}|}),
        Always(["Bash(git push:*)", "WebSearch"]),
      ),
    )
  ),
  case("allowing always with no patterns is allowing once", () =>
    Alcotest.(check(string))(
      "same line",
      Protocol.to_string(Protocol.answer(~id="r", ~input=`Null, Once)),
      Protocol.to_string(
        Protocol.answer(~id="r", ~input=`Null, Always([])),
      ),
    )
  ),
  case("a denial tells the agent not to retry", () =>
    written(
      {|{"type":"control_response","response":{"subtype":"success","request_id":"req_c","response":{"behavior":"deny","message":"The user declined this in Portolan's chat. Do not retry it; carry on another way or ask what they would prefer."}}}|},
      Protocol.answer(~id="req_c", ~input=parse("{}"), Deny),
    )
  ),
  case("answers go back in the questions' input, replacing any there", () => {
    written(
      {|{"type":"control_response","response":{"subtype":"success","request_id":"req_q","response":{"behavior":"allow","updatedInput":{"questions":[{"question":"Which?","options":[{"label":"A"},{"label":"B"}]}],"answers":{"Which?":"B"}}}}}|},
      Protocol.answer(
        ~id="req_q",
        ~input=parse(questions),
        Answers([("Which?", "B")]),
      ),
    );
    written(
      {|{"type":"control_response","response":{"subtype":"success","request_id":"req_q","response":{"behavior":"allow","updatedInput":{"questions":[],"answers":{"Which?":"A, B"}}}}}|},
      Protocol.answer(
        ~id="req_q",
        ~input=parse({|{"answers":{"old":"x"},"questions":[]}|}),
        Answers([("Which?", "A, B")]),
      ),
    );
  }),
  case("a request Portolan does not handle is refused", () =>
    written(
      {|{"type":"control_response","response":{"subtype":"error","request_id":"req_u","error":"Portolan does not handle this request."}}|},
      Protocol.refusal(~id="req_u"),
    )
  ),
  case("a stop is an interrupt request", () =>
    written(
      {|{"type":"control_request","request_id":"stop-7","request":{"subtype":"interrupt"}}|},
      Protocol.interrupt(~id="stop-7"),
    )
  ),
];
