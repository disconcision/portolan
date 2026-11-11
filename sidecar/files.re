let (/) = Filename.concat;

let read = path => {
  let ic = open_in_bin(path);
  Fun.protect(
    ~finally=() => close_in_noerr(ic),
    () => really_input_string(ic, in_channel_length(ic)),
  );
};

let write = (path, text) => {
  let oc = open_out_bin(path);
  Fun.protect(
    ~finally=() => close_out_noerr(oc),
    () => output_string(oc, text),
  );
};

let rec mkdir_p = dir =>
  if (dir != "" && dir != "/" && !Sys.file_exists(dir)) {
    mkdir_p(Filename.dirname(dir));
    try(Unix.mkdir(dir, 0o755)) {
    | Unix.Unix_error(Unix.EEXIST, _, _) => ()
    };
  };

let private_dir = dir => {
  mkdir_p(dir);
  let ignore_file = dir / ".gitignore";
  if (!Sys.file_exists(ignore_file)) {
    write(ignore_file, "*\n");
  };
};

let lines = text =>
  List.filter(l => String.trim(l) != "", String.split_on_char('\n', text));
