(* Reads a dune project's .cmt files and builds the Portolan graph.

   Two passes over the typed trees:
   1. [collect]: flatten every module (and nested module) into a list of
      items, and record which names the project declares, so that a
      reference from one file to another can be resolved.
   2. [emit]: turn each item into a node, an edge, a value or a test.

   Nothing here needs the compiler's environment or load path: paths are
   resolved by hand against the names collected in pass 1. *)

module G = Portolan_graph.Graph
open Typedtree

(* ---------- strings and files ---------- *)

let ( // ) = Filename.concat

(* "Vector__Scene" -> ["Vector"; "Scene"] *)
let split_double_underscore (s : string) : string list =
  let n = String.length s in
  let rec go start i acc =
    if i + 1 >= n then List.rev (String.sub s start (n - start) :: acc)
    else if s.[i] = '_' && s.[i + 1] = '_' && i > start then
      go (i + 2) (i + 2) (String.sub s start (i - start) :: acc)
    else go start (i + 1) acc
  in
  List.filter (fun part -> part <> "") (go 0 0 [])

(* Collapse every run of whitespace into one space. *)
let one_line (s : string) : string =
  String.split_on_char ' '
    (String.map (function '\n' | '\t' | '\r' -> ' ' | c -> c) s)
  |> List.filter (fun w -> w <> "")
  |> String.concat " "

let contains ~(sub : string) (s : string) : bool =
  let n = String.length sub and m = String.length s in
  let rec at i = i + n <= m && (String.sub s i n = sub || at (i + 1)) in
  at 0

let dedup (l : string list) : string list =
  List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) [] l
  |> List.rev

let qualify (mpath : string list) (name : string) : string =
  String.concat "." (mpath @ [ name ])

(* Every .cmt / .cmti under [dir], sorted. *)
let rec find_cmts (dir : string) : string list =
  match Sys.readdir dir with
  | exception Sys_error _ -> []
  | entries ->
      Array.sort compare entries;
      Array.to_list entries
      |> List.concat_map (fun entry ->
             let path = dir // entry in
             if Sys.is_directory path then find_cmts path
             else if
               Filename.check_suffix entry ".cmt"
               || Filename.check_suffix entry ".cmti"
             then [ path ]
             else [])

(* ---------- what pass 1 collects ---------- *)

(* What an interface lets through. [Only sg]: just what [sg] lists. *)
type vis = All | Only of Types.signature | Nothing

(* Where an item sits: its enclosing modules, and what they export. *)
type scope = { mpath : string list; vis : vis }

type item =
  | Type_item of scope * type_declaration
  | Value_item of scope * value_binding * Location.t  (* loc of the whole item *)
  | Prim_item of scope * value_description  (* external f : ... *)

(* One compilation unit (one .ml file). The three tables map the idents
   the unit declares to their qualified names; idents are only unique
   within a unit, so each unit has its own. *)
type unit_info = {
  u_modname : string;  (* as compiled: "Vector__Scene" *)
  u_mpath : string list;  (* as shown: ["Scene"] *)
  u_file : string;  (* relative to the root: "lib/scene.ml" *)
  u_str : structure;
  u_vis : vis;
  u_types : string Ident.Tbl.t;
  u_mods : string Ident.Tbl.t;
  u_vals : string Ident.Tbl.t;
  u_mtys : Types.signature Ident.Tbl.t;  (* [module type S = sig ... end] *)
  mutable u_items : item list;  (* reversed *)
}

(* A top-level definition before the graph is assembled: [mentions] are
   the project names its body refers to, not yet filtered. *)
type def = Edge of G.edge * string list | Value of G.value * string list

type ctx = {
  failing : (string * int) list option;
  units : (string, unit_info) Hashtbl.t;  (* by compiled name *)
  (* dune's alias modules: ("Vector", "Scene") -> "Vector__Scene" *)
  wrappers : (string * string, string) Hashtbl.t;
  (* [module S = Scene] inside the project: "Other.S" -> "Scene" *)
  mod_aliases : (string, string) Hashtbl.t;
  (* every type the project declares, by node key *)
  type_decls : (string, unit_info * scope * type_declaration) Hashtbl.t;
  (* every top-level value the project binds, by qualified name *)
  def_names : (string, unit) Hashtbl.t;
  seen : (string, unit) Hashtbl.t;  (* node keys already emitted *)
  mutable nodes : G.node list;  (* reversed *)
  mutable defs : def list;  (* reversed *)
  mutable tests : (G.test * string list) list;  (* reversed, with mentions *)
}

(* ---------- resolving paths to project names ---------- *)

let unit_name (ctx : ctx) (modname : string) : string option =
  Hashtbl.find_opt ctx.units modname
  |> Option.map (fun u -> String.concat "." u.u_mpath)

(* The qualified name of the project module a path denotes, if any. *)
let rec module_prefix (ctx : ctx) (u : unit_info) (p : Path.t) : string option =
  match p with
  | Pident id -> (
      match Ident.Tbl.find_opt u.u_mods id with
      | Some prefix -> Some prefix
      | None ->
          if Ident.persistent id then unit_name ctx (Ident.name id) else None)
  | Pdot (Pident w, sub) when Hashtbl.mem ctx.wrappers (Ident.name w, sub) ->
      (* Vector.Scene, through dune's alias module *)
      unit_name ctx (Hashtbl.find ctx.wrappers (Ident.name w, sub))
  | Pdot (m, sub) ->
      module_prefix ctx u m
      |> Option.map (fun prefix ->
             let q = prefix ^ "." ^ sub in
             Option.value (Hashtbl.find_opt ctx.mod_aliases q) ~default:q)
  | Papply _ | Pextra_ty _ -> None

(* The node key of a project type, if the path names one. [from] is the
   module the reference is written in. *)
let project_type (ctx : ctx) (u : unit_info) ~(from : string list) (p : Path.t) :
    string option =
  let declared key = if Hashtbl.mem ctx.type_decls key then Some key else None in
  match p with
  | Pident id -> (
      match Ident.Tbl.find_opt u.u_types id with
      | Some key -> Some key
      | None ->
          (* An ident pass 1 did not see (it came through an include, say):
             look the name up in the enclosing modules, innermost first. *)
          let rec outward = function
            | [] -> None
            | _ :: rest as rev_mpath -> (
                match declared (qualify (List.rev rev_mpath) (Ident.name id)) with
                | Some key -> Some key
                | None -> outward rest)
          in
          outward (List.rev from))
  | Pdot (m, name) ->
      Option.bind (module_prefix ctx u m) (fun prefix ->
          declared (prefix ^ "." ^ name))
  | Papply _ | Pextra_ty _ -> None

(* The qualified name of a project top-level value, if the path names one.
   Local variables are idents pass 1 never registered, so they fall out. *)
let project_value (ctx : ctx) (u : unit_info) (p : Path.t) : string option =
  match p with
  | Pident id -> Ident.Tbl.find_opt u.u_vals id
  | Pdot (m, name) ->
      Option.bind (module_prefix ctx u m) (fun prefix ->
          let q = prefix ^ "." ^ name in
          if Hashtbl.mem ctx.def_names q then Some q else None)
  | Papply _ | Pextra_ty _ -> None

let is_predef (p : Path.t) : bool =
  match p with Pident id -> Ident.is_predef id | _ -> false

(* A type from outside the project, named the way one would write it:
   "Stdlib.Format.formatter" -> "Format.formatter", "Lib__Mod.t" -> "Lib.Mod.t". *)
let outside_name (p : Path.t) : string =
  let name = String.concat "." (split_double_underscore (Path.name p)) in
  let stdlib = "Stdlib." in
  if String.starts_with ~prefix:stdlib name then
    String.sub name (String.length stdlib)
      (String.length name - String.length stdlib)
  else name

(* "Scene.Sub.t" as written from inside [from]: the enclosing modules the
   two have in common are dropped. *)
let relative ~(from : string list) (key : string) : string =
  let rec drop parts from =
    match (parts, from) with
    | p :: (_ :: _ as parts'), f :: from' when p = f -> drop parts' from'
    | _ -> parts
  in
  String.concat "." (drop (String.split_on_char '.' key) from)

(* ---------- printing types ---------- *)

(* Printtyp prints paths as the type checker stored them ([Vector__Scene.shape],
   or a bare [shape] whose meaning depends on the file). To choose the names
   ourselves we print a copy of the type in which every named type is
   replaced by an ident carrying the name we want. Type variables are shared
   with the original, so they keep their names; object and polymorphic
   variant types are left as they are. *)
let rec rename (name_of : Path.t -> string option) (ty : Types.type_expr) :
    Types.type_expr =
  let rename = rename name_of in
  match Types.get_desc ty with
  | Tconstr (p, args, _) ->
      let p =
        match name_of p with
        | Some name -> Path.Pident (Ident.create_persistent name)
        | None -> p
      in
      Btype.newgenty (Tconstr (p, List.map rename args, ref Types.Mnil))
  | Tarrow (label, a, b, c) -> Btype.newgenty (Tarrow (label, rename a, rename b, c))
  | Ttuple l -> Btype.newgenty (Ttuple (List.map rename l))
  | Tpoly (t, vars) -> Btype.newgenty (Tpoly (rename t, vars))
  | _ -> ty

(* The name a path is printed with: qualified ("Scene.shape", for keys) or
   as written from [from] ("shape", for labels). Built-in types keep their
   own printing. *)
let name_of (ctx : ctx) (u : unit_info) ~(qualified : bool) ~(from : string list)
    (p : Path.t) : string option =
  if is_predef p then None
  else
    match project_type ctx u ~from p with
    | Some key -> Some (if qualified then key else relative ~from key)
    | None -> Some (outside_name p)

(* Run a Format printer onto one line. *)
let to_line (pp : Format.formatter -> 'a -> unit) (x : 'a) : string =
  let buffer = Buffer.create 80 in
  let ppf = Format.formatter_of_buffer buffer in
  Format.pp_set_margin ppf 1_000_000;
  pp ppf x;
  Format.pp_print_flush ppf ();
  one_line (Buffer.contents buffer)

let print_type (ctx : ctx) (u : unit_info) ~(qualified : bool) ~(from : string list)
    (ty : Types.type_expr) : string =
  (* [reset] forgets earlier idents; [type_scheme] restarts variables at 'a. *)
  Printtyp.reset ();
  to_line Printtyp.type_scheme (rename (name_of ctx u ~qualified ~from) ty)

(* The right-hand side of a type declaration, [None] if it has none. *)
let decl_rhs (ctx : ctx) (u : unit_info) ~(from : string list)
    (d : type_declaration) : string option =
  let ty = rename (name_of ctx u ~qualified:false ~from) in
  let label (l : Types.label_declaration) = { l with ld_type = ty l.ld_type } in
  let ctor (c : Types.constructor_declaration) =
    {
      c with
      cd_args =
        (match c.cd_args with
        | Cstr_tuple l -> Cstr_tuple (List.map ty l)
        | Cstr_record l -> Cstr_record (List.map label l));
      cd_res = Option.map ty c.cd_res;
    }
  in
  let decl = d.typ_type in
  let decl =
    {
      decl with
      type_manifest = Option.map ty decl.type_manifest;
      type_kind =
        (match decl.type_kind with
        | Type_record (l, r) -> Type_record (List.map label l, r)
        | Type_variant (l, r) -> Type_variant (List.map ctor l, r)
        | (Type_abstract _ | Type_open) as k -> k);
    }
  in
  Printtyp.reset ();
  (* Prints "type shape = Circle of ... | ...": keep what follows the "=". *)
  let printed = to_line (Printtyp.type_declaration d.typ_id) decl in
  let sep = " = " in
  let rec find i =
    if i + String.length sep > String.length printed then None
    else if String.sub printed i (String.length sep) = sep then
      Some (i + String.length sep)
    else find (i + 1)
  in
  find 0
  |> Option.map (fun start ->
         let rhs = String.sub printed start (String.length printed - start) in
         if String.length rhs > 200 then String.sub rhs 0 200 ^ " ..." else rhs)

(* ---------- handles, docs, visibility ---------- *)

let handle (u : unit_info) (mpath : string list) (name : string) (loc : Location.t)
    : G.handle =
  {
    file = u.u_file;
    path = String.concat "/" (mpath @ [ name ]);
    line = loc.loc_start.pos_lnum;
    end_line = loc.loc_end.pos_lnum;
  }

(* A doc comment reaches the typed tree as [@ocaml.doc "..."]. *)
let doc_of (attrs : attributes) : string option =
  List.find_map
    (fun (a : Parsetree.attribute) ->
      match (a.attr_name.txt, a.attr_payload) with
      | ( ("ocaml.doc" | "doc"),
          PStr
            [
              {
                pstr_desc =
                  Pstr_eval
                    ({ pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ }, _);
                _;
              };
            ] ) ->
          Some (String.trim s)
      | _ -> None)
    attrs

(* Doc comments are often written in the .mli only: look there too. *)
let sig_doc (vis : vis) ~(is_type : bool) (name : string) : string option =
  match vis with
  | All | Nothing -> None
  | Only sg ->
      List.find_map
        (function
          | Types.Sig_value (id, vd, _) when (not is_type) && Ident.name id = name ->
              doc_of vd.val_attributes
          | Types.Sig_type (id, decl, _, _) when is_type && Ident.name id = name ->
              doc_of decl.type_attributes
          | _ -> None)
        sg

let first_some (a : 'a option) (b : 'a option) : 'a option =
  match a with Some _ -> a | None -> b

let value_exported (vis : vis) (name : string) : bool =
  match vis with
  | All -> true
  | Nothing -> false
  | Only sg ->
      List.exists
        (function
          | Types.Sig_value (id, _, _) -> Ident.name id = name | _ -> false)
        sg

(* (exported, abstract) *)
let type_visibility (vis : vis) (name : string) : bool * bool =
  match vis with
  | All -> (true, false)
  | Nothing -> (false, false)
  | Only sg -> (
      let find = function
        | Types.Sig_type (id, decl, _, _) when Ident.name id = name -> Some decl
        | _ -> None
      in
      match List.find_map find sg with
      | None -> (false, false)
      | Some decl -> (
          match (decl.type_kind, decl.type_manifest) with
          | Type_abstract _, None -> (true, true)
          | _ -> (true, false)))

(* The items of a module type, when it is written out or is a module type
   declared earlier in the same file. One from elsewhere cannot be looked
   into without the compiler's environment. *)
let signature_of (u : unit_info) (mty : Types.module_type) : Types.signature option =
  match mty with
  | Mty_signature sg -> Some sg
  | Mty_ident (Pident id) -> Ident.Tbl.find_opt u.u_mtys id
  | _ -> None

(* What is visible of submodule [name]. If its signature cannot be looked
   into, treat it as fully exported. *)
let sub_vis (u : unit_info) (vis : vis) (name : string) : vis =
  match vis with
  | All -> All
  | Nothing -> Nothing
  | Only sg -> (
      let find = function
        | Types.Sig_module (id, _, md, _, _) when Ident.name id = name ->
            Some md.md_type
        | _ -> None
      in
      match List.find_map find sg with
      | None -> Nothing
      | Some mty -> (
          match signature_of u mty with Some sg -> Only sg | None -> All))

(* [(M : S)] and [module M : S = ...]: peel the constraints off, narrowing
   what is visible by each signature. The outer restriction wins. *)
let rec strip_constraint (u : unit_info) (me : module_expr) (vis : vis) :
    module_expr * vis =
  match me.mod_desc with
  | Tmod_constraint (inner, mty, _, _) ->
      let vis =
        match (vis, signature_of u mty) with All, Some sg -> Only sg | _ -> vis
      in
      strip_constraint u inner vis
  | _ -> (me, vis)

(* ---------- pass 1: collect ---------- *)

let rec collect (ctx : ctx) (u : unit_info) (scope : scope)
    (items : structure_item list) : unit =
  let push item = u.u_items <- item :: u.u_items in
  let declare_value id name =
    let q = qualify scope.mpath name in
    Ident.Tbl.replace u.u_vals id q;
    Hashtbl.replace ctx.def_names q ()
  in
  List.iter
    (fun (si : structure_item) ->
      match si.str_desc with
      | Tstr_type (_, decls) ->
          List.iter
            (fun (d : type_declaration) ->
              let key = qualify scope.mpath d.typ_name.txt in
              Ident.Tbl.replace u.u_types d.typ_id key;
              Hashtbl.replace ctx.type_decls key (u, scope, d);
              push (Type_item (scope, d)))
            decls
      | Tstr_value (_, bindings) ->
          let several = List.length bindings > 1 in
          List.iter
            (fun (vb : value_binding) ->
              List.iter
                (fun (id, (name : string Location.loc), _, _) ->
                  declare_value id name.txt)
                (pat_bound_idents_full vb.vb_pat);
              let loc = if several then vb.vb_loc else si.str_loc in
              push (Value_item (scope, vb, loc)))
            bindings
      | Tstr_primitive vd ->
          declare_value vd.val_id vd.val_name.txt;
          push (Prim_item (scope, vd))
      | Tstr_modtype { mtd_id; mtd_type = Some { mty_type = Mty_signature sg; _ }; _ } ->
          Ident.Tbl.replace u.u_mtys mtd_id sg
      | Tstr_module mb -> collect_module ctx u scope mb
      | Tstr_recmodule mbs -> List.iter (collect_module ctx u scope) mbs
      | Tstr_include { incl_mod; incl_type; _ } -> (
          (* [include struct ... end] is walked in place. The included names
             get fresh idents in the enclosing module ([incl_type]): map
             those to the same qualified names. Other includes are skipped. *)
          match strip_constraint u incl_mod scope.vis with
          | { mod_desc = Tmod_structure s; _ }, _ ->
              collect ctx u scope s.str_items;
              List.iter
                (function
                  | Types.Sig_value (id, _, _) ->
                      let q = qualify scope.mpath (Ident.name id) in
                      if Hashtbl.mem ctx.def_names q then
                        Ident.Tbl.replace u.u_vals id q
                  | Sig_type (id, _, _, _) ->
                      let q = qualify scope.mpath (Ident.name id) in
                      if Hashtbl.mem ctx.type_decls q then
                        Ident.Tbl.replace u.u_types id q
                  | Sig_module (id, _, _, _, _) ->
                      Ident.Tbl.replace u.u_mods id
                        (qualify scope.mpath (Ident.name id))
                  | _ -> ())
                incl_type
          | _ -> ())
      | _ -> ())
    items

and collect_module (ctx : ctx) (u : unit_info) (scope : scope)
    (mb : module_binding) : unit =
  match (mb.mb_id, mb.mb_name.txt) with
  | Some id, Some name -> (
      let prefix = qualify scope.mpath name in
      let me, vis = strip_constraint u mb.mb_expr (sub_vis u scope.vis name) in
      match me.mod_desc with
      | Tmod_structure s ->
          Ident.Tbl.replace u.u_mods id prefix;
          collect ctx u { mpath = scope.mpath @ [ name ]; vis } s.str_items
      | Tmod_ident (p, _) -> (
          (* [module S = Scene]: S is another name for a project module *)
          match module_prefix ctx u p with
          | Some target ->
              Ident.Tbl.replace u.u_mods id target;
              Hashtbl.replace ctx.mod_aliases prefix target
          | None -> ())
      | _ -> () (* functors, applications, first-class modules: not followed *))
  | _ -> () (* module _ = ... *)

(* ---------- nodes ---------- *)

let add_node (ctx : ctx) (n : G.node) : unit =
  Hashtbl.replace ctx.seen n.key ();
  ctx.nodes <- n :: ctx.nodes

(* A node that is not a declaration of the project. *)
let plain_node ~key ~label ~kind ~parts : G.node =
  {
    key; label; m_path = []; kind; ctors = []; n_ty = None; n_doc = None;
    n_err = false; parts; abstract = false; exported = true; at = None;
  }

let ctors_of (d : type_declaration) : string list =
  match (d.typ_kind, d.typ_manifest) with
  | Ttype_variant cds, _ -> List.map (fun (c : constructor_declaration) -> c.cd_name.txt) cds
  | Ttype_record lds, _ -> List.map (fun (l : label_declaration) -> l.ld_name.txt) lds
  | _, Some { ctyp_desc = Ttyp_variant (fields, _, _); _ } ->
      (* type t = [ `A | `B ] *)
      List.filter_map
        (fun (f : row_field) ->
          match f.rf_desc with Ttag (tag, _, _) -> Some tag.txt | Tinherit _ -> None)
        fields
  | _ -> []

(* Emit the Alias node of a declared type, the first time it is reached:
   at its declaration, or at an earlier reference from another file. *)
let touch_alias (ctx : ctx) (key : string) : unit =
  if not (Hashtbl.mem ctx.seen key) then begin
    let u, scope, d = Hashtbl.find ctx.type_decls key in
    let name = d.typ_name.txt in
    let exported, abstract = type_visibility scope.vis name in
    add_node ctx
      {
        key; label = name; m_path = scope.mpath; kind = Alias;
        ctors = ctors_of d;
        n_ty = decl_rhs ctx u ~from:scope.mpath d;
        n_doc =
          first_some (doc_of d.typ_attributes) (sig_doc scope.vis ~is_type:true name);
        n_err = false; parts = []; abstract; exported;
        at = Some (handle u scope.mpath name d.typ_loc);
      }
  end

let builtin (ctx : ctx) (name : string) : string =
  if not (Hashtbl.mem ctx.seen name) then
    add_node ctx (plain_node ~key:name ~label:name ~kind:Builtin ~parts:[]);
  name

let ghost (ctx : ctx) (name : string) : string =
  if not (Hashtbl.mem ctx.seen name) then
    add_node ctx (plain_node ~key:name ~label:name ~kind:Ghost ~parts:[]);
  name

(* Keys of the project and built-in types a type mentions, in order,
   emitting their nodes on the way. *)
let rec mentioned (ctx : ctx) (u : unit_info) ~(from : string list)
    (ty : Types.type_expr) : string list =
  let all l = List.concat_map (mentioned ctx u ~from) l in
  match Types.get_desc ty with
  | Tconstr (p, args, _) ->
      let here =
        if is_predef p then if args = [] then [ builtin ctx (Path.name p) ] else []
        else
          match project_type ctx u ~from p with
          | Some key -> touch_alias ctx key; [ key ]
          | None -> []
      in
      here @ all args
  | Tarrow (_, a, b, _) -> all [ a; b ]
  | Ttuple l -> all l
  | Tpoly (t, _) -> mentioned ctx u ~from t
  | _ -> []

let is_variable (ty : Types.type_expr) : bool =
  match Types.get_desc ty with Tvar _ | Tunivar _ -> true | _ -> false

(* The node a type expression stands for.
   - a project type (bare, or applied to type variables only): its Alias
   - a built-in without arguments (int, string, exn, ...): a Builtin
   - an outside type without arguments (Buffer.t): a Ghost
   - anything else (tuple, arrow, variable, [shape list], [int tree],
     [(shape, string) result]): a Derived node keyed by its printed form. *)
let rec node_of_type (ctx : ctx) (u : unit_info) ~(from : string list)
    (ty : Types.type_expr) : string =
  match Types.get_desc ty with
  | Tpoly (t, _) -> node_of_type ctx u ~from t
  | Tconstr (p, args, _) -> (
      match project_type ctx u ~from p with
      | Some key when List.for_all is_variable args -> touch_alias ctx key; key
      | Some _ -> derived ctx u ~from ty
      | None ->
          if args <> [] then derived ctx u ~from ty
          else if is_predef p then builtin ctx (Path.name p)
          else ghost ctx (outside_name p))
  | _ -> derived ctx u ~from ty

and derived (ctx : ctx) (u : unit_info) ~(from : string list)
    (ty : Types.type_expr) : string =
  let key = print_type ctx u ~qualified:true ~from ty in
  if not (Hashtbl.mem ctx.seen key) then begin
    let parts = dedup (mentioned ctx u ~from ty) in
    let label = print_type ctx u ~qualified:false ~from ty in
    add_node ctx (plain_node ~key ~label ~kind:Derived ~parts)
  end;
  key

(* The source of a function of several arguments: one Product node per
   distinct list of argument nodes. [labels] are the arguments as written. *)
let product (ctx : ctx) (arg_keys : string list) ~(labels : string list) : string =
  let key = "(" ^ String.concat ", " arg_keys ^ ")" in
  if not (Hashtbl.mem ctx.seen key) then begin
    let label = "(" ^ String.concat ", " labels ^ ")" in
    add_node ctx (plain_node ~key ~label ~kind:Product ~parts:arg_keys)
  end;
  key

(* ---------- expressions: mentions, holes, tests ---------- *)

(* Qualified names of the project's top-level values an expression refers to. *)
let mentions (ctx : ctx) (u : unit_info) (e : expression) : string list =
  let found = ref [] in
  let expr (it : Tast_iterator.iterator) (e : expression) =
    (match e.exp_desc with
    | Texp_ident (p, _, _) -> (
        match project_value ctx u p with
        | Some name -> found := name :: !found
        | None -> ())
    | _ -> ());
    Tast_iterator.default_iterator.expr it e
  in
  let it = { Tast_iterator.default_iterator with expr } in
  it.expr it e;
  dedup (List.rev !found)

let is_unit_value (e : expression) : bool =
  match e.exp_desc with
  | Texp_construct (_, { cstr_name = "()"; _ }, []) -> true
  | _ -> false

(* Is the body of a definition a placeholder? The body is what is left
   after the parameters: [fun x y -> BODY] or [function _ -> BODY]. *)
let rec is_hole (e : expression) : bool =
  match e.exp_desc with
  | Texp_function (_, Tfunction_body body) -> is_hole body
  | Texp_function (_, Tfunction_cases { cases = [ c ]; _ }) -> is_hole c.c_rhs
  | Texp_assert ({ exp_desc = Texp_construct (_, { cstr_name = "false"; _ }, []); _ }, _)
    ->
      true
  | Texp_apply ({ exp_desc = Texp_ident (p, _, _); _ }, [ (_, Some arg) ]) -> (
      match (outside_name p, arg.exp_desc) with
      | "failwith", Texp_constant (Const_string (s, _, _)) ->
          let s = String.lowercase_ascii s in
          contains ~sub:"todo" s || contains ~sub:"not implemented" s
      | "Obj.magic", _ -> is_unit_value arg
      | _ -> false)
  | _ -> false

(* ppx_inline_test turns [let%test "name" = e] into

     let () = Ppx_inline_test_lib.test ~config ~descr:(lazy "name") ~tags
                ~filename ~line_number ~start_pos ~end_pos (fun () -> e)

   ([test_unit] for [let%test_unit]; older versions pass [~descr] as a
   plain string, or go through [Ppx_inline_test_lib.Runtime]). Returns the
   description and the closure. *)
let inline_test (e : expression) : (string * expression) option =
  let rec string_of (e : expression) =
    match e.exp_desc with
    | Texp_constant (Const_string (s, _, _)) -> Some s
    | Texp_lazy e -> string_of e
    | _ -> None
  in
  match e.exp_desc with
  | Texp_apply ({ exp_desc = Texp_ident (p, _, _); _ }, args)
    when String.starts_with ~prefix:"Ppx_inline_test_lib" (Path.name p)
         && List.mem (Path.last p) [ "test"; "test_unit" ] ->
      let descr =
        List.find_map
          (function
            | Asttypes.Labelled "descr", Some d -> string_of d | _ -> None)
          args
      in
      let closure =
        List.find_map
          (function Asttypes.Nolabel, Some c -> Some c | _ -> None)
          (List.rev args)
      in
      Option.map (fun c -> (Option.value descr ~default:"", c)) closure
  | _ -> None

let status (ctx : ctx) (file : string) (line : int) : G.test_status =
  match ctx.failing with
  | None -> Unknown
  | Some l -> if List.mem (file, line) l then Fail else Pass

let add_test (ctx : ctx) (u : unit_info) ~(mpath : string list) ~(name : string)
    ~(path_name : string) ~(loc : Location.t) (body : expression) : unit =
  let t_at = handle u mpath path_name loc in
  let test : G.test =
    { t_name = name; t_status = status ctx u.u_file t_at.line; t_refs = []; t_at }
  in
  ctx.tests <- (test, mentions ctx u body) :: ctx.tests

(* Record every inline test in an expression. They sit at the top of a
   [let () = ...], or deeper inside a [let%test_module]. *)
let find_inline_tests (ctx : ctx) (u : unit_info) ~(mpath : string list)
    (e : expression) : unit =
  let expr (it : Tast_iterator.iterator) (e : expression) =
    match inline_test e with
    | Some (descr, closure) ->
        let name =
          if descr = "" then Printf.sprintf "line %d" e.exp_loc.loc_start.pos_lnum
          else descr
        in
        add_test ctx u ~mpath ~name ~path_name:name ~loc:e.exp_loc closure
    | None -> Tast_iterator.default_iterator.expr it e
  in
  let it = { Tast_iterator.default_iterator with expr } in
  it.expr it e

(* ---------- pass 2: emit ---------- *)

(* [a -> ?b:c -> d] as ([a; c], d). An optional argument is stored as
   [c option]: unwrap it. *)
let rec flatten_arrow (ty : Types.type_expr) :
    Types.type_expr list * Types.type_expr =
  match Types.get_desc ty with
  | Tpoly (t, _) -> flatten_arrow t
  | Tarrow (label, arg, rest, _) ->
      let arg =
        match (label, Types.get_desc arg) with
        | Optional _, Tconstr (p, [ t ], _) when Path.same p Predef.path_option -> t
        | _ -> arg
      in
      let args, result = flatten_arrow rest in
      (arg :: args, result)
  | _ -> ([], ty)

let is_unit_type (ty : Types.type_expr) : bool =
  match Types.get_desc ty with
  | Tconstr (p, [], _) -> Path.same p Predef.path_unit
  | _ -> false

(* One top-level name: a test ([let test_xxx () = ...]), an edge (arrow
   type) or a value. [body] is [None] for an external. *)
let define (ctx : ctx) (u : unit_info) (scope : scope) ~(name : string)
    ~(ty : Types.type_expr) ~(loc : Location.t) ~(attrs : attributes)
    ~(body : expression option) : unit =
  let from = scope.mpath in
  let qname = qualify from name in
  let at = handle u from name loc in
  let print = print_type ctx u ~qualified:false ~from in
  (* [let f : 'a. ...] is stored under a quantifier: look through it. *)
  let ty = match Types.get_desc ty with Tpoly (t, _) -> t | _ -> ty in
  let prefix = "test_" in
  match (flatten_arrow ty, body) with
  | ([ arg ], _), Some body
    when String.starts_with ~prefix name
         && String.length name > String.length prefix
         && is_unit_type arg ->
      let short =
        String.sub name (String.length prefix)
          (String.length name - String.length prefix)
      in
      add_test ctx u ~mpath:from ~name:short ~path_name:name ~loc body
  | ([], _), _ ->
      let value : G.value =
        {
          v_name = qname; v_label = name; v_m_path = from;
          v_key = node_of_type ctx u ~from ty;
          v_ty = print ty; v_err = false; v_at = at;
        }
      in
      let uses = match body with Some e -> mentions ctx u e | None -> [] in
      ctx.defs <- Value (value, uses) :: ctx.defs
  | (args, result), _ ->
      let arg_keys = List.map (node_of_type ctx u ~from) args in
      let src =
        match arg_keys with
        | [ key ] -> key
        | keys -> product ctx keys ~labels:(List.map print args)
      in
      let dst = node_of_type ctx u ~from result in
      let edge : G.edge =
        {
          e_name = qname; e_label = name; e_m_path = from; e_ty = print ty;
          src; dst;
          e_doc = first_some (doc_of attrs) (sig_doc scope.vis ~is_type:false name);
          e_err = false;
          e_hole = (match body with Some e -> is_hole e | None -> false);
          main = false; e_deps = [];
          e_exported = value_exported scope.vis name;
          tests = []; e_at = at;
        }
      in
      let uses = match body with Some e -> mentions ctx u e | None -> [] in
      ctx.defs <- Edge (edge, uses) :: ctx.defs

let emit (ctx : ctx) (u : unit_info) (item : item) : unit =
  match item with
  | Type_item (scope, d) -> touch_alias ctx (qualify scope.mpath d.typ_name.txt)
  | Prim_item (scope, vd) ->
      define ctx u scope ~name:vd.val_name.txt ~ty:vd.val_val.val_type
        ~loc:vd.val_loc ~attrs:vd.val_attributes ~body:None
  | Value_item (scope, vb, loc) -> (
      match pat_bound_idents_full vb.vb_pat with
      | [] ->
          (* [let () = ...] or [let _ = ...]: only of interest for its tests *)
          find_inline_tests ctx u ~mpath:scope.mpath vb.vb_expr
      | bound ->
          List.iter
            (fun (_, (name : string Location.loc), ty, _) ->
              define ctx u scope ~name:name.txt ~ty ~loc ~attrs:vb.vb_attributes
                ~body:(Some vb.vb_expr))
            bound)

(* Fill in what needs the whole project: deps, tests per edge, [main]. *)
let assemble (ctx : ctx) : G.t =
  let def_name = function Edge (e, _) -> e.e_name | Value (v, _) -> v.v_name in
  let uses_of = function Edge (_, uses) | Value (_, uses) -> uses in
  (* A name bound twice ([let x = 1 let x = x + 1]): the last one stands.
     [ctx.defs] is reversed, so the last binding of a name comes first. *)
  let kept = Hashtbl.create 64 in
  let defs =
    List.fold_left
      (fun acc d ->
        if Hashtbl.mem kept (def_name d) then acc
        else (
          Hashtbl.replace kept (def_name d) (match d with Edge _ -> true | Value _ -> false);
          d :: acc))
      [] ctx.defs
  in
  let is_def name = Hashtbl.mem kept name in
  let is_edge name = Hashtbl.find_opt kept name = Some true in
  let tests =
    List.rev_map
      (fun ((t : G.test), uses) -> { t with t_refs = List.filter is_edge uses })
      ctx.tests
  in
  (* For each name: how many other definitions and tests mention it, and
     which tests do. *)
  let users = Hashtbl.create 64 and tests_of = Hashtbl.create 64 in
  let count name =
    Hashtbl.replace users name (1 + Option.value (Hashtbl.find_opt users name) ~default:0)
  in
  List.iter
    (fun d -> List.iter (fun n -> if n <> def_name d then count n) (uses_of d))
    defs;
  List.iter
    (fun (t : G.test) ->
      List.iter
        (fun n ->
          count n;
          Hashtbl.replace tests_of n
            (t :: Option.value (Hashtbl.find_opt tests_of n) ~default:[]))
        t.t_refs)
    tests;
  let edges =
    List.filter_map
      (function
        | Value _ -> None
        | Edge (e, uses) ->
            Some
              {
                e with
                e_deps = List.filter (fun n -> n <> e.e_name && is_def n) uses;
                main = Option.value (Hashtbl.find_opt users e.e_name) ~default:0 >= 2;
                tests =
                  List.rev (Option.value (Hashtbl.find_opt tests_of e.e_name) ~default:[]);
              })
      defs
  in
  {
    nodes = List.rev ctx.nodes;
    edges;
    values = List.filter_map (function Value (v, _) -> Some v | Edge _ -> None) defs;
    loose_tests = List.filter (fun (t : G.test) -> t.t_refs = []) tests;
  }

(* ---------- reading the build directory ---------- *)

(* dune preprocesses lib/scene.ml into lib/scene.pp.ml and compiles that. *)
let strip_pp (file : string) : string =
  let swap suffix by =
    if Filename.check_suffix file suffix then Some (Filename.chop_suffix file suffix ^ by)
    else None
  in
  match swap ".pp.ml" ".ml" with
  | Some f -> f
  | None -> Option.value (swap ".pp.mli" ".mli") ~default:file

(* The source file of an implementation, relative to the root: the file the
   compiler was given, or else the file the first located item points to
   (a Reason source, say). [None] if neither is in the project, as for the
   files dune generates. *)
let source_file ~(root : string) (cmt : Cmt_format.cmt_infos) (str : structure) :
    string option =
  let from_items =
    List.find_map
      (fun (si : structure_item) ->
        let f = si.str_loc.loc_start.pos_fname in
        if f = "" || f = "_none_" then None else Some f)
      str.str_items
  in
  let in_project f =
    Filename.is_relative f
    && Sys.file_exists (root // f)
    && not (Sys.is_directory (root // f))
  in
  List.find_opt in_project
    (Option.to_list (Option.map strip_pp cmt.cmt_sourcefile) @ Option.to_list from_items)

(* dune writes an alias module per wrapped library (source: vector.ml-gen)
   holding [module Scene = Vector__Scene]. Not part of the graph, but it
   says what [Vector.Scene] means. *)
let read_alias_module (ctx : ctx) (modname : string) (str : structure) : unit =
  List.iter
    (fun (si : structure_item) ->
      match si.str_desc with
      | Tstr_module
          { mb_name = { txt = Some sub; _ }; mb_expr = { mod_desc = Tmod_ident (p, _); _ }; _ }
        ->
          Hashtbl.replace ctx.wrappers (modname, sub) (Path.name p)
      | _ -> ())
    str.str_items

(* The signature of the .cmti next to a .cmt, when the module has an .mli. *)
let interface_of (cmt_path : string) : vis =
  let cmti = cmt_path ^ "i" in
  if not (Sys.file_exists cmti) then All
  else
    match (Cmt_format.read_cmt cmti).cmt_annots with
    | Interface sg -> Only sg.sig_type
    | _ -> All
    | exception _ -> All

(* Names shown for the units: "Scene" for Vector__Scene. When two units
   would get the same name, fall back to "Vector.Scene", then to the short
   name tagged with its directory. *)
let name_units (found : (string * string * structure * vis) list) :
    (string list * (string * string * structure * vis)) list =
  let short (modname, _, _, _) =
    match List.rev (split_double_underscore modname) with
    | last :: _ -> [ last ]
    | [] -> [ modname ]
  in
  let dotted (modname, _, _, _) = split_double_underscore modname in
  (* how many units each candidate name would be given to *)
  let tally name_of =
    let t = Hashtbl.create 16 in
    List.iter
      (fun x ->
        Hashtbl.replace t (name_of x)
          (1 + Option.value (Hashtbl.find_opt t (name_of x)) ~default:0))
      found;
    fun x -> Hashtbl.find t (name_of x) = 1
  in
  let unique_short = tally short and unique_dotted = tally dotted in
  List.map
    (fun ((_, file, _, _) as x) ->
      let mpath =
        if unique_short x then short x
        else if unique_dotted x then dotted x
        else [ String.concat "" (short x) ^ "@" ^ Filename.dirname file ]
      in
      (mpath, x))
    found

let extract ~(root : string) ~(build_dir : string)
    ~(failing : (string * int) list option) : (G.t, string) result =
  let ctx =
    {
      failing;
      units = Hashtbl.create 16; wrappers = Hashtbl.create 16;
      mod_aliases = Hashtbl.create 16; type_decls = Hashtbl.create 64;
      def_names = Hashtbl.create 64; seen = Hashtbl.create 64;
      nodes = []; defs = []; tests = [];
    }
  in
  let dir = root // build_dir // "default" in
  let cmts = List.filter (fun f -> Filename.check_suffix f ".cmt") (find_cmts dir) in
  (* Read every implementation. A file that failed to type-check has only
     a partial tree: it is left out. *)
  let unreadable = ref 0 in
  let found =
    List.filter_map
      (fun path ->
        match Cmt_format.read_cmt path with
        | exception _ -> incr unreadable; None
        | cmt -> (
            match cmt.cmt_annots with
            | Implementation str -> (
                let generated =
                  match cmt.cmt_sourcefile with
                  | Some f -> Filename.check_suffix f "-gen"
                  | None -> false
                in
                if generated then (read_alias_module ctx cmt.cmt_modname str; None)
                else
                  match source_file ~root cmt str with
                  | Some file -> Some (cmt.cmt_modname, file, str, interface_of path)
                  | None -> None)
            | _ -> incr unreadable; None))
      cmts
  in
  (* The same source can be compiled more than once: keep the first. *)
  let sources = Hashtbl.create 16 in
  let found =
    List.filter
      (fun (_, file, _, _) ->
        let first = not (Hashtbl.mem sources file) in
        Hashtbl.replace sources file ();
        first)
      found
  in
  if found = [] then
    Error
      (if cmts = [] then
         Printf.sprintf "no .cmt files under %s (build the project with: dune build --root . --build-dir %s @check)"
           dir build_dir
       else
         Printf.sprintf "no module under %s could be read (%d of %d .cmt files unreadable or not type-checked)"
           dir !unreadable (List.length cmts))
  else begin
    let units =
      name_units found
      |> List.map (fun (mpath, (modname, file, str, vis)) ->
             {
               u_modname = modname; u_mpath = mpath; u_file = file; u_str = str;
               u_vis = vis; u_types = Ident.Tbl.create 16;
               u_mods = Ident.Tbl.create 8; u_vals = Ident.Tbl.create 32;
               u_mtys = Ident.Tbl.create 8;
               u_items = [];
             })
      |> List.sort (fun a b -> compare a.u_file b.u_file)
    in
    List.iter
      (fun u ->
        if not (Hashtbl.mem ctx.units u.u_modname) then
          Hashtbl.replace ctx.units u.u_modname u)
      units;
    List.iter
      (fun u -> collect ctx u { mpath = u.u_mpath; vis = u.u_vis } u.u_str.str_items)
      units;
    List.iter (fun u -> List.iter (emit ctx u) (List.rev u.u_items)) units;
    Ok (assemble ctx)
  end
