open Ocamlbuild_plugin

module M = Ocamlbuild_eliom.Make (struct
  let client_dir = "client"
  let server_dir = "server"
  let type_dir = "type"
end)

let oasis_executables = [
  "web/client/test.byte";
]

let sed instr src dst = Cmd (S[A"sed"; A instr; P src; Sh">"; Px dst])

let doc_rules_for dir =
  rule (dir ^ "/%(libname).mllib -> doc/%(libname).odocl")
    ~dep:(Filename.concat dir "%(libname).mllib")
    ~prod:"doc/%(libname).odocl"
    (fun env build ->
      let src = Filename.concat dir (env "%(libname).mllib") in
      let dst = env "doc/%(libname).odocl" in
      sed ("s;^;" ^ dir ^ "/;") src dst)

let local_rules () =
  rule "%.mli & %.idem -> %.ml"
    ~deps:["%.mli"; "%.idem"] ~prod:"%.ml"
    begin fun env build ->
      let src = env "%.mli" and dst = env "%.ml" in
      cp src dst
    end;
  rule "%.eliomi & %.idem -> %.eliom"
    ~deps:["%.eliomi"; "%.idem"] ~prod:"%.eliom"
    begin fun env build ->
      let src = env "%.eliomi" and dst = env "%.eliom" in
      cp src dst
    end

let js_of_ocaml_version =
  let ic = Unix.open_process_in "js_of_ocaml --version" in
  let version = input_line ic in
  (match Unix.close_process_in ic with
   | Unix.WEXITED 0 -> ()
   | _ -> failwith "js_of_ocaml --version failed");
  (match String.split_on_char '.' (String.trim version) with
   | [] | [_] -> failwith "Failed to parse js_of_ocaml version."
   | v0 :: v1 :: _ -> (int_of_string v0, int_of_string v1))

let () = Ocamlbuild_plugin.dispatch @@ fun hook ->
  M.dispatcher ~oasis_executables hook;
  match hook with
  | Before_options -> Options.make_links := false
  | After_rules ->
    (match Sys.getenv "TERM" with
     | exception Not_found -> ()
     | "" | "dumb" -> ()
     | _ -> flag ["ocaml"; "compile"] (S [A"-color"; A"always"]));
    local_rules ();
    Pathname.define_context "tests" ["lib"];
    Pathname.define_context "web/type" ["lib"; "lib/server"];
    Pathname.define_context "web/server" ["lib"; "lib/server"];
    Pathname.define_context "web/client" ["lib"; "lib/client"];
    doc_rules_for "lib";
    doc_rules_for "lib/server";
    doc_rules_for "lib/client";
    dep ["ocaml"; "ocamldep"; "package(lib.server)"] ["lib/server.otarget"];
    dep ["ocaml"; "ocamldep"; "package(lib.client)"] ["lib/client.otarget"];
    flag ["ocaml"; "compile"] & S[A"-w"; A"+A-4-29-39-40-42-44-48"];
    if js_of_ocaml_version >= (3, 6) then
      flag ["js_of_ocaml"] & S[A"+js_of_ocaml-compiler/runtime.js"]
  | _ -> ()
