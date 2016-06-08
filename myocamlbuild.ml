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
  rule "pkg/META -> lib/META"
    ~dep:"pkg/META" ~prod:"lib/META"
    begin fun env build ->
      sed "/^\\s*requires =/ s/\\<panograph\\>/lib/g" "pkg/META" "lib/META"
    end;
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

let () = Ocamlbuild_plugin.dispatch @@ fun hook ->
  M.dispatcher ~oasis_executables hook;
  match hook with
  | Before_options -> Options.make_links := false
  | After_rules ->
    local_rules ();
    doc_rules_for "lib";
    doc_rules_for "lib/server";
    doc_rules_for "lib/client";
    dep ["ocaml"; "ocamldep"; "package(lib.server)"] ["lib/server.otarget"];
    dep ["ocaml"; "ocamldep"; "package(lib.client)"] ["lib/client.otarget"]
  | _ -> ()
