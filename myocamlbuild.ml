open Ocamlbuild_plugin

module M = Ocamlbuild_eliom.Make (struct
  let client_dir = "client"
  let server_dir = "server"
  let type_dir = "type"
end)

let oasis_executables = [
  "web/client/test.byte";
]

let () = mark_tag_used "tests"

let () =
  rule "pkg/META -> lib/META"
    ~dep:"pkg/META" ~prod:"lib/META"
    begin fun env build ->
      Cmd (S[A"sed"; A"s/%%NAME%%/lib/g"; P"pkg/META"; Sh">"; Px"lib/META"])
    end

let () = Ocamlbuild_plugin.dispatch @@ fun hook ->
  M.dispatcher ~oasis_executables hook;
  match hook with
  | Before_options -> Options.make_links := false
  | _ -> ()
