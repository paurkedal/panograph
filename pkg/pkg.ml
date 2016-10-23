#! /usr/bin/env ocaml
#use "topfind"
#require "adpkg"
#require "topkg"
#require "unix"

open Adpkg
open Topkg

let licenses = List.map Pkg.std_file ["COPYING.LESSER"; "COPYING"]

let build_cmd c os targets =
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  let build_dir = Conf.build_dir c in
  let targets =
    if Conf.build_tests c then
      "web/server/panograph-test.cma" ::
      "web/server/panograph-test.cmxs" ::
      "web/client/test.js" ::
      targets
    else
      targets in
  Unix.putenv "OCAMLPATH" ".";
  OS.Cmd.run @@
  Cmd.(ocamlbuild
        % "-use-ocamlfind"
        % "-plugin-tag" % "package(ocamlbuild-eliom-dev)"
        % "-build-dir" % build_dir
        %% of_list targets)

let build = Pkg.build ~cmd:build_cmd ()

let opams = [Pkg.opam_file ~lint_deps_excluding:(Some ["lib"; "oUnit"]) "opam"]

let map_client_server_dir tags dir =
  if Tags.mem "server" tags then Fpath.append dir "server" else
  if Tags.mem "client" tags then Fpath.append dir "client" else
  dir

let () = Pkg.describe ~build ~licenses ~opams "panograph" @@ fun c ->
  Modules.of_file "lib/panograph.oclib" >>= fun modules ->
  Modules.mllib
    ~filter:Filter.(tagged "shared") modules
    "lib/panograph.mllib" >>= fun shared_mllib ->
  Modules.mllib
    ~filter:Filter.(tagged "server") modules
    ~strip_dir:"lib" ~dst_dir:"server/"
    "lib/server/panograph-server.mllib" >>= fun server_mllib ->
  Modules.mllib
    ~filter:Filter.(tagged "client") modules
    ~strip_dir:"lib" ~dst_dir:"client/"
    "lib/client/panograph-client.mllib" >>= fun client_mllib ->
  Modules.save
    ~filter:Filter.(not (tagged "internal"))
    ~map_dir:map_client_server_dir modules "doc/api.odocl" >>= fun () ->
  Modules.save
    ~map_dir:map_client_server_dir modules "doc/dev.odocl" >>= fun () ->
  Ok [
    shared_mllib; server_mllib; client_mllib;
    Pkg.share ~dst:"static/css/" "static/css/panograph.css";
    Pkg.test "tests/testsuite";
  ]
