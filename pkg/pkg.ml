#! /usr/bin/env ocaml

(* Copyright (C) 2016  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

#use "topfind"
#require "adpkg"
#require "topkg"

open Adpkg
open Topkg

let licenses = List.map Pkg.std_file ["COPYING.LESSER"; "COPYING"]

let build_cmd c os targets =
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  let build_dir = Conf.build_dir c in
  OS.Cmd.run @@
  Cmd.(ocamlbuild
        % "-use-ocamlfind"
        % "-plugin-tag" % "package(ocamlbuild-eliom-dev)"
        % "-build-dir" % build_dir
        %% of_list targets)

let build = Pkg.build ~cmd:build_cmd ()

let opams = [Pkg.opam_file ~lint_deps_excluding:(Some ["lib"]) "opam"]

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
  ]
