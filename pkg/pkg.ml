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
#require "topkg"

open Topkg

let license = Pkg.std_file "COPYING.LESSER"

let build_cmd c os =
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  let build_dir = Conf.build_dir c in
  Cmd.(ocamlbuild
        % "-use-ocamlfind"
        % "-plugin-tag" % "package(ocamlbuild-eliom-dev)"
        % "-build-dir" % build_dir)

let build = Pkg.build ~cmd:build_cmd ()

let () = Pkg.describe ~build ~license "panograph" @@ fun c ->
  Ok [
    Pkg.mllib "lib/panograph.mllib";
    Pkg.mllib ~dst_dir:"server/" "lib/server/panograph-server.mllib";
    Pkg.mllib ~dst_dir:"client/" "lib/client/panograph-client.mllib";
    Pkg.share ~dst:"static/css/" "static/css/panograph.css";
  ]
