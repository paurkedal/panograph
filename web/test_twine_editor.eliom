(* Copyright (C) 2014--2022  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

[%%server
  open Eliom_client
]
[%%shared
  open Eliom_content
  open Panograph_twine_editor
  open Panograph_types
]
[%%client
  module Dep_pte = Panograph_twine_editor
]

open Eliom_lib
open Panograph_i18n
open Panograph_prereq

let stream, emit = Lwt_stream.create ()
let comet = Eliom_comet.Channel.create ~scope:`Site stream

let on_update' p =
  begin match p with
  | `Add (lang, msg) ->
    Lwt_log.debug_f "Received add %s => %s" (Lang.to_string lang) msg
  | `Remove lang ->
    Lwt_log.debug_f "Received remove %s" (Lang.to_string lang)
  end >>= fun () ->
  Lwt_unix.sleep 1.0 >>= fun () ->
  emit (Some p);
  Lwt.return (Panui_result.ok ())

let on_update = server_function [%json: twine_editor_out] on_update'

let handler () () =
  let open Html in
  let twe_el, twe_patch = twine_editor [%client ~%on_update] in

  ignore_cv [%client Lwt.async (fun () -> Lwt_stream.iter ~%twe_patch ~%comet)];

  Lwt.return [
    D.h2 [D.txt "Server Side"];
    twe_el
  ]
