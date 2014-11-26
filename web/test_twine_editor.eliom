(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

{shared{
  open Eliom_content
  open Panograph_lwt
  open Panograph_twine_editor
}}
{client{
  module Dep_pte = Panograph_twine_editor
}}

open Eliom_lib
open Panograph_i18n

let on_update' = function
  | `Add (lang, msg) ->
    Lwt_log.debug_f "Received add %s => %s" (Lang.to_string lang) msg
  | `Remove lang ->
    Lwt_log.debug_f "Received remove %s" (Lang.to_string lang)

let on_update = server_function Json.t<twine_editor_out> on_update'

let render () =
  let open Html5 in
  let twe_el, twe_emitter, twe_patch = twine_editor () in

  ignore {unit{
    Lwt.async (fun () -> Emitter.iter_s %on_update %twe_emitter)
  }};

  D.div [
    D.h2 [D.pcdata "Server Side"];
    twe_el
  ]
