(* Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
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
open Eliom_content.Html5
open Panograph_types
open Panui_content_with_edit
let (>|=) = Lwt.(>|=)
}}

{client{
  module Dep_content_with_edit = Panui_content_with_edit

  let a_ev, emit_a = React.E.create ()
  let b_ev, emit_b = React.E.create ()

  let emit_a' x = Lwt_js.sleep 0.5 >|= fun () -> emit_a x; Ack_ok
  let emit_b' x = Lwt_js.sleep 0.5 >|= fun () -> emit_b x; Ack_ok
}}

let render () =
  let set_a, elem_a = pcdata_with_edit {{emit_a'}} "value a" in
  let set_b, elem_b = pcdata_with_edit {{emit_b'}} "value b" in
  ignore {unit{
    Lwt_react.E.keep (React.E.trace %set_a a_ev);
    Lwt_react.E.keep (React.E.trace %set_b b_ev)
  }};
  D.div [elem_a; D.pcdata ", "; elem_b]
