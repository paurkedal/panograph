(* Copyright (C) 2015--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

open Eliom_content.Html
open Panograph_prereq
open Panui_content_with_edit

[%%client
  open Lwt.Infix
  open Js_of_ocaml_lwt

  let (a_ev, emit_a) : string React.E.t * _ = React.E.create ()
  let (b_ev, emit_b) : string React.E.t * _ = React.E.create ()
  let (c_ev, emit_c) : string React.E.t * _ = React.E.create ()

  let emit_a' x = Lwt_js.sleep 0.5 >|= fun () -> emit_a x; Ok ()
  let emit_b' x = Lwt_js.sleep 0.5 >|= fun () -> emit_b x; Ok ()
  let emit_c' x = Lwt_js.sleep 0.5 >|= fun () -> emit_c x; Ok ()
]

let handler () () =
  let set_a, elem_a = span_with_input [%client emit_a'] "value a" in
  let set_b, elem_b = span_with_input [%client emit_b'] "value b" in
  let set_c, elem_c = p_with_textarea [%client emit_c'] "A paragraph." in
  ignore_cv [%client
    Lwt_react.E.keep (React.E.trace ~%set_a a_ev);
    Lwt_react.E.keep (React.E.trace ~%set_b b_ev);
    Lwt_react.E.keep (React.E.trace ~%set_c c_ev)
  ];
  Lwt.return [
    D.p [elem_a; D.txt ", "; elem_b];
    elem_c;
  ]

[%%client.start]
[@@@warning "-33"]
open Panui_content_with_edit
