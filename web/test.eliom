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
}}

{client{
  open Consimila_intf
  open Consimila_simple

  let test_int_editor () =
    let ev, send_ev = React.E.create () in
    let open Html5.D in
    let on_patch (`Set i) =
      Lwt_js.sleep 1.0 >> (send_ev i; Lwt.return Ack_ok) in
    let w =
      Int_editor.create ~init:19 ~on_patch
			Simple_shape.(make ~a:[a_title "test"] ()) in
    Lwt_react.E.keep
      (Lwt_react.E.map (fun i -> Int_editor.patch w (`Set i)) ev);
    div [Int_editor.ui w]

  let test_float_editor () =
    let ev, send_ev = React.E.create () in
    let open Html5.D in
    let on_patch (`Set i) =
      Lwt_js.sleep 1.0 >> (send_ev i; Lwt.return Ack_ok) in
    let w =
      Float_editor.create ~init:0.01 ~on_patch
			  Simple_shape.(make ()) in
    Lwt_react.E.keep
      (Lwt_react.E.map (fun i -> Float_editor.patch w (`Set i)) ev);
    div [Float_editor.ui w]
}}

let main_handler () () =
  let open Html5.D in
  Lwt.return @@
    Eliom_tools.D.html
      ~title:"Panograph Test"
      ~css:[["css"; "panograph.css"]]
      (body [
	h1 [pcdata "Panograph Test"];
	Html5.C.node {{(test_int_editor ())}};
	Html5.C.node {{(test_float_editor ())}};
      ])

module Main_app =
  Eliom_registration.App (struct let application_name = "test" end)
let main_service =
  Main_app.register_service ~path:[] ~get_params:Eliom_parameter.unit
			    main_handler
