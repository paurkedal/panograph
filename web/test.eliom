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
  open Consimila_collection

  module Ul_container = struct
    type shape = unit
    type ui = Html5_types.flow5 Html5.elt
    type item_ui = Html5_types.ul_content Html5.elt
    type elt_ui = Html5_types.flow5 Html5.elt
    let create () = Html5.D.ul []
    let create_item _ elt_ui = Html5.D.li [elt_ui]
    let append ul li = Html5.Manip.appendChild ul li
    let insert ul li_succ li = Html5.Manip.appendChild ul ~before:li_succ li
    let remove ul li = Html5.Manip.removeChild ul li
  end

  module Int_ul_PE = Collection_editor (Ul_container) (Int_PE) (Int_SE)

  let test_int_editor () =
    let ev, send_ev = React.E.create () in
    let open Html5 in
    let on_patch p = Lwt_js.sleep 1.0 >> (send_ev p; Lwt.return Ack_ok) in
    let w =
      Int_PE.create ~init:19 ~on_patch
		    Simple_shape.(make ~a:[F.a_title "test"] ()) in
    Lwt_react.E.keep (Lwt_react.E.map (Int_PE.patch w) ev);
    Int_PE.ui w

  let test_float_editor () =
    let ev, send_ev = React.E.create () in
    let open Html5 in
    let on_patch p = Lwt_js.sleep 1.0 >> (send_ev p; Lwt.return Ack_ok) in
    let w = Float_PE.create ~init:0.01 ~on_patch Simple_shape.(make ()) in
    Lwt_react.E.keep (Lwt_react.E.map (Float_PE.patch w) ev);
    Float_PE.ui w

  let test_int_ul () =
    let ev, send_ev = React.E.create () in
    let elt_shape = Simple_shape.(make ()) in
    let shape = Int_ul_PE.({container_shape = ();
			    elt_shape = elt_shape; new_shape = elt_shape}) in
    let on_patch p =
      Lwt.(async (fun () -> Lwt_js.sleep 0.33 >|= fun () -> send_ev p));
      Lwt.return Ack_ok  in
    let pe = Int_ul_PE.create ~init:[5; 7; 3; 11; 17; 13] ~on_patch shape in
    Lwt_react.E.keep (Lwt_react.E.map (Int_ul_PE.patch pe) ev);
    Int_ul_PE.ui pe
}}

let main_handler () () =
  let open Html5.D in
  Lwt.return @@
    Eliom_tools.D.html
      ~title:"Panograph Test"
      ~css:[["css"; "panograph.css"]]
      (body [
	h1 [pcdata "Panograph Test"];
	Html5.C.node {{test_int_editor ()}};
	Html5.C.node {{test_float_editor ()}};
	Html5.C.node {{test_int_ul ()}};
      ])

module Main_app =
  Eliom_registration.App (struct let application_name = "test" end)
let main_service =
  Main_app.register_service ~path:[] ~get_params:Eliom_parameter.unit
			    main_handler
