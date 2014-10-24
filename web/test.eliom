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
  open Panograph_intf
  open Panograph_simple
  open Panograph_collection

  let (>|=) = Lwt.(>|=)

  let set_error dom msg =
    dom##classList##add(Js.string "error");
    dom##title <- Js.string msg
  let clear_error dom =
    dom##classList##remove(Js.string "error");
    dom##title <- Js.string ""
  let flash_error dom msg =
    set_error dom msg;
    Lwt_js.sleep 4.0 >|= fun () ->
    clear_error dom

  let make_button f content =
    let open Html5 in
    let button = D.button ~button_type:`Button content in
    let button_dom = To_dom.of_button button in
    let on_click _ _ =
      match_lwt f () with
      | Ack_ok -> Lwt.return_unit
      | Ack_error msg -> flash_error button_dom msg in
    Lwt.async (fun () -> Lwt_js_events.clicks button_dom on_click);
    button

  module Ul_container = struct
    type shape = unit
    type ui = Html5_types.flow5 Html5.elt
    type item_ui = Html5_types.ul_content Html5.elt
    type elt_pe_ui = Html5_types.flow5 Html5.elt
    type elt_se_ui = Html5_types.flow5 Html5.elt
    let create ?add_ui ?on_add () =
      let open Html5 in
      let lis =
	match add_ui with
	| None -> []
	| Some add_ui ->
	  let contr =
	    match on_add with
	    | None -> []
	    | Some on_add -> [make_button on_add [F.pcdata "+"]] in
	  [D.li (add_ui :: contr)] in
      D.ul lis
    let create_item ~edit_ui ?on_remove _ =
      let open Html5 in
      let contr =
	match on_remove with
	| None -> []
	| Some on_remove -> [make_button on_remove [F.pcdata "-"]] in
      D.li (edit_ui :: contr)
    let append ?before ul li = Html5.Manip.appendChild ?before ul li
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
    let elt_pe_shape = Simple_shape.(make ()) in
    let shape = Int_ul_PE.({container_shape = ();
			    elt_pe_shape; elt_se_shape = elt_pe_shape}) in
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
