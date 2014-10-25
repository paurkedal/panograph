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
  open Unprime_list
  open Unprime_option
}}

{client{
  open Panograph_intf
  open Panograph_simple
  open Panograph_mapped
  open Panograph_collection

  let (>|=) = Lwt.(>|=)

  module Int_order = struct
    type t = int
    let compare = compare
  end

  module Collection_ul_container = struct
    type shape = unit
    type ui = Html5_types.flow5 Html5.elt
    type t = ui
    type item_ui = Html5_types.flow5 Html5.elt * controls_ui
    type item = Html5_types.ul_content Html5.elt
    type aux_ui = Html5_types.flow5 Html5.elt * controls_ui
    let ui w = w
    let create ?(aux : aux_ui option) () =
      let open Html5 in
      D.ul
	begin match aux with
	| None -> []
	| Some (aux_ui, controls_ui) ->
	  [D.li [aux_ui; D.span ~a:[F.a_class ["controls"]] controls_ui]]
	end
    let create_item ((elt_ui, controls_ui) : item_ui) () =
      let open Html5 in
      D.li [elt_ui; D.span ~a:[F.a_class ["controls"]] controls_ui]
    let append ?before ul li = Html5.Manip.appendChild ?before ul li
    let remove ul li = Html5.Manip.removeChild ul li
  end

  module Mapped_ul_container = struct
    type shape = unit
    type ui = Html5_types.flow5 Html5.elt
    type t = ui
    type item_ui = Html5_types.flow5 Html5.elt * Html5_types.flow5 Html5.elt
    type item = Html5_types.ul_content Html5.elt
    type aux_ui = Prime.counit
    let ui w = w
    let create ?aux () = assert (aux = None); Html5.D.ul []
    let create_item (key_ui, elt_ui) () = Html5.D.li [key_ui; elt_ui]
    let append ?before ul li = Html5.Manip.appendChild ?before ul li
    let remove ul li = Html5.Manip.removeChild ul li
  end

  module Int_ul_PE =
    Collection_editor (Int_PE) (Int_SE) (Collection_ul_container)
  module Int_ul_MPE =
    Mapped_PE (Int_order) (Int_SV) (Int_PE) (Mapped_ul_container)

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
      Lwt.return Ack_ok in
    let init = [5; 7; 3; 11; 17; 13] in
    let pe = Int_ul_PE.create ~init ~on_patch shape in
    let on_mapped_patch (`Patch (k, (`Change (v, v')))) =
      send_ev (`Patch (`Change (k, - v')));
      Lwt.return Ack_ok in
    let mapped_shape =
      Int_ul_MPE.({key_sv_shape = []; elt_pe_shape; container_shape = ()}) in
    let mapped_pe = Int_ul_MPE.create ~init:(List.map (fun k -> k, -k) init)
				      ~on_patch:on_mapped_patch mapped_shape in
    let update p =
      Int_ul_PE.patch pe p;
      let p' =
	match p with
	| `Add v -> `Add (v, -v)
	| `Remove v -> `Remove v
	| `Patch (`Change (v, v')) ->
	  if v = v' then `Patch (v, None, `Change (-v, -v'))
		    else `Patch (v, Some v', `Change (-v, -v')) in
      Int_ul_MPE.patch mapped_pe p' in
    Lwt_react.E.keep (Lwt_react.E.map update ev);
    Html5.D.div [
      Int_ul_PE.ui pe;
      Int_ul_MPE.ui mapped_pe;
    ]
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
