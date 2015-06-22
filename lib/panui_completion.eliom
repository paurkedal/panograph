(* Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
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
  open Eliom_content.Html5
  open Panograph_types
  open Printf
  open Unprime
  open Unprime_option
}}

{client{ let unique_id = let c = ref (-1) in
			 fun () -> incr c; sprintf "pan-id-c%d" !c }}
{server{ let unique_id = let c = ref (-1) in
			 fun () -> incr c; sprintf "pan-id-c%d" !c }}

{shared{
  let string_completion_input
	?(value : string option)
	(fetch : (string -> string list Lwt.t) client_value)
	(commit : (string -> ack Lwt.t) client_value) =
    let id = unique_id () in
    let input_elem = D.input ~input_type:`Text ~a:[D.a_list id] () in
    let datalist_elem = D.datalist ~a:[D.a_id id] () in
    let absorb = {string -> unit{
      let input_dom = To_dom.of_input %input_elem in
      let on_input _ _ =
	lwt completions = %fetch (Js.to_string input_dom##value) in
	let options = List.map (fun v -> D.option (D.pcdata v)) completions in
	Eliom_lib.debug "Got %d options" (List.length options);
	Manip.replaceChildren %datalist_elem options;
	Lwt.return_unit in
      Lwt_js_events.(async (fun () -> inputs input_dom on_input));
      Panograph_basic_editors.outfit_interactive ?value:%value
	~to_string:ident ~of_string:ident input_dom %commit
    }} in
    D.span [input_elem; datalist_elem], absorb
}}
