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
}}

{client{
let dirty_cls = Js.string "dirty"
let editing_cls = Js.string "editing"
let error_cls = Js.string "error"
}}

{shared{

let editable_string ?a (emit_value : (string -> ack Lwt.t) client_value) x =

  let pcdata = D.pcdata x in
  let span = D.span ?a [pcdata] in

  let set_value = {string -> unit{
    let pcdata_dom = To_dom.of_pcdata %pcdata in
    let input = D.input ~input_type:`Text () in
    let input_dom = To_dom.of_input input in
    let span_dom = To_dom.of_span %span in

    let mkbutton f label =
      D.button ~button_type:`Button ~a:[D.a_onclick f] [D.pcdata label] in

    let rec add_edit_button () =
      let edit_button = mkbutton on_edit "edit" in
      Dom.appendChild span_dom (To_dom.of_button edit_button)
    and on_edit _ =
      let cancel_button = mkbutton on_cancel "cancel" in
      let save_button = mkbutton on_save "save" in
      if not (Js.to_bool span_dom##classList##contains(editing_cls)) then begin
	input_dom##value <- pcdata_dom##data;
	input_dom##disabled <- Js._false;
	input_dom##classList##remove(error_cls);
	input_dom##title <- Js.string "";
	span_dom##classList##add(editing_cls);
	span_dom##innerHTML <- Js.string "";
	Dom.appendChild span_dom input_dom;
	Dom.appendChild span_dom (To_dom.of_button save_button);
	Dom.appendChild span_dom (To_dom.of_button cancel_button)
      end
    and on_cancel _ =
      if Js.to_bool span_dom##classList##contains(editing_cls) then begin
	span_dom##classList##remove(editing_cls);
	span_dom##innerHTML <- Js.string "";
	Dom.appendChild span_dom pcdata_dom;
	add_edit_button ()
      end
    and on_save ev =
      span_dom##classList##add(dirty_cls);
      input_dom##disabled <- Js._true;
      Lwt.async begin fun () ->
	match_lwt %emit_value (Js.to_string input_dom##value) with
	| Ack_ok ->
	  on_cancel ev;
	  Lwt.return_unit
	| Ack_error msg ->
	  input_dom##classList##add(error_cls);
	  input_dom##title <- Js.string msg;
	  Lwt.return_unit
      end in

    add_edit_button ();

    let set_value x =
      span_dom##classList##remove(dirty_cls);
      pcdata_dom##replaceData(0, pcdata_dom##length, Js.string x);
      input_dom##value <- Js.string x in
    set_value %x;
    set_value
  }} in

  (set_value : (string -> unit) client_value), span

}}
