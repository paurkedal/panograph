(* Copyright (C) 2015--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

[%%shared
open Eliom_content
open Eliom_content.Html5
open Panograph_types
]

[%%client
let dirty_cls = Js.string "dirty"
let editing_cls = Js.string "editing"
let error_cls = Js.string "error"
]

[%%client

let add_edit outer pcdata input_dom emit x =
  let pcdata_dom = To_dom.of_pcdata pcdata in
  let outer_dom = To_dom.of_element outer in

  let mkbutton f label =
    D.Raw.button ~a:[D.a_button_type `Button; D.a_onclick f] [D.pcdata label] in

  let rec add_edit_button () =
    let edit_button = mkbutton on_edit "edit" in
    Dom.appendChild outer_dom (To_dom.of_button edit_button)
  and on_edit _ =
    let cancel_button = mkbutton on_cancel "cancel" in
    let save_button = mkbutton on_save "save" in
    if not (Js.to_bool (outer_dom##.classList##contains editing_cls)) then begin
      input_dom##.value := pcdata_dom##.data;
      input_dom##.disabled := Js._false;
      input_dom##.classList##remove(error_cls);
      input_dom##.title := Js.string "";
      outer_dom##.classList##add(editing_cls);
      outer_dom##.innerHTML := Js.string "";
      Dom.appendChild outer_dom input_dom;
      Dom.appendChild outer_dom (To_dom.of_button save_button);
      Dom.appendChild outer_dom (To_dom.of_button cancel_button)
    end
  and on_cancel _ =
    if Js.to_bool (outer_dom##.classList##contains editing_cls) then begin
      outer_dom##.classList##remove(editing_cls);
      outer_dom##.innerHTML := Js.string "";
      Dom.appendChild outer_dom pcdata_dom;
      add_edit_button ()
    end
  and on_save ev =
    outer_dom##.classList##add(dirty_cls);
    input_dom##.disabled := Js._true;
    Lwt.async begin fun () ->
      match%lwt emit (Js.to_string input_dom##.value) with
      | Ack_ok ->
        on_cancel ev;
        Lwt.return_unit
      | Ack_error msg ->
        input_dom##.classList##add(error_cls);
        input_dom##.title := Js.string msg;
        Lwt.return_unit
    end in

  add_edit_button ();

  let set_value x =
    outer_dom##.classList##remove(dirty_cls);
    pcdata_dom##replaceData 0 pcdata_dom##.length (Js.string x);
    input_dom##.value := Js.string x in
  set_value x;
  set_value

let add_edit_input outer pcdata emit x =
  let inp = D.Raw.input ~a:[F.a_input_type `Text] () in
  add_edit outer pcdata (To_dom.of_input inp) emit x

let add_edit_textarea outer pcdata emit x =
  let inp = D.Raw.textarea (D.pcdata x) in
  add_edit outer pcdata (To_dom.of_textarea inp) emit x

]

[%%shared

let span_with_input ?a (emit : (string -> ack Lwt.t) client_value) x =
  let pcdata = D.pcdata x in
  let outer = D.span ?a [pcdata] in
  let g = [%client add_edit_input ~%outer ~%pcdata ~%emit ~%x] in
  g, outer

let p_with_textarea ?a (emit : (string -> ack Lwt.t) client_value) x =
  let pcdata = D.pcdata x in
  let outer = D.p ?a [pcdata] in
  let g = [%client add_edit_textarea ~%outer ~%pcdata ~%emit ~%x] in
  g, outer

]
