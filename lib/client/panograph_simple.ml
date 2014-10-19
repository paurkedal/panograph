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

open Eliom_content
open Panograph_sigs
open Unprime
open Unprime_option

module Simple_shape = struct
  type t = {
    input_a : Html5_types.input_attrib Html5.attrib list;
  }
  let make ?(a = []) () = {input_a = a}
end

module type SIMPLE_EDITOR = sig
  type value
  type patch_up = [`Set of value]
  include FULL_EDITOR
     with type value := value
      and type patch_up := patch_up
      and type patch_down = patch_up
      and type shape = Simple_shape.t
      and type ui = Html5_types.flow5 Html5.elt
end

module Simple_editor (Value : STRINGABLE) = struct
  type shape = Simple_shape.t
  let default_shape = Simple_shape.make ()

  type value = Value.t
  type patch_up = [`Set of value]
  type patch_down = patch_up

  type ui = Html5_types.flow5 Html5.elt
  type vi = {
    vi_dom : Dom_html.inputElement Js.t;
    vi_saved_title : string;
  }

  let has_class vi cls =
    Js.to_bool (vi.vi_dom##classList##contains(Js.string cls))

  let set_dirty vi =
    if not (has_class vi "dirty") then
      vi.vi_dom##classList##add(Js.string "dirty")

  let clear_dirty vi =
    vi.vi_dom##classList##remove(Js.string "dirty")

  let set_error vi msg =
    if not (has_class vi "error") then
      vi.vi_dom##classList##add(Js.string "error");
    vi.vi_dom##title <- Js.string (msg ^ "\n" ^ vi.vi_saved_title)

  let clear_error vi =
    vi.vi_dom##classList##remove(Js.string "error");
    vi.vi_dom##title <- Js.string vi.vi_saved_title

  let patch vi (`Set x) =
    clear_error vi;
    vi.vi_dom##value <- Js.string (Value.to_string x);
    clear_dirty vi

  let set {vi_dom} x = vi_dom##value <- Js.string (Value.to_string x)
  let get {vi_dom} = Value.of_string (Js.to_string vi_dom##value)

  let create ?init ?up {Simple_shape.input_a} =
    let inp = Html5.D.input ~input_type:`Text ~a:input_a () in
    let vi_dom = Html5.To_dom.of_input inp in
    let vi = {vi_dom; vi_saved_title = Js.to_string vi_dom##title} in
    Option.iter (set vi) init;
    Option.iter (fun up ->
      let on_change _ _ =
	try
	  set_dirty vi;
	  match_lwt up (`Set (get vi)) with
	  | Ack_ok -> Lwt.return_unit
	  | Ack_error msg -> set_error vi msg; Lwt.return_unit
	with
	| Failure _ | Invalid_argument _ ->
	  clear_dirty vi; set_error vi "Invalid input."; Lwt.return_unit in
      Lwt_js_events.(async @@ fun () -> changes vi.vi_dom on_change)) up;
    inp, vi
end

module String_editor = Simple_editor
  (struct type t = string
	  let of_string = ident
	  let to_string = ident end)

module Int_editor = Simple_editor
  (struct type t = int
	  let of_string = int_of_string
	  let to_string = string_of_int end)

module Float_editor = Simple_editor
  (struct type t = float
	  let of_string = float_of_string
	  let to_string = string_of_float end)
