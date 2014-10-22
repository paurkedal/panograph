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
open Consimila_intf
open Unprime
open Unprime_option

module Simple_shape = struct
  type t = {
    input_a : Html5_types.input_attrib Html5.attrib list;
  }
  let make ?(a = []) () = {input_a = a}
end

module type SIMPLE_PATCH_EDITOR = sig
  type value
  include PATCH_EDITOR
     with type value := value
      and type patch_out = [`Set of value]
      and type patch_in = [`Set of value]
      and type shape = Simple_shape.t
      and type ui = Html5_types.flow5 Html5.elt
end

module type SIMPLE_SNAPSHOT_EDITOR =
  SNAPSHOT_EDITOR
    with type shape = Simple_shape.t
     and type ui = Html5_types.flow5 Html5.elt

module Simple_patch_editor (Value : STRINGABLE) = struct
  type shape = Simple_shape.t
  let default_shape = Simple_shape.make ()

  type value = Value.t
  type patch_out = [`Set of value]
  type patch_in = patch_out

  type ui = Html5_types.flow5 Html5.elt
  type t = {
    w_ui : [`Input] Html5.elt;
    w_dom : Dom_html.inputElement Js.t;
    w_saved_title : string;
  }

  let ui w = (w.w_ui :> ui)

  let has_class w cls =
    Js.to_bool (w.w_dom##classList##contains(Js.string cls))

  let set_dirty w =
    if not (has_class w "dirty") then
      w.w_dom##classList##add(Js.string "dirty")

  let clear_dirty w =
    w.w_dom##classList##remove(Js.string "dirty")

  let set_error w msg =
    if not (has_class w "error") then
      w.w_dom##classList##add(Js.string "error");
    w.w_dom##title <- Js.string (msg ^ "\n" ^ w.w_saved_title)

  let clear_error w =
    w.w_dom##classList##remove(Js.string "error");
    w.w_dom##title <- Js.string w.w_saved_title

  let patch w (`Set x) =
    clear_error w;
    w.w_dom##value <- Js.string (Value.to_string x);
    clear_dirty w

  let set {w_dom} x = w_dom##value <- Js.string (Value.to_string x)
  let get {w_dom} = Value.of_string (Js.to_string w_dom##value)

  let create ~init ?on_patch {Simple_shape.input_a} =
    let inp = Html5.D.input ~input_type:`Text ~a:input_a () in
    let w_dom = Html5.To_dom.of_input inp in
    let w = {w_ui = inp; w_dom; w_saved_title = Js.to_string w_dom##title} in
    set w init;
    Option.iter (fun on_patch ->
      let on_change _ _ =
	try
	  set_dirty w;
	  match_lwt on_patch (`Set (get w)) with
	  | Ack_ok -> Lwt.return_unit
	  | Ack_error msg -> set_error w msg; Lwt.return_unit
	with
	| Failure _ | Invalid_argument _ ->
	  clear_dirty w; set_error w "Invalid input."; Lwt.return_unit in
      Lwt_js_events.(async @@ fun () -> changes w.w_dom on_change)) on_patch;
    w
end

module Simple_snapshot_editor (Value : STRINGABLE) = struct
  type shape = Simple_shape.t
  let default_shape = Simple_shape.make ()

  type value = Value.t
  type ui = Html5_types.flow5 Html5.elt
  type t = {
    w_ui : [`Input] Html5.elt;
    w_dom : Dom_html.inputElement Js.t;
    w_saved_title : string;
    mutable w_value : value;
  }
  let ui w = (w.w_ui :> ui)

  let snapshot w = Value.of_string (Js.to_string w.w_dom##value)

  let set_error w msg =
    w.w_dom##classList##add(Js.string "error");
    w.w_dom##title <- Js.string (msg ^ "\n" ^ w.w_saved_title)
  let clear_error w =
    w.w_dom##classList##remove(Js.string "error");
    w.w_dom##title <- Js.string w.w_saved_title

  let create ~init shape =
    let inp =
      Html5.D.input ~input_type:`Text ~a:shape.Simple_shape.input_a () in
    let w_dom = Html5.To_dom.of_input inp in
    let w = {w_ui = inp; w_dom; w_saved_title = Js.to_string w_dom##title;
	     w_value = init} in
    w_dom##value <- Js.string (Value.to_string init);
    let on_change _ _ =
      Lwt.wrap begin fun () ->
	try clear_error w;
	    w.w_value <- Value.of_string (Js.to_string w_dom##value)
	with Failure _ | Invalid_argument _ -> set_error w "Invalid input."
      end in
    Lwt_js_events.(async @@ fun () -> changes w.w_dom on_change);
    w
end

module String_str = struct
  type t = string
  let of_string = ident
  let to_string = ident
end

module Int_str = struct
  type t = int
  let of_string = int_of_string
  let to_string = string_of_int
end

module Float_str = struct
  type t = float
  let of_string = float_of_string
  let to_string = string_of_float
end

module String_PE = Simple_patch_editor (String_str)
module Int_PE = Simple_patch_editor (Int_str)
module Float_PE = Simple_patch_editor (Float_str)

module String_SE = Simple_snapshot_editor (String_str)
module Int_SE = Simple_snapshot_editor (Int_str)
module Float_SE = Simple_snapshot_editor (Float_str)
