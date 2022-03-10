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

open Js_of_ocaml
open Js_of_ocaml_lwt
open Pandom_style
open Panograph_common
open Panograph_types
open Unprime_option

class type basicInteractiveElement = object
  inherit Dom_html.element
  method value : Js.js_string Js.t Js.prop
end

let outfit_interactive ~to_string ~of_string ?error ?value input_dom emit =
  let set_error, clear_error =
    match error with
    | None -> set_error, clear_error
    | Some f -> (fun msg _ -> f (Some msg)), (fun _ -> f None) in
  Lwt_js_events.async begin fun () ->
    Lwt_js_events.changes input_dom @@ fun _ _ ->
    clear_error input_dom;
    set_dirty input_dom;
    match%lwt
      try emit (of_string (Js.to_string input_dom##.value)) with
      | Invalid_input msg -> Lwt.return (Panui_result.error msg)
      | Invalid_argument _ | Failure _ ->
        Lwt.return (Panui_result.error "Invalid input.")
    with
    | Ok () ->
      clear_error input_dom;
      Lwt.return_unit
    | Error err ->
      let msg = Panui_error.message err in
      clear_dirty input_dom;
      set_error msg input_dom;
      Lwt.return_unit
  end;
  let absorb v =
    clear_dirty input_dom;
    clear_error input_dom;
    input_dom##.value := Js.string (to_string v) in
  Option.iter absorb value;
  absorb

let outfit_input ~to_string ~of_string ?error ?value input emit =
  outfit_interactive ~to_string ~of_string ?error ?value
                     (Eliom_content.Html.To_dom.of_input input) emit

let outfit_select ~to_string ~of_string ?error ?value select emit =
  outfit_interactive ~to_string ~of_string ?error ?value
                     (Eliom_content.Html.To_dom.of_select select) emit

let outfit_textarea ~to_string ~of_string ?error ?value textarea emit =
  outfit_interactive ~to_string ~of_string ?error ?value
                     (Eliom_content.Html.To_dom.of_textarea textarea) emit

let outfit_checkbox ?error ?value checkbox emit =
  let set_error, clear_error =
    match error with
    | None -> set_error, clear_error
    | Some f -> (fun msg _ -> f (Some msg)), (fun _ -> f None) in
  let input_dom = Eliom_content.Html.To_dom.of_input checkbox in
  Lwt_js_events.async begin fun () ->
    Lwt_js_events.changes input_dom @@ fun _ _ ->
    clear_error input_dom;
    set_dirty input_dom;
    match%lwt emit (Js.to_bool input_dom##.checked) with
    | Ok () ->
      clear_error input_dom;
      Lwt.return_unit
    | Error err ->
      let msg = Panui_error.message err in
      clear_dirty input_dom;
      set_error msg input_dom;
      Lwt.return_unit
  end;
  let absorb v =
    clear_dirty input_dom;
    clear_error input_dom;
    input_dom##.checked := Js.bool v in
  Option.iter absorb value;
  absorb
