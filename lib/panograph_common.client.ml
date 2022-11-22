(* Copyright (C) 2014--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

open Eliom_content
open Js_of_ocaml_lwt
open Unprime
open Unprime_list
open Unprime_option

exception Invalid_input of string

let make_button f content =
  let open Html in
  let button = D.Raw.button ~a:[D.a_button_type `Button] content in
  let button_dom = To_dom.of_button button in
  let on_click _ _ =
    match%lwt f () with
    | Ok () -> Lwt.return_unit
    | Error err ->
      Pandom_style.flash_error (Panui_error.message err) button_dom in
  Lwt.async (fun () -> Lwt_js_events.clicks button_dom on_click);
  button

module type BASIC_SHAPE_TYPE = sig
  type shape = {
    a_id: string option;
    a_class: string list;
    a_title: string option;
  }
end

module Basic_shape = struct
  type shape = {
    a_id: string option;
    a_class: string list;
    a_title: string option;
  }
  let make_default_shape default_class = {
    a_id = None;
    a_class = default_class;
    a_title = None;
  }
  let attribs_of_shape s =
    [] |> Option.fold (fun id -> List.cons (Html.F.a_id id)) s.a_id
       |> begin match s.a_class with
          | [] -> ident
          | cls -> List.cons (Html.F.a_class cls)
          end
       |> Option.fold (fun s -> List.cons (Html.F.a_title s)) s.a_title
end
