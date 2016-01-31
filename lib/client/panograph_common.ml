(* Copyright (C) 2014--2016  Petter A. Urkedal <paurkedal@gmail.com>
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
open Panograph_intf
open Panograph_types
open Unprime
open Unprime_list
open Unprime_option

exception Invalid_input of string

let make_button f content =
  let open Html5 in
  let button = D.Raw.button ~a:[D.a_button_type `Button] content in
  let button_dom = To_dom.of_button button in
  let on_click _ _ =
    match_lwt f () with
    | Ack_ok -> Lwt.return_unit
    | Ack_error msg -> Pandom_style.flash_error msg button_dom in
  Lwt.async (fun () -> Lwt_js_events.clicks button_dom on_click);
  button

module type BASIC_SHAPE_TYPE = sig
  type shape = {
    a_id : string option;
    a_class : string list;
    a_title : string option;
  }
end

module Basic_shape = struct
  type shape = {
    a_id : string option;
    a_class : string list;
    a_title : string option;
  }
  let make_default_shape default_class = {
    a_id = None;
    a_class = default_class;
    a_title = None;
  }
  let attribs_of_shape s =
    [] |> Option.fold (fun id -> List.push (Html5.F.a_id id)) s.a_id
       |> begin match s.a_class with
	  | [] -> ident
	  | cls -> List.push (Html5.F.a_class cls)
	  end
       |> Option.fold (fun s -> List.push (Html5.F.a_title s)) s.a_title
end
