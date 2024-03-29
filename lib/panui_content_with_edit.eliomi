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

(** Text or input with edit, commit, and cancel buttons to toggle. *)

[%%shared.start]

open Eliom_content
open Html_types

val span_with_input :
  ?a: [< span_attrib > `Class] Html.attrib list ->
  (string -> unit Panui_result.t Lwt.t) Eliom_client_value.t -> string ->
  (string -> unit) Eliom_client_value.t * [> span] Html.elt

val p_with_textarea :
  ?a: [< p_attrib > `Class] Html.attrib list ->
  (string -> unit Panui_result.t Lwt.t) Eliom_client_value.t -> string ->
  (string -> unit) Eliom_client_value.t * [> p] Html.elt
