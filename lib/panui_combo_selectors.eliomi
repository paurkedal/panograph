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

[%%shared.start]

open Eliom_content.Html
open Html_types
open Panograph_types
open Panui_content

val int_string_option_combo_selector :
      ?a: [< span_attrib > `Class] attrib list ->
      ?inl_a: select_attrib attrib list ->
      ?inr_a: input_attrib attrib list ->
      inl_selection: int option Selection.t ->
      ?inr_to_string: (string -> string) Eliom_client_value.t ->
      ?inr_of_string: (string -> string) Eliom_client_value.t ->
      ?value: (int, string) either option ->
      ((int, string) either option -> ack Lwt.t) Eliom_client_value.t ->
      [> span] elt * ((int, string) either option -> unit) Eliom_client_value.t

val int32_string_option_combo_selector :
      ?a: [< span_attrib > `Class] attrib list ->
      ?inl_a: select_attrib attrib list ->
      ?inr_a: input_attrib attrib list ->
      inl_selection: int32 option Selection.t ->
      ?inr_to_string: (string -> string) Eliom_client_value.t ->
      ?inr_of_string: (string -> string) Eliom_client_value.t ->
      ?value: (int32, string) either option ->
      ((int32, string) either option -> ack Lwt.t) Eliom_client_value.t ->
      [> span] elt * ((int32, string) either option -> unit) Eliom_client_value.t

val int64_string_option_combo_selector :
      ?a: [< span_attrib > `Class] attrib list ->
      ?inl_a: select_attrib attrib list ->
      ?inr_a: input_attrib attrib list ->
      inl_selection: int64 option Selection.t ->
      ?inr_to_string: (string -> string) Eliom_client_value.t ->
      ?inr_of_string: (string -> string) Eliom_client_value.t ->
      ?value: (int64, string) either option ->
      ((int64, string) either option -> ack Lwt.t) Eliom_client_value.t ->
      [> span] elt * ((int64, string) either option -> unit) Eliom_client_value.t
