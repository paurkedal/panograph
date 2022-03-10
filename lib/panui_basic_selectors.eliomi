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

[%%shared.start]

open Eliom_content.Html
open Html_types
open Panograph_types
open Panui_content

val string_option_selector :
      ?a: select_attrib attrib list ->
      selection: string option Selection.t ->
      ?value: string option ->
      (string option -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> select] elt * (string option -> unit) Eliom_client_value.t

val bool_option_selector :
      ?a: select_attrib attrib list ->
      selection: bool option Selection.t ->
      ?value: bool option ->
      (bool option -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> select] elt * (bool option -> unit) Eliom_client_value.t

val int_option_selector :
      ?a: Html_types.select_attrib attrib list ->
      selection: int option Selection.t ->
      ?value: int option ->
      (int option -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> select] elt * (int option -> unit) Eliom_client_value.t

val int32_option_selector :
      ?a: Html_types.select_attrib attrib list ->
      selection: int32 option Selection.t ->
      ?value: int32 option ->
      (int32 option -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> select] elt * (int32 option -> unit) Eliom_client_value.t

val int64_option_selector :
      ?a: Html_types.select_attrib attrib list ->
      selection: int64 option Selection.t ->
      ?value: int64 option ->
      (int64 option -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> select] elt * (int64 option -> unit) Eliom_client_value.t
