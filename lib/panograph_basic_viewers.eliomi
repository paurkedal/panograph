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

val string_viewer :
      ?a: [< Html_types.span_attrib] attrib list ->
      ?to_string: (string -> string) Eliom_client_value.t ->
      ?value: string -> unit ->
      [> Html_types.span] elt * (string -> unit) Eliom_client_value.t

val int_viewer :
      ?a: [< Html_types.span_attrib] attrib list ->
      ?to_string: (int -> string) Eliom_client_value.t ->
      ?value: int -> unit ->
      [> Html_types.span] elt * (int -> unit) Eliom_client_value.t

val int32_viewer :
      ?a: [< Html_types.span_attrib] attrib list ->
      ?to_string: (int32 -> string) Eliom_client_value.t ->
      ?value: int32 -> unit ->
      [> Html_types.span] elt * (int32 -> unit) Eliom_client_value.t

val int64_viewer :
      ?a: [< Html_types.span_attrib] attrib list ->
      ?to_string: (int64 -> string) Eliom_client_value.t ->
      ?value: int64 -> unit ->
      [> Html_types.span] elt * (int64 -> unit) Eliom_client_value.t

val float_viewer :
      ?a: [< Html_types.span_attrib] attrib list ->
      ?to_string: (float -> string) Eliom_client_value.t ->
      ?value: float -> unit ->
      [> Html_types.span] elt * (float -> unit) Eliom_client_value.t

val string_option_viewer :
      ?a: [< Html_types.span_attrib] attrib list ->
      ?to_string: (string -> string) Eliom_client_value.t ->
      ?value: string option -> unit ->
      [> Html_types.span] elt * (string option -> unit) Eliom_client_value.t

val int_option_viewer :
      ?a: [< Html_types.span_attrib] attrib list ->
      ?to_string: (int -> string) Eliom_client_value.t ->
      ?value: int option -> unit ->
      [> Html_types.span] elt * (int option -> unit) Eliom_client_value.t

val int32_option_viewer :
      ?a: [< Html_types.span_attrib] attrib list ->
      ?to_string: (int32 -> string) Eliom_client_value.t ->
      ?value: int32 option -> unit ->
      [> Html_types.span] elt * (int32 option -> unit) Eliom_client_value.t

val int64_option_viewer :
      ?a: [< Html_types.span_attrib] attrib list ->
      ?to_string: (int64 -> string) Eliom_client_value.t ->
      ?value: int64 option -> unit ->
      [> Html_types.span] elt * (int64 option -> unit) Eliom_client_value.t

val float_option_viewer :
      ?a: [< Html_types.span_attrib] attrib list ->
      ?to_string: (float -> string) Eliom_client_value.t ->
      ?value: float option -> unit ->
      [> Html_types.span] elt * (float option -> unit) Eliom_client_value.t
