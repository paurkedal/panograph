(* Copyright (C) 2014--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

open Eliom_content
open Panograph_types

val string_editor :
      ?a: [< Html_types.input_attrib > `Input_Type] Html.attrib list ->
      ?to_string: (string -> string) Eliom_client_value.t ->
      ?of_string: (string -> string) Eliom_client_value.t ->
      ?value: string ->
      (string -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> Html_types.input] Html.elt * (string -> unit) Eliom_client_value.t
[@@ocaml.deprecated "Use Panui_scalar"]

val int_editor :
      ?a: [< Html_types.input_attrib > `Input_Type] Html.attrib list ->
      ?to_string: (int -> string) Eliom_client_value.t ->
      ?of_string: (string -> int) Eliom_client_value.t ->
      ?value: int ->
      (int -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> Html_types.input] Html.elt * (int -> unit) Eliom_client_value.t
[@@ocaml.deprecated "Use Panui_scalar"]

val int32_editor :
      ?a: [< Html_types.input_attrib > `Input_Type] Html.attrib list ->
      ?to_string: (int32 -> string) Eliom_client_value.t ->
      ?of_string: (string -> int32) Eliom_client_value.t ->
      ?value: int32 ->
      (int32 -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> Html_types.input] Html.elt * (int32 -> unit) Eliom_client_value.t
[@@ocaml.deprecated "Use Panui_scalar"]

val int64_editor :
      ?a: [< Html_types.input_attrib > `Input_Type] Html.attrib list ->
      ?to_string: (int64 -> string) Eliom_client_value.t ->
      ?of_string: (string -> int64) Eliom_client_value.t ->
      ?value: int64 ->
      (int64 -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> Html_types.input] Html.elt * (int64 -> unit) Eliom_client_value.t
[@@ocaml.deprecated "Use Panui_scalar"]

val float_editor :
      ?a: [< Html_types.input_attrib > `Input_Type] Html.attrib list ->
      ?to_string: (float -> string) Eliom_client_value.t ->
      ?of_string: (string -> float) Eliom_client_value.t ->
      ?value: float ->
      (float -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> Html_types.input] Html.elt * (float -> unit) Eliom_client_value.t
[@@ocaml.deprecated "Use Panui_scalar"]

val string_option_editor :
      ?a: [< Html_types.input_attrib > `Input_Type] Html.attrib list ->
      ?to_string: (string -> string) Eliom_client_value.t ->
      ?of_string: (string -> string) Eliom_client_value.t ->
      ?value: string option ->
      (string option -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> Html_types.input] Html.elt * (string option -> unit) Eliom_client_value.t
[@@ocaml.deprecated "Use Panui_scalar"]

val string_option_textarea :
      ?a: Html_types.textarea_attrib Html.attrib list ->
      ?to_string: (string -> string) Eliom_client_value.t ->
      ?of_string: (string -> string) Eliom_client_value.t ->
      ?value: string option ->
      (string option -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> Html_types.textarea] Html.elt
        * (string option -> unit) Eliom_client_value.t

val int_option_editor :
      ?a: [< Html_types.input_attrib > `Input_Type] Html.attrib list ->
      ?to_string: (int -> string) Eliom_client_value.t ->
      ?of_string: (string -> int) Eliom_client_value.t ->
      ?value: int option ->
      (int option -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> Html_types.input] Html.elt * (int option -> unit) Eliom_client_value.t
[@@ocaml.deprecated "Use Panui_scalar"]

val int32_option_editor :
      ?a: [< Html_types.input_attrib > `Input_Type] Html.attrib list ->
      ?to_string: (int32 -> string) Eliom_client_value.t ->
      ?of_string: (string -> int32) Eliom_client_value.t ->
      ?value: int32 option ->
      (int32 option -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> Html_types.input] Html.elt * (int32 option -> unit) Eliom_client_value.t
[@@ocaml.deprecated "Use Panui_scalar"]

val int64_option_editor :
      ?a: [< Html_types.input_attrib > `Input_Type] Html.attrib list ->
      ?to_string: (int64 -> string) Eliom_client_value.t ->
      ?of_string: (string -> int64) Eliom_client_value.t ->
      ?value: int64 option ->
      (int64 option -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> Html_types.input] Html.elt * (int64 option -> unit) Eliom_client_value.t
[@@ocaml.deprecated "Use Panui_scalar"]

val float_option_editor :
      ?a: [< Html_types.input_attrib > `Input_Type] Html.attrib list ->
      ?to_string: (float -> string) Eliom_client_value.t ->
      ?of_string: (string -> float) Eliom_client_value.t ->
      ?value: float option ->
      (float option -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> Html_types.input] Html.elt * (float option -> unit) Eliom_client_value.t
[@@ocaml.deprecated "Use Panui_scalar"]

val bool_option_selector :
      ?a: [< Html_types.select_attrib] Html.attrib list ->
      ?none_label: string ->
      false_label: string ->
      true_label: string ->
      ?value: bool option ->
      (bool option -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> Html_types.select] Html.elt * (bool option -> unit) Eliom_client_value.t
[@@ocaml.deprecated "Use Panui_scalar"]

val int_option_selector :
      ?a: [< Html_types.select_attrib] Html.attrib list ->
      ?none_label: string ->
      items: (string option * (int * string * bool) list) list ->
      ?value: int option ->
      (int option -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> Html_types.select] Html.elt * (int option -> unit) Eliom_client_value.t
[@@ocaml.deprecated "Use Panui_scalar"]

val int32_option_selector :
      ?a: [< Html_types.select_attrib] Html.attrib list ->
      ?none_label: string ->
      items: (string option * (int32 * string * bool) list) list ->
      ?value: int32 option ->
      (int32 option -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> Html_types.select] Html.elt * (int32 option -> unit) Eliom_client_value.t
[@@ocaml.deprecated "Use Panui_scalar"]

val int64_option_selector :
      ?a: [< Html_types.select_attrib] Html.attrib list ->
      ?none_label: string ->
      items: (string option * (int64 * string * bool) list) list ->
      ?value: int64 option ->
      (int64 option -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> Html_types.select] Html.elt * (int64 option -> unit) Eliom_client_value.t
[@@ocaml.deprecated "Use Panui_scalar"]

val string_option_menu :
      ?a: [< Html_types.select_attrib] Html.attrib list ->
      values: string list -> ?value: string option ->
      (string option -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> Html_types.select] Html.elt * (string option -> unit) Eliom_client_value.t
[@@ocaml.deprecated "Use Panui_scalar"]

val bool_checkbox :
      ?a: [< Html_types.input_attrib > `Checked `Input_Type]
            Html.attrib list ->
      ?value: bool ->
      (bool -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      [> Html_types.input] Html.elt * (bool -> unit) Eliom_client_value.t
[@@ocaml.deprecated "Use Panui_scalar.bool"]
