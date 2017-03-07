(* Copyright (C) 2015--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Input boxes with dynamic completions (deprecated). *)

[%%shared.start]

open Eliom_content
open Panograph_types

val string_completion_input :
      ?value: string ->
      (string -> string list ui_result Lwt.t) Eliom_client_value.t ->
      (string -> unit ui_result Lwt.t) Eliom_client_value.t ->
      [> Html_types.span] Html.elt * (string -> unit) Eliom_client_value.t
