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

[%%shared.start]

open Eliom_content
open Eliom_pervasives
open Panograph_i18n
open Panograph_types

type twine_editor_out = [`Add of lang * string | `Remove of lang]
                        [@@deriving json]

type twine_editor_in = twine_editor_out

val twine_editor : ?value: twine ->
                   (twine_editor_out -> ack Lwt.t) client_value ->
                   [> Html5_types.span] Html5.elt *
                   (twine_editor_in -> unit) client_value
