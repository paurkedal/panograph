(* Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
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

{shared{
  open Eliom_content.Html5

  val string_viewer :
	?a: Html5_types.span_attrib attrib list ->
	to_string: (string -> string) client_value ->
	?value: string -> unit ->
	[> Html5_types.span] elt * (string -> unit) client_value

  val int_viewer :
	?a: Html5_types.span_attrib attrib list ->
	to_string: (int -> string) client_value ->
	?value: int -> unit ->
	[> Html5_types.span] elt * (int -> unit) client_value

  val float_viewer :
	?a: Html5_types.span_attrib attrib list ->
	to_string: (float -> string) client_value ->
	?value: float -> unit ->
	[> Html5_types.span] elt * (float -> unit) client_value

  val string_option_viewer :
	?a: Html5_types.span_attrib attrib list ->
	to_string: (string option -> string) client_value ->
	?value: string option -> unit ->
	[> Html5_types.span] elt * (string option -> unit) client_value

  val int_option_viewer :
	?a: Html5_types.span_attrib attrib list ->
	to_string: (int option -> string) client_value ->
	?value: int option -> unit ->
	[> Html5_types.span] elt * (int option -> unit) client_value

  val float_option_viewer :
	?a: Html5_types.span_attrib attrib list ->
	to_string: (float option -> string) client_value ->
	?value: float option -> unit ->
	[> Html5_types.span] elt * (float option -> unit) client_value
}}
