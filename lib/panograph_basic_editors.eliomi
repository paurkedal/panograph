(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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
  open Eliom_content
  open Panograph_types
}}

{client{
  val outfit_input_editor :
	to_string: ('a -> string) ->
	of_string: (string -> 'b) ->
	?value: 'a ->
	[< Html5_types.input] Html5.elt ->
	('b -> ack Lwt.t) -> ('a -> unit)
}}

{shared{
  val string_editor :
	?a: Html5_types.input_attrib Html5.attrib list ->
	?value: string ->
	(string -> ack Lwt.t) client_value ->
	[> Html5_types.input] Html5.elt * (string -> unit) client_value

  val int_editor :
	?a: Html5_types.input_attrib Html5.attrib list ->
	?value: int ->
	(int -> ack Lwt.t) client_value ->
	[> Html5_types.input] Html5.elt * (int -> unit) client_value

  val float_editor :
	?a: Html5_types.input_attrib Html5.attrib list ->
	?value: float ->
	(float -> ack Lwt.t) client_value ->
	[> Html5_types.input] Html5.elt * (float -> unit) client_value

  val string_option_editor :
	?a: Html5_types.input_attrib Html5.attrib list ->
	?value: string option ->
	(string option -> ack Lwt.t) client_value ->
	[> Html5_types.input] Html5.elt * (string option -> unit) client_value

  val int_option_editor :
	?a: Html5_types.input_attrib Html5.attrib list ->
	?value: int option ->
	(int option -> ack Lwt.t) client_value ->
	[> Html5_types.input] Html5.elt * (int option -> unit) client_value

  val float_option_editor :
	?a: Html5_types.input_attrib Html5.attrib list ->
	?value: float option ->
	(float option -> ack Lwt.t) client_value ->
	[> Html5_types.input] Html5.elt * (float option -> unit) client_value
}}
