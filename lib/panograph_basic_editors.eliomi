(* Copyright (C) 2014--2015  Petter A. Urkedal <paurkedal@gmail.com>
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
  class type basicInteractiveElement = object
    inherit Dom_html.element
    method value : Js.js_string Js.t Js.prop
  end

  val outfit_interactive :
	to_string: ('a -> string) ->
	of_string: (string -> 'b) ->
	?value: 'a ->
	basicInteractiveElement Js.t ->
	('b -> ack Lwt.t) -> ('a -> unit)

  val outfit_input :
	to_string: ('a -> string) ->
	of_string: (string -> 'b) ->
	?value: 'a ->
	[< Html5_types.input] Html5.elt ->
	('b -> ack Lwt.t) -> ('a -> unit)

  val outfit_select :
	to_string: ('a -> string) ->
	of_string: (string -> 'b) ->
	?value: 'a ->
	[< Html5_types.select] Html5.elt ->
	('b -> ack Lwt.t) -> ('a -> unit)
}}

{shared{
  val string_editor :
	?a: Html5_types.input_attrib Html5.attrib list ->
	?to_string: (string -> string) client_value ->
	?of_string: (string -> string) client_value ->
	?value: string ->
	(string -> ack Lwt.t) client_value ->
	[> Html5_types.input] Html5.elt * (string -> unit) client_value

  val int_editor :
	?a: Html5_types.input_attrib Html5.attrib list ->
	?to_string: (int -> string) client_value ->
	?of_string: (string -> int) client_value ->
	?value: int ->
	(int -> ack Lwt.t) client_value ->
	[> Html5_types.input] Html5.elt * (int -> unit) client_value

  val int32_editor :
	?a: Html5_types.input_attrib Html5.attrib list ->
	?to_string: (int32 -> string) client_value ->
	?of_string: (string -> int32) client_value ->
	?value: int32 ->
	(int32 -> ack Lwt.t) client_value ->
	[> Html5_types.input] Html5.elt * (int32 -> unit) client_value

  val int64_editor :
	?a: Html5_types.input_attrib Html5.attrib list ->
	?to_string: (int64 -> string) client_value ->
	?of_string: (string -> int64) client_value ->
	?value: int64 ->
	(int64 -> ack Lwt.t) client_value ->
	[> Html5_types.input] Html5.elt * (int64 -> unit) client_value

  val float_editor :
	?a: Html5_types.input_attrib Html5.attrib list ->
	?to_string: (float -> string) client_value ->
	?of_string: (string -> float) client_value ->
	?value: float ->
	(float -> ack Lwt.t) client_value ->
	[> Html5_types.input] Html5.elt * (float -> unit) client_value

  val string_option_editor :
	?a: Html5_types.input_attrib Html5.attrib list ->
	?to_string: (string -> string) client_value ->
	?of_string: (string -> string) client_value ->
	?value: string option ->
	(string option -> ack Lwt.t) client_value ->
	[> Html5_types.input] Html5.elt * (string option -> unit) client_value

  val string_option_textarea :
	?a: Html5_types.textarea_attrib Html5.attrib list ->
	?to_string: (string -> string) client_value ->
	?of_string: (string -> string) client_value ->
	?value: string option ->
	(string option -> ack Lwt.t) client_value ->
	[> Html5_types.textarea] Html5.elt
	  * (string option -> unit) client_value

  val int_option_editor :
	?a: Html5_types.input_attrib Html5.attrib list ->
	?to_string: (int -> string) client_value ->
	?of_string: (string -> int) client_value ->
	?value: int option ->
	(int option -> ack Lwt.t) client_value ->
	[> Html5_types.input] Html5.elt * (int option -> unit) client_value

  val int32_option_editor :
	?a: Html5_types.input_attrib Html5.attrib list ->
	?to_string: (int32 -> string) client_value ->
	?of_string: (string -> int32) client_value ->
	?value: int32 option ->
	(int32 option -> ack Lwt.t) client_value ->
	[> Html5_types.input] Html5.elt * (int32 option -> unit) client_value

  val int64_option_editor :
	?a: Html5_types.input_attrib Html5.attrib list ->
	?to_string: (int64 -> string) client_value ->
	?of_string: (string -> int64) client_value ->
	?value: int64 option ->
	(int64 option -> ack Lwt.t) client_value ->
	[> Html5_types.input] Html5.elt * (int64 option -> unit) client_value

  val float_option_editor :
	?a: Html5_types.input_attrib Html5.attrib list ->
	?to_string: (float -> string) client_value ->
	?of_string: (string -> float) client_value ->
	?value: float option ->
	(float option -> ack Lwt.t) client_value ->
	[> Html5_types.input] Html5.elt * (float option -> unit) client_value

  val bool_option_selector :
	?a: Html5_types.select_attrib Html5.attrib list ->
	?none_label: string ->
	false_label: string ->
	true_label: string ->
	?value: bool option ->
	(bool option -> ack Lwt.t) client_value ->
	[> Html5_types.select] Html5.elt * (bool option -> unit) client_value
  (** @deprecated Use {!Panui_basic_selectors}. *)

  val int_option_selector :
	?a: Html5_types.select_attrib Html5.attrib list ->
	?none_label: string ->
	items: (string option * (int * string * bool) list) list ->
	?value: int option ->
	(int option -> ack Lwt.t) client_value ->
	[> Html5_types.select] Html5.elt * (int option -> unit) client_value
  (** @deprecated Use {!Panui_basic_selectors}. *)

  val int32_option_selector :
	?a: Html5_types.select_attrib Html5.attrib list ->
	?none_label: string ->
	items: (string option * (int32 * string * bool) list) list ->
	?value: int32 option ->
	(int32 option -> ack Lwt.t) client_value ->
	[> Html5_types.select] Html5.elt * (int32 option -> unit) client_value
  (** @deprecated Use {!Panui_basic_selectors}. *)

  val int64_option_selector :
	?a: Html5_types.select_attrib Html5.attrib list ->
	?none_label: string ->
	items: (string option * (int64 * string * bool) list) list ->
	?value: int64 option ->
	(int64 option -> ack Lwt.t) client_value ->
	[> Html5_types.select] Html5.elt * (int64 option -> unit) client_value
  (** @deprecated Use {!Panui_basic_selectors}. *)

  val string_option_menu :
	?a: Html5_types.select_attrib Html5.attrib list ->
	values: string list -> ?value: string option ->
	(string option -> ack Lwt.t) client_value ->
	[> Html5_types.select] Html5.elt * (string option -> unit) client_value
  (** @deprecated Use {!Panui_basic_selectors}. *)

  val bool_checkbox :
	?a: Html5_types.input_attrib Html5.attrib list ->
	?value: bool ->
	(bool -> ack Lwt.t) client_value ->
	[> Html5_types.input] Html5.elt * (bool -> unit) client_value
}}
