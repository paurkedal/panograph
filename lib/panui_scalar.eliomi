(* Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
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
  open Panograph_types
}}

{server{
  type 'a handle
}}

{client{
  class type ['a] handle = object
    method show : unit
    method hide : unit
    method edit_on : ('a -> ack Lwt.t) -> unit
    method edit_off : unit
    method get : 'a
    method set : 'a -> unit
  end
}}

{shared{
  type ('a, 'opt) opt constraint 'opt = [< `Opt | `Optgroup]

  val opt : ?enabled: bool -> string -> 'a -> ('a, [> `Opt]) opt
  val optgroup : ?enabled: bool -> string ->
		 ('a, [`Opt]) opt list -> ('a, [> `Optgroup]) opt

  type ('a, 'attrib, 'opt) t =
	?to_string: ('a -> string) client_value ->
	?of_string: (string -> 'a) client_value ->
	?opts: ('a, 'opt) opt list ->
	?emit: ('a -> ack Lwt.t) client_value ->
	?error: (string option -> unit) client_value ->
	?a: 'attrib attrib list ->
	'a -> Html5_types.span elt * 'a handle client_value
      constraint 'attrib = [< Html5_types.common]
      constraint 'opt = [< `Opt | `Optgroup]

  val bool : (bool, 'attrib, 'opt) t
  val string : (string, 'attrib, 'opt) t
  val int : (int, 'attrib, 'opt) t
  val int32 : (int32, 'attrib, 'opt) t
  val int64 : (int64, 'attrib, 'opt) t
  val float : (float, 'attrib, 'opt) t

  val bool_option : (bool option, 'attrib, 'opt) t
  val string_option : (string option, 'attrib, 'opt) t
  val int_option : (int option, 'attrib, 'opt) t
  val int32_option : (int32 option, 'attrib, 'opt) t
  val int64_option : (int64 option, 'attrib, 'opt) t
  val float_option : (float option, 'attrib, 'opt) t
}}
