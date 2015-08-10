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
  type ('a, 'item) item constraint 'item = [< `Item | `Group]

  val item : ?enabled: bool -> string -> 'a -> ('a, [> `Item]) item
  val item_group : ?enabled: bool -> string ->
		   ('a, [`Item]) item list -> ('a, [> `Group]) item

(*
  type 'a item
  type 'a item_group

  val item : ?enabled: bool -> string -> 'a -> 'a item
  val item_group : ?enabled: bool -> string -> 'a item list -> 'a item_group
*)

  type ('a, 'attrib, 'item) t =
	?to_string: ('a -> string) client_value ->
	?of_string: (string -> 'a) client_value ->
	?items: ('a, 'item) item list ->
	?emit: ('a -> ack Lwt.t) client_value ->
	?error: (string option -> unit) client_value ->
	?a: 'attrib attrib list ->
	'a -> Html5_types.span elt * 'a handle client_value
      constraint 'attrib = [< Html5_types.common]
      constraint 'item = [< `Item | `Group]

  val bool : (bool, 'attrib, 'item) t
  val string : (string, 'attrib, 'item) t
  val int : (int, 'attrib, 'item) t
  val int32 : (int32, 'attrib, 'item) t
  val int64 : (int64, 'attrib, 'item) t
  val float : (float, 'attrib, 'item) t

  val bool_option : (bool option, 'attrib, 'item) t
  val string_option : (string option, 'attrib, 'item) t
  val int_option : (int option, 'attrib, 'item) t
  val int32_option : (int32 option, 'attrib, 'item) t
  val int64_option : (int64 option, 'attrib, 'item) t
  val float_option : (float option, 'attrib, 'item) t
}}
