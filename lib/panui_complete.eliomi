(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
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
open Panograph_types

[%%server.start]

type 'a handle

[%%client.start]

class type ['a] handle = object
  method get : 'a
  method set : 'a -> unit
end

[%%shared.start]

type ('a, 'b, 'attrib, 'elt) t =
    ?has_feedback: bool ->
    complete: (string -> 'b ui_result Lwt.t) Eliom_client_value.t ->
    emit: ('a -> unit ui_result Lwt.t) Eliom_client_value.t ->
    ?a: 'attrib attrib list ->
    'a -> 'elt elt * 'a handle Eliom_client_value.t
  constraint 'attrib = [< Html_types.common > `Class]
  constraint 'elt = [> `Span]

type ('a, 'attrib, 'elt) t_req = ('a, 'a list, 'attrib, 'elt) t
type ('a, 'attrib, 'elt) t_opt = ('a option, 'a list, 'attrib, 'elt) t

val string : (string, 'attrib, 'elt) t_req
val string_option : (string, 'attrib, 'elt) t_opt
val labelled_int : (string * int, 'attrib, 'elt) t_req
val labelled_int_option : (string * int, 'attrib, 'elt) t_opt
val labelled_int32 : (string * int32, 'attrib, 'elt) t_req
val labelled_int32_option : (string * int32, 'attrib, 'elt) t_opt
val labelled_int64 : (string * int64, 'attrib, 'elt) t_req
val labelled_int64_option : (string * int64, 'attrib, 'elt) t_opt
