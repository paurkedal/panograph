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

(** Input boxes with dynamic completions. *)

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

type ('a, 'b, 'c, 'attrib, 'elt) t =
    ?has_feedback: bool ->
    complete: (string -> 'b ui_result Lwt.t) Eliom_client_value.t ->
    ?name: 'c Eliom_parameter.param_name ->
    ?emit: ('a -> unit ui_result Lwt.t) Eliom_client_value.t ->
    ?a: 'attrib attrib list ->
    'a -> 'elt elt * 'a handle Eliom_client_value.t
  constraint 'attrib = [< Html_types.common > `Class]
  constraint 'elt = [> `Span]

type ('a, 'attrib, 'elt) simple_req =
  ('a, 'a list, [`One of 'a], 'attrib, 'elt) t

type ('a, 'attrib, 'elt) simple_opt =
  ('a option, 'a list, [`One of 'a], 'attrib, 'elt) t

type ('a, 'attrib, 'elt) labelled_req =
  (string * 'a, (string * 'a) list, [`One of 'a], 'attrib, 'elt) t

type ('a, 'attrib, 'elt) labelled_opt =
  ((string * 'a) option, (string * 'a) list, [`One of 'a], 'attrib, 'elt) t

val string : (string, 'attrib, 'elt) simple_req
val string_option : (string, 'attrib, 'elt) simple_opt
val labelled_int : (int, 'attrib, 'elt) labelled_req
val labelled_int_option : (int, 'attrib, 'elt) labelled_opt
val labelled_int32 : (int32, 'attrib, 'elt) labelled_req
val labelled_int32_option : (int32, 'attrib, 'elt) labelled_opt
val labelled_int64 : (int64, 'attrib, 'elt) labelled_req
val labelled_int64_option : (int64, 'attrib, 'elt) labelled_opt
