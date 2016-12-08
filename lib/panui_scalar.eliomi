(* Copyright (C) 2015--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

type ('a, +'opt) opt constraint 'opt = [< `Opt | `Optgroup]

[%%server.start]

type 'a handle

[%%client.start]

class type ['a] handle = object
  method show : unit
  method hide : unit
  method edit_on : ('a -> ack Lwt.t) -> unit
  method edit_off : unit
  method get : 'a
  method set : 'a -> unit
end

val add_input_with_handle :
      to_string: ('a -> string) ->
      of_string: (string -> 'a) ->
      ?opts: ('a, 'opt) opt list ->
      ?emit: ('a -> ack Lwt.t) ->
      ?error: (string option -> unit) ->
      'a -> [< `Span | `Div | `Td] elt -> 'a handle

[%%shared.start]

val opt : ?enabled: bool -> string -> 'a -> ('a, [> `Opt]) opt
val optv : ?enabled: bool -> 'a -> ('a, [> `Opt]) opt
val optgroup : ?enabled: bool -> string ->
               ('a, [`Opt]) opt list -> ('a, [> `Optgroup]) opt

type ('a, 'opt, 'attrib, 'elt) t =
      ?to_string: ('a -> string) Eliom_client_value.t ->
      ?of_string: (string -> 'a) Eliom_client_value.t ->
      ?opts: ('a, 'opt) opt list ->
      ?emit: ('a -> ack Lwt.t) Eliom_client_value.t ->
      ?error: (string option -> unit) Eliom_client_value.t ->
      ?a: 'attrib attrib list ->
      'a -> 'elt elt * 'a handle Eliom_client_value.t
    constraint 'attrib = [< Html_types.common > `Class]
    constraint 'opt = [< `Opt | `Optgroup]
    constraint 'elt = [> `Span]

val bool : (bool, 'opt, 'attrib, 'elt) t
val string : (string, 'opt, 'attrib, 'elt) t
val int : (int, 'opt, 'attrib, 'elt) t
val int32 : (int32, 'opt, 'attrib, 'elt) t
val int64 : (int64, 'opt, 'attrib, 'elt) t
val float : (float, 'opt, 'attrib, 'elt) t

val bool_option : (bool option, 'opt, 'attrib, 'elt) t
val string_option : (string option, 'opt, 'attrib, 'elt) t
val int_option : (int option, 'opt, 'attrib, 'elt) t
val int32_option : (int32 option, 'opt, 'attrib, 'elt) t
val int64_option : (int64 option, 'opt, 'attrib, 'elt) t
val float_option : (float option, 'opt, 'attrib, 'elt) t
