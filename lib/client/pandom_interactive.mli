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

open Panograph_types

class type basicInteractiveElement = object
  inherit Dom_html.element
  method value : Js.js_string Js.t Js.prop
end

val outfit_interactive :
      to_string: ('a -> string) ->
      of_string: (string -> 'b) ->
      ?error: (string option -> unit) ->
      ?value: 'a ->
      #basicInteractiveElement Js.t ->
      ('b -> ack Lwt.t) -> ('a -> unit)

val outfit_input :
      to_string: ('a -> string) ->
      of_string: (string -> 'b) ->
      ?error: (string option -> unit) ->
      ?value: 'a ->
      [< Html5_types.input] Eliom_content.Html5.elt ->
      ('b -> ack Lwt.t) -> ('a -> unit)

val outfit_select :
      to_string: ('a -> string) ->
      of_string: (string -> 'b) ->
      ?error: (string option -> unit) ->
      ?value: 'a ->
      [< Html5_types.select] Eliom_content.Html5.elt ->
      ('b -> ack Lwt.t) -> ('a -> unit)

val outfit_textarea :
      to_string: ('a -> string) ->
      of_string: (string -> 'b) ->
      ?error: (string option -> unit) ->
      ?value: 'a ->
      [< Html5_types.textarea] Eliom_content.Html5.elt ->
      ('b -> ack Lwt.t) -> ('a -> unit)
