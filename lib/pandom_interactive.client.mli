(* Copyright (C) 2015--2022  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

(** Helpers for interactive elements (internal). *)

open Js_of_ocaml

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
      ('b -> unit Panui_result.t Lwt.t) -> ('a -> unit)

val outfit_input :
      to_string: ('a -> string) ->
      of_string: (string -> 'b) ->
      ?error: (string option -> unit) ->
      ?value: 'a ->
      [< Html_types.input] Eliom_content.Html.elt ->
      ('b -> unit Panui_result.t Lwt.t) -> ('a -> unit)

val outfit_select :
      to_string: ('a -> string) ->
      of_string: (string -> 'b) ->
      ?error: (string option -> unit) ->
      ?value: 'a ->
      [< Html_types.select] Eliom_content.Html.elt ->
      ('b -> unit Panui_result.t Lwt.t) -> ('a -> unit)

val outfit_textarea :
      to_string: ('a -> string) ->
      of_string: (string -> 'b) ->
      ?error: (string option -> unit) ->
      ?value: 'a ->
      [< Html_types.textarea] Eliom_content.Html.elt ->
      ('b -> unit Panui_result.t Lwt.t) -> ('a -> unit)

val outfit_checkbox :
      ?error: (string option -> unit) ->
      ?value: bool ->
      [< Html_types.input] Eliom_content.Html.elt ->
      (bool -> unit Panui_result.t Lwt.t) -> (bool -> unit)
