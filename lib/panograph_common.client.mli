(* Copyright (C) 2016--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Experimental functorial widget construction. *)

open Eliom_content.Html
open Panograph_types

exception Invalid_input of string

val make_button :
  (unit -> unit Panui_result.t Lwt.t) ->
  [< Html_types.button_content_fun] elt list ->
  [> `Button] elt

module type BASIC_SHAPE_TYPE = sig
  type shape = {
    a_id: string option;
    a_class: string list;
    a_title: string option;
  }
end

module Basic_shape : sig
  include BASIC_SHAPE_TYPE
  val make_default_shape : string list -> shape
  val attribs_of_shape : shape -> [> `Class | `Id | `Title] attrib list
end
