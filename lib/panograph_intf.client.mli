(* Copyright (C) 2014--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Experimental functorial widget construction. *)

open Eliom_content
open Panograph_types

type controls_ui = [`Button | `Img | `Img_interactive | `Span] Html.elt list

module type STRINGABLE = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end

module type WIDGET_BASE = sig
  type shape
  type t
  type ui
  val default_shape : shape
end

module type PATCH_VIEWER = sig
  include WIDGET_BASE

  type value
  type patch_in

  val create : ?shape: shape -> value -> t * ui
  val patch : t -> patch_in -> unit
end

module type SNAPSHOT_VIEWER = sig
  include WIDGET_BASE

  type value

  val create : ?shape: shape -> value -> t * ui
  val set : t -> value -> unit
end

module type PATCH_EDITOR = sig
  include WIDGET_BASE

  type value
  type patch_out
  type patch_in

  val create :
    ?shape: shape -> ?on_patch: (patch_out -> unit Panui_result.t Lwt.t) ->
    value -> t * ui

  val patch : t -> patch_in -> unit
end

module type SNAPSHOT_EDITOR = sig
  include WIDGET_BASE

  type value

  val create : ?shape: shape -> ?init: value -> unit -> t * ui
  val snapshot : t -> value
end

module type RETRACTABLE_PATCH_EDITOR = sig
  include PATCH_EDITOR
  include Prime_retraction.RETRACTABLE with type t := t
  val key_of_t : t -> key
  val key_of_value : value -> key
  val key_of_patch_in : patch_in -> key * key option
  val key_of_patch_out : patch_out -> key * key option
end

module type CONTAINER = sig
  include WIDGET_BASE
  type item
  type item_ui
  type static_ui

  val create : ?shape: shape -> ?static: static_ui -> unit -> t * ui
  val create_item : ?shape: shape -> item_ui -> item
  val append : ?before: item -> t -> item -> unit
  val remove : t -> item -> unit
end
