(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

type ack =
  | Ack_ok
  | Ack_error of string

module type STRINGABLE = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end

module type WIDGET_BASE = sig
  type shape
  type t
  type ui
  type value
  val ui : t -> ui
end

module type PATCH_VIEWER = sig
  include WIDGET_BASE

  type patch_in

  val create : init: value -> shape -> t
  val patch : t -> patch_in -> unit
end

module type PATCH_EDITOR = sig
  include WIDGET_BASE

  type patch_out
  type patch_in

  val create : init: value -> ?on_patch: (patch_out -> ack Lwt.t) -> shape -> t
  val patch : t -> patch_in -> unit
end

module type SNAPSHOT_EDITOR = sig
  include WIDGET_BASE

  val create : ?init: value -> shape -> t
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
  type shape
  type ui
  type item_ui
  type elt_pe_ui
  type elt_se_ui
  val create : ?add_ui: elt_se_ui -> ?on_add: (unit -> ack Lwt.t)  ->
	       shape -> ui
  val create_item : edit_ui: elt_pe_ui -> ?on_remove: (unit -> ack Lwt.t) ->
		    shape -> item_ui
  val append : ?before: item_ui -> ui -> item_ui -> unit
  val remove : ui -> item_ui -> unit
end
