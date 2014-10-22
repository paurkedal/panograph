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

  val create : init: value -> shape -> t
  val snapshot : t -> value
end

module type RETRACTABLE_PATCH_EDITOR = sig
  include PATCH_EDITOR
  include Prime_retraction.RETRACTABLE with type t := t
  val key_of_value : value -> key
  val affects_key : patch_in -> bool
end

module type CONTAINER = sig
  type shape
  type ui
  type elt_ui
  val create : shape -> ui
  val prepend : ui -> elt_ui -> unit
  val insert : ui -> elt_ui -> elt_ui -> unit
  val remove : ui -> elt_ui -> unit
end
