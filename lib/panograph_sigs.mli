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

module type PATCH_WIDGET_BASE = sig
  type shape
  val default_shape : shape

  type ui
  type vi

  type value
  type patch_down

  val patch : vi -> patch_down -> unit
end

module type PATCH_VIEWER = sig
  include PATCH_WIDGET_BASE

  val create : ?init: value -> shape -> ui * vi
end

module type FULL_VIEWER = sig
  include PATCH_VIEWER

  val set : vi -> value -> unit
end

module type PATCH_EDITOR = sig
  include PATCH_WIDGET_BASE

  type patch_up

  val create : ?init: value -> ?up: (patch_up -> ack Lwt.t) ->
	       shape -> ui * vi
end

module type FULL_EDITOR = sig
  include PATCH_EDITOR
  val get : vi -> value
  val set : vi -> value -> unit
end
