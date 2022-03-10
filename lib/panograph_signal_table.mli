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

(** A weak table of signals. *)

module type S = sig
  type key
  type 'a t
  val create : int -> 'a t
  val signal : 'a t -> key -> 'a -> 'a React.S.t
  val signal_opt : 'a t -> key -> 'a React.S.t option
  val value_opt : 'a t -> key -> 'a option
  val set : 'a t -> key -> ?step: React.Step.t -> 'a -> unit
  val size : 'a t -> int
end

module Make (Key : Hashtbl.HashedType) : S with type key = Key.t
