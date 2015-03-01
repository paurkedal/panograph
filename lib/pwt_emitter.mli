(* Copyright (C) 2014--2015  Petter Urkedal <paurkedal@gmail.com>
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

(** Simple event mechanism. *)

type 'a t

type 'a tangle = 'a t * ('a -> unit)

val create : unit -> 'a t * ('a -> unit)

val next : 'a t -> 'a Lwt.t

val iter : ('a -> unit) -> 'a t -> unit Lwt.t

val iter_s : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t
