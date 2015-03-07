(* Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
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

(** Basic additions to Lwt. *)

open Lwt

val when_s : bool -> (unit -> unit t) -> unit t

val failwith : string -> 'a t
val failwith_f : ('a, unit, string, 'b Lwt.t) format4 -> 'a
val invalid_arg : string -> 'a t
val invalid_arg_f : ('a, unit, string, 'b Lwt.t) format4 -> 'a
