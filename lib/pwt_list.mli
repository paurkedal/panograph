(* Copyright (C) 2015--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Lwt operations over lists (supplements [Lwt_list]). *)

val fold_s : ('a -> 'b -> 'b Lwt.t) -> 'a list -> 'b -> 'b Lwt.t
val search_s : ('a -> 'b option Lwt.t) -> 'a list -> 'b option Lwt.t
val search_p : ('a -> 'b option Lwt.t) -> 'a list -> 'b option Lwt.t
val flatten_map_s : ('a -> 'b list Lwt.t) -> 'a list -> 'b list Lwt.t
val flatten_map_p : ('a -> 'b list Lwt.t) -> 'a list -> 'b list Lwt.t
