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

(** Lwt operations over options. *)

val iter_s : ('a -> unit Lwt.t) -> 'a option -> unit Lwt.t
val fold_s : ('a -> 'b -> 'b Lwt.t) -> 'a option -> 'b -> 'b Lwt.t
val for_all_s : ('a -> bool Lwt.t) -> 'a option -> bool Lwt.t
val exists_s : ('a -> bool Lwt.t) -> 'a option -> bool Lwt.t
val filter_s : ('a -> bool Lwt.t) -> 'a option -> 'a option Lwt.t
val map_s : ('a -> 'b Lwt.t) -> 'a option -> 'b option Lwt.t
val fmap_s : ('a -> 'b option Lwt.t) -> 'a option -> 'b option Lwt.t
(* search_s = fmap_s *)
